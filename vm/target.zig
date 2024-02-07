const std = @import("std");
const Allocator = std.mem.Allocator;
const in_debug = @import("builtin").mode == .Debug;
const ops = @import("ops.zig");
const Width = ops.Width;
const Op = ops.Op;
const ByteOp = ops.ByteOp;
const Env = @import("Env.zig");

/// addressable spaces within vm translation units and executable modules
pub const Segment = enum { code, data, bss };

/// a position within loaded memory during execution
pub const Location = struct {
    segment: Segment,
    offset: u32,

    pub fn format(
        self: Location,
        comptime _: []const u8,
        _: std.fmt.FormatOptions,
        writer: anytype,
    ) @TypeOf(writer).Error!void {
        try writer.print("@{s}+{d}", .{ @tagName(self.segment), self.offset });
    }
};

/// visibility for symbols
pub const Visibility = enum {
    /// symbol is only visible within this unit
    private,
    /// symbol is visible between units
    public,
    /// symbol is visible between units and by external users
    exported,
};

/// a named label within a translation unit
const Symbol = struct {
    name: []const u8,
    vis: Visibility,
    loc: Location,
};

/// linkable bytecode translation unit (kind of like an object or a library)
pub const Unit = struct {
    const Import = struct {
        name: []const u8,
        /// write the u32 offset of the label to these locations to link
        refs: []const Location,
    };

    const Relocation = struct {
        /// the relative location of something which will need to be linked
        loc: Location,
        /// write the u32 offset of the label to these locations to link
        refs: []const Location,
    };

    /// symbols which must be resolved before execution
    imports: []const Import,
    /// symbols exported by this unit for linking to other units
    exports: []const Symbol,
    /// list of locations that need to be linked internally once segment offsets
    /// are known
    relocations: []const Relocation,

    /// unlinked bytecode
    code: []const u8,
    /// initialized globally accessible memory
    data: []const u8,
    /// reserved, uninitialized, globally accessible memory
    bss: u32,

    pub fn deinit(self: Unit, ally: Allocator) void {
        for (self.imports) |import| ally.free(import.refs);
        ally.free(self.imports);
        for (self.relocations) |relocation| ally.free(relocation.refs);
        ally.free(self.relocations);
        ally.free(self.exports);
        ally.free(self.code);
        ally.free(self.data);
    }
};

/// the fully linked, executable bytecode format
pub const Module = struct {
    const Export = struct {
        name: []const u8,
        loc: Location,
    };

    /// binary-search-able list of exports
    exports: []const Export,
    code: []const u8,
    data: []const u8,
    bss: u32,

    pub fn deinit(mod: Module, ally: Allocator) void {
        ally.free(mod.exports);
        ally.free(mod.code);
        ally.free(mod.data);
    }
};

// =============================================================================

/// builder builds a translation unit
pub const Builder = struct {
    const Self = @This();

    /// refers to a unique location, which may be currently unknown
    pub const Label = struct {
        index: usize,

        pub fn eql(a: Label, b: Label) bool {
            return a.index == b.index;
        }

        pub fn format(
            self: Label,
            comptime _: []const u8,
            _: std.fmt.FormatOptions,
            writer: anytype,
        ) @TypeOf(writer).Error!void {
            try writer.print(":{d}", .{self.index});
        }
    };

    const Backrefs = std.ArrayListUnmanaged(Location);

    ally: Allocator,

    code: std.ArrayListUnmanaged(u8) = .{},
    data_seg: std.ArrayListUnmanaged(u8) = .{},
    bss_size: u32 = 0,

    /// stores labels as they get resolved
    labels: std.ArrayListUnmanaged(?Location) = .{},
    /// resolved symbols originating from this unit
    exports: std.StringHashMapUnmanaged(Visibility) = .{},
    /// all symbols. may be imported or refer to an exported name
    symbols: std.StringHashMapUnmanaged(Label) = .{},
    /// maps backref label -> locations to be overwritten. becomes relocations
    /// when built.
    backrefs: std.AutoHashMapUnmanaged(Label, Backrefs) = .{},

    pub fn init(ally: Allocator) Self {
        return Self{ .ally = ally };
    }

    pub fn deinit(self: *Self) void {
        const ally = self.ally;

        self.code.deinit(ally);
        self.data_seg.deinit(ally);
        self.labels.deinit(ally);
        self.exports.deinit(ally);
        self.symbols.deinit(ally);

        var br_iter = self.backrefs.valueIterator();
        while (br_iter.next()) |br_list| br_list.deinit(ally);
        self.backrefs.deinit(ally);
    }

    /// create a translation unit with the builder's data, owned by the
    /// allocator provided
    pub fn build(self: Self, ally: Allocator) Allocator.Error!Unit {
        // collect backrefs as relocations
        var relocs = std.AutoHashMap(Location, Backrefs).init(ally);
        defer {
            var values = relocs.valueIterator();
            while (values.next()) |refs| refs.deinit(ally);
            relocs.deinit();
        }

        var backrefs = self.backrefs.iterator();
        while (backrefs.next()) |entry| {
            const lbl = entry.key_ptr.*;
            const list = entry.value_ptr;

            const loc = self.labels.items[lbl.index] orelse {
                // this must be an imported symbol, otherwise there has been
                // a mistake in code generation
                if (in_debug) check: {
                    var imports = self.symbols.valueIterator();
                    while (imports.next()) |imported| {
                        if (imported.eql(lbl)) {
                            break :check;
                        }
                    }

                    std.debug.panic("unresolved label: {}", .{lbl});
                }

                continue;
            };

            const res = try relocs.getOrPut(loc);
            if (!res.found_existing) res.value_ptr.* = .{};
            try res.value_ptr.appendSlice(ally, list.items);
        }

        // collect imports, making them relocations if possible
        var imports = std.ArrayList(Unit.Import).init(ally);
        defer imports.deinit();

        var symbols = self.symbols.iterator();
        while (symbols.next()) |entry| {
            const name = entry.key_ptr.*;
            const lbl = entry.value_ptr.*;

            // get refs for this import
            const refs = self.backrefs.get(lbl) orelse {
                continue;
            };

            // if this is already resolved, it can just be a relocation
            if (self.labels.items[lbl.index]) |loc| {
                const res = try relocs.getOrPut(loc);
                if (!res.found_existing) res.value_ptr.* = .{};
                try res.value_ptr.appendSlice(ally, refs.items);

                continue;
            }

            // add it to the import list
            try imports.append(Unit.Import{
                .name = name,
                .refs = try ally.dupe(Location, refs.items),
            });
        }

        // collect exports
        var exports = std.ArrayList(Symbol).init(ally);
        defer exports.deinit();

        var entries = self.exports.iterator();
        while (entries.next()) |entry| {
            // private symbols should already be resolved as relocations, if
            // they are used at all
            const vis = entry.value_ptr.*;
            if (vis == .private) continue;

            // module-and-exported-level symbols must be exported
            const name = entry.key_ptr.*;
            const lbl = self.symbols.get(name).?;
            const loc = self.labels.items[lbl.index].?;

            try exports.append(Symbol{
                .name = name,
                .vis = vis,
                .loc = loc,
            });
        }

        // transform relocations into the shape unit wants
        var reloc_list = std.ArrayList(Unit.Relocation).init(ally);
        defer reloc_list.deinit();

        var reloc_iter = relocs.iterator();
        while (reloc_iter.next()) |entry| {
            if (entry.value_ptr.items.len == 0) continue;

            try reloc_list.append(Unit.Relocation{ .loc = entry.key_ptr.*, .refs = try entry.value_ptr.toOwnedSlice(ally) });
        }

        return Unit{
            .imports = try imports.toOwnedSlice(),
            .exports = try exports.toOwnedSlice(),
            .relocations = try reloc_list.toOwnedSlice(),
            .code = try ally.dupe(u8, self.code.items),
            .data = try ally.dupe(u8, self.data_seg.items),
            .bss = self.bss_size,
        };
    }

    pub const SymbolError = Allocator.Error || error{
        SymbolAlreadyDefined,
    };

    /// resolve a symbol at a current location.
    ///
    /// instead of using backrefs for this, you can just use symbol() which
    /// provides a unique label for each symbol for the entire module. you can
    /// also use symbol to retrieve a name if you need it.
    ///
    /// *name must outlive builder + unit*
    pub fn define(
        self: *Self,
        name: []const u8,
        vis: Visibility,
        segment: Segment,
    ) SymbolError!void {
        if (self.exports.contains(name)) {
            return SymbolError.SymbolAlreadyDefined;
        }

        const lbl = try self.symbol(name);
        try self.exports.put(self.ally, name, vis);
        self.resolveInSegment(lbl, segment);
    }

    /// get or create a label for a name
    ///
    /// *name must outlive builder + unit*
    pub fn symbol(self: *Self, name: []const u8) Allocator.Error!Label {
        const ally = self.ally;

        // create a new backref
        const res = try self.symbols.getOrPut(ally, name);
        if (!res.found_existing) {
            res.value_ptr.* = try self.backref();
        }

        return res.value_ptr.*;
    }

    /// create and resolve a label
    fn here(self: *Self, segment: Segment) Allocator.Error!Label {
        const lbl = try self.backref();
        self.resolveInSegment(lbl, segment);
        return lbl;
    }

    /// create an unresolved (future) label
    pub fn backref(self: *Self) Allocator.Error!Label {
        const ally = self.ally;

        const lbl = Label{ .index = self.labels.items.len };
        try self.labels.append(ally, null);
        try self.backrefs.put(ally, lbl, .{});

        return lbl;
    }

    /// resolve a backref with the current offset of a segment
    pub fn resolveInSegment(self: *Self, lbl: Label, segment: Segment) void {
        const offset: u32 = switch (segment) {
            .code => @intCast(self.code.items.len),
            .data => @intCast(self.data_seg.items.len),
            .bss => self.bss_size,
        };

        const loc = Location{
            .segment = segment,
            .offset = offset,
        };

        // don't resolve twice
        std.debug.assert(self.labels.items[lbl.index] == null);
        self.labels.items[lbl.index] = loc;
    }

    /// resolve a code backref
    pub fn resolve(self: *Self, lbl: Label) void {
        self.resolveInSegment(lbl, .code);
    }

    /// create and resolve a code label
    pub fn label(self: *Self) Allocator.Error!Label {
        return try self.here(.code);
    }

    /// add data to globally loaded data
    pub fn data(self: *Self, bytes: []const u8) Allocator.Error!Label {
        const lbl = self.here(.data);

        try self.data_seg.appendSlice(self.ally, bytes);

        // ensure data remains aligned to 8 bytes
        const len = self.data_seg.items.len;
        const extra = std.mem.alignForward(usize, len, 8) - len;
        if (extra > 0) {
            try self.data_seg.appendNTimes(self.ally, undefined, extra);
        }

        return lbl;
    }

    /// reserve uninitialized global memory
    pub fn bss(self: *Self, size: usize) Allocator.Error!Label {
        const lbl = self.here(.bss);
        self.bss_size += @intCast(size);

        return lbl;
    }

    /// compile an op to bytecode and add it to the builder's code
    pub fn op(self: *Self, o: Op) Allocator.Error!void {
        const ally = self.ally;

        // construct byteop
        const width: ?Width = switch (o) {
            inline else => |meta| switch (@TypeOf(meta)) {
                Width, Op.Constant => meta,
                Op.Address, Op.CondJmp => meta.width,
                else => null,
            },
        };

        const byteop = ByteOp{
            .width = width orelse .byte,
            .opcode = o,
        };

        try self.code.append(ally, @bitCast(byteop));

        // add extra bytes
        var extra = std.BoundedArray(u8, 8){};
        switch (o) {
            .ret => |num_params| {
                extra.appendAssumeCapacity(num_params);
            },
            .enter => |stack_size| {
                extra.appendSliceAssumeCapacity(std.mem.asBytes(&stack_size));
            },
            .constant => |c| switch (c) {
                inline else => |bytes| {
                    extra.appendSliceAssumeCapacity(&bytes);
                },
            },
            .local => |offset| {
                extra.appendSliceAssumeCapacity(std.mem.asBytes(&offset));
            },
            .load, .store => |addr| {
                extra.appendSliceAssumeCapacity(std.mem.asBytes(&addr.offset));
            },
            .zero, .copy => |length| {
                extra.appendSliceAssumeCapacity(std.mem.asBytes(&length));
            },
            inline .label, .data, .bss, .jump, .jz, .jnz => |meta, tag| {
                const lbl: Label = switch (comptime tag) {
                    .label, .data, .bss, .jump => meta,
                    .jz, .jnz => meta.dest,
                    else => unreachable,
                };

                // create backref
                const loc = Location{
                    .segment = .code,
                    .offset = @intCast(self.code.items.len),
                };

                const backrefs = self.backrefs.getPtr(lbl).?;
                try backrefs.append(ally, loc);

                extra.appendSliceAssumeCapacity(&.{ 0xDE, 0xAD, 0xBE, 0xEF });
            },

            else => {},
        }

        std.debug.assert(extra.len == byteop.extraBytes());
        try self.code.appendSlice(ally, extra.slice());
    }

    /// sugar for pushing a function parameter to the stack
    pub fn param(self: *Self, index: usize) Allocator.Error!void {
        const frame_size: i16 = @intCast(Env.Frame.aligned_size);
        const param_offset: i16 = 8 * (1 + @as(i16, @intCast(index)));
        const local_offset = -(frame_size + param_offset);
        try self.op(.{ .local = local_offset });
    }

    /// sugar for the constant op
    pub fn constant(
        self: *Self,
        comptime T: type,
        value: T,
    ) Allocator.Error!void {
        const width = comptime Width.fromBytesFit(@sizeOf(T)) orelse {
            @compileError(@typeName(T) ++ " is too large for constant");
        };

        var meta = @unionInit(Op.Constant, @tagName(width), undefined);
        const slice: []u8 = switch (meta) {
            inline else => |*arr| arr,
        };
        const bytes = std.mem.asBytes(&value);
        @memcpy(slice, bytes);

        try self.op(.{ .constant = meta });
    }
};

pub const LinkError = Allocator.Error || error{
    ExportAlreadyDefined,
    SymbolNotFound,
};

/// get the absolute location of a relative location using its segment and
/// the current offsets
fn absLoc(
    code_offset: u32,
    data_offset: u32,
    bss_offset: u32,
    loc: Location,
) Location {
    const offset = switch (loc.segment) {
        .code => code_offset,
        .data => data_offset,
        .bss => bss_offset,
    };
    return Location{
        .segment = loc.segment,
        .offset = offset + loc.offset,
    };
}

/// write a location to another location in code or data
fn stitch(
    src: Location,
    dst: Location,
    code: []u8,
    data: []u8,
) void {
    const src_offset: u32 = @intCast(src.offset);
    const src_offset_bytes = std.mem.asBytes(&src_offset);

    const slice = switch (dst.segment) {
        .code => code,
        .data => data,
        .bss => unreachable,
    };
    const dst_bytes = slice[dst.offset .. dst.offset + @sizeOf(u32)];
    @memcpy(dst_bytes, src_offset_bytes);
}

/// link multiple units into a single unit
pub fn link(ally: Allocator, units: []const Unit) LinkError!Module {
    // collect public symbols and determine their absolute locations
    var symbols = std.StringHashMap(Symbol).init(ally);
    defer symbols.deinit();

    var symbol_code_offset: u32 = 0;
    var symbol_data_offset: u32 = 0;
    var symbol_bss_offset: u32 = 0;

    for (units) |unit| {
        for (unit.exports) |x| {
            const res = try symbols.getOrPut(x.name);
            if (res.found_existing) {
                return LinkError.ExportAlreadyDefined;
            }

            const segment_offset = switch (x.loc.segment) {
                .code => symbol_code_offset,
                .data => symbol_data_offset,
                .bss => symbol_bss_offset,
            };
            const abs_loc = Location{
                .segment = x.loc.segment,
                .offset = segment_offset + x.loc.offset,
            };

            res.value_ptr.* = Symbol{
                .name = x.name,
                .vis = x.vis,
                .loc = abs_loc,
            };
        }

        symbol_code_offset += @intCast(unit.code.len);
        symbol_data_offset += @intCast(unit.data.len);
        symbol_bss_offset += unit.bss;
    }

    // stitch together unit data and write module location to unit relocations
    var code = std.ArrayList(u8).init(ally);
    defer code.deinit();
    var data = std.ArrayList(u8).init(ally);
    defer data.deinit();
    var bss: u32 = 0;

    for (units) |unit| {
        const code_offset: u32 = @intCast(code.items.len);
        const data_offset: u32 = @intCast(data.items.len);
        const bss_offset = bss;

        try code.appendSlice(unit.code);
        try data.appendSlice(unit.data);
        bss += unit.bss;

        for (unit.relocations) |reloc| {
            for (reloc.refs) |ref| {
                const src = absLoc(code_offset, data_offset, bss_offset, reloc.loc);
                const dst = absLoc(code_offset, data_offset, bss_offset, ref);
                stitch(src, dst, code.items, data.items);
            }
        }

        for (unit.imports) |import| {
            const meta = symbols.get(import.name) orelse {
                return LinkError.SymbolNotFound;
            };
            const loc = meta.loc;

            for (import.refs) |ref| {
                const dst = absLoc(code_offset, data_offset, bss_offset, ref);
                stitch(loc, dst, code.items, data.items);
            }
        }
    }

    // collect exported symbols in a binary-search-able slice
    var exports = std.ArrayList(Module.Export).init(ally);
    defer exports.deinit();

    var public_symbols = symbols.valueIterator();
    while (public_symbols.next()) |symbol| {
        if (symbol.vis == .exported) {
            try exports.append(Module.Export{
                .name = symbol.name,
                .loc = symbol.loc,
            });
        }
    }

    return Module{
        .exports = try exports.toOwnedSlice(),
        .code = try code.toOwnedSlice(),
        .data = try data.toOwnedSlice(),
        .bss = bss,
    };
}
