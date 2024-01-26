const std = @import("std");
const Allocator = std.mem.Allocator;
const in_debug = @import("builtin").mode == .Debug;
const ops = @import("ops.zig");
const Width = ops.Width;
const Op = ops.Op;
const ByteOp = ops.ByteOp;

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

/// a linkable and/or executable bytecode translation unit
pub const Unit = struct {
    const Import = struct {
        name: []const u8,
        /// write the u32 offset of the label to these locations to link
        backrefs: []const Location,
    };

    /// symbols which must be resolved before execution
    imports: []const Import,
    /// symbols made available by this module for other modules
    /// TODO use some kind of immutable hashmap for this
    exports: []const Symbol,
    /// bytecode
    code: []const u8,
    /// initialized globally accessible memory
    data: []const u8,
    /// reserved, uninitialized, globally accessible memory
    bss: u32,

    pub fn deinit(self: Unit, ally: Allocator) void {
        for (self.imports) |import| ally.free(import.backrefs);
        ally.free(self.imports);
        ally.free(self.exports);
        ally.free(self.code);
        ally.free(self.data);
    }

    /// is this unit ready to execute
    pub fn isResolved(unit: Unit) bool {
        return unit.imports.len == 0;
    }

    /// get the location of an export
    pub fn get(unit: Unit, name: []const u8) ?Location {
        return for (unit.exports) |sym| {
            if (std.mem.eql(u8, sym.name, name)) {
                break sym.loc;
            }
        } else null;
    }
};

/// builder builds a translation unit
pub const Builder = struct {
    const Self = @This();

    /// refers to a unique location, which may be currently unknown
    pub const Label = struct {
        index: usize,

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
    /// all symbols, resolved or unresolved. may be imported or refer to an
    /// exported name.
    symbols: std.StringHashMapUnmanaged(Label) = .{},
    /// maps backref label -> locations to be overwritten
    /// unresolved labels internal to the unit. all must be resolved before
    /// building final unit.
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
        self.link();

        var imports = std.ArrayList(Unit.Import).init(ally);
        defer imports.deinit();

        if (self.symbols.count() > self.exports.count()) {
            @panic("TODO collect imports");
        }

        var exports = std.ArrayList(Symbol).init(ally);
        defer exports.deinit();

        var entries = self.exports.iterator();
        while (entries.next()) |entry| {
            const name = entry.key_ptr.*;
            const vis = entry.value_ptr.*;
            const lbl = self.symbols.get(name).?;
            const loc = self.labels.items[lbl.index].?;

            try exports.append(Symbol{
                .name = name,
                .vis = vis,
                .loc = loc,
            });
        }

        return Unit{
            .imports = try imports.toOwnedSlice(),
            .exports = try exports.toOwnedSlice(),
            .code = try ally.dupe(u8, self.code.items),
            .data = try ally.dupe(u8, self.data_seg.items),
            .bss = self.bss_size,
        };
    }

    /// write a u32 to a location
    fn writeToLoc(self: Self, value: u32, dest: Location) void {
        const bytes: []u8 = switch (dest.segment) {
            .data => self.data_seg.items,
            .code => self.code.items,
            .bss => unreachable,
        };

        const slice = bytes[dest.offset .. dest.offset + @sizeOf(u32)];
        @memcpy(slice, std.mem.asBytes(&value));
    }

    /// write all resolved labels to their backref locations
    fn link(self: Self) void {
        for (self.labels.items, 0..) |maybe_loc, i| {
            const lbl = Label{ .index = i };
            const loc = maybe_loc orelse {
                if (!in_debug) unreachable;
                std.debug.panic("unresolved label: {}", .{lbl});
            };

            const backrefs = self.backrefs.get(lbl) orelse return;
            for (backrefs.items) |br| {
                self.writeToLoc(loc.offset, br);
            }
        }
    }

    pub const SymbolError = Allocator.Error || error{
        SymbolAlreadyDefined,
    };

    /// define a future named export
    ///
    /// *name must outlive builder + unit*
    pub fn defineBackref(
        self: *Self,
        name: []const u8,
        vis: Visibility,
    ) SymbolError!Label {
        if (self.exports.contains(name)) {
            return SymbolError.SymbolAlreadyDefined;
        }

        try self.exports.put(self.ally, name, vis);
        return try self.symbol(name);
    }

    /// define and resolve a named export at a current location
    ///
    /// *name must outlive builder + unit*
    pub fn define(
        self: *Self,
        name: []const u8,
        vis: Visibility,
        segment: Segment,
    ) SymbolError!Label {
        const lbl = try self.defineBackref(name, vis);
        self.resolveInSegment(lbl, segment);
        return lbl;
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
    /// TODO make this return a data label
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

/// link multiple units into a single unit
pub fn link(ally: Allocator, units: []const Unit) Allocator.Error!Unit {
    std.debug.assert(units.len == 1);

    // TODO actually link

    const unit = units[0];
    std.debug.assert(unit.imports.len == 0);

    return Unit{
        .imports = &.{},
        .exports = try ally.dupe(Symbol, unit.exports),
        .data = try ally.dupe(u8, unit.data),
        .code = try ally.dupe(u8, unit.code),
        .bss = unit.bss,
    };
}
