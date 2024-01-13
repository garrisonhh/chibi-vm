const std = @import("std");
const Allocator = std.mem.Allocator;
const in_debug = @import("builtin").mode == .Debug;
const ops = @import("ops.zig");
const Op = ops.Op;
const ByteOp = ops.ByteOp;

const Export = struct {
    name: []const u8,
    /// address of label
    offset: u32,
};

/// a bytecode translation unit
pub const Object = struct {
    code: []const u8,
    exports: []const Export,

    pub fn deinit(self: Object, ally: Allocator) void {
        ally.free(self.code);
        ally.free(self.exports);
    }
};

/// fully linked and executable bytecode
pub const Module = struct {
    pub const Exports = std.StringHashMapUnmanaged(u32);

    code: []const u8,
    /// maps label -> offset
    exports: Exports,

    // TODO I hate that this has to be mutable
    pub fn deinit(self: *Module, ally: Allocator) void {
        ally.free(self.code);

        var keys = self.exports.keyIterator();
        while (keys.next()) |key| ally.free(key.*);
        self.exports.deinit(ally);
    }
};

/// builder builds objects
pub const Builder = struct {
    const Self = @This();

    const Global = struct {
        const Namespace = enum { module, global };

        ns: Namespace,
        label: Label,
    };

    pub const Label = struct {
        index: usize,

        pub fn format(
            self: Label,
            comptime _: []const u8,
            _: std.fmt.FormatOptions,
            writer: anytype,
        ) @TypeOf(writer).Error!void {
            try writer.print("@{d}", .{self.index});
        }
    };

    const Backrefs = std.ArrayListUnmanaged(usize);

    ally: Allocator,
    code: std.ArrayListUnmanaged(u8) = .{},
    globals: std.StringHashMapUnmanaged(Global) = .{},

    /// maps label (as index) -> code index
    labels: std.ArrayListUnmanaged(?u32) = .{},
    /// maps label -> indices of u32s to write
    backrefs: std.AutoHashMapUnmanaged(Label, Backrefs) = .{},

    pub fn init(ally: Allocator) Self {
        return Self{ .ally = ally };
    }

    pub fn deinit(self: *Self) void {
        var ally = self.ally;

        self.code.deinit(ally);
        self.globals.deinit(ally);

        self.labels.deinit(ally);
        var backref_lists = self.backrefs.valueIterator();
        while (backref_lists.next()) |list| {
            list.deinit(ally);
        }
    }

    pub fn build(self: *Self) Allocator.Error!Object {
        const ally = self.ally;

        self.resolveBackrefs();

        var exports = std.ArrayListUnmanaged(Export){};
        defer exports.deinit(ally);

        var globals = self.globals.iterator();
        while (globals.next()) |entry| {
            const name = entry.key_ptr.*;
            const meta = entry.value_ptr.*;

            // don't export module-level names
            if (meta.ns == .module) {
                continue;
            }

            // guaranteed valid by `resolveBackrefs()`
            const loc = self.labels.items[meta.label.index].?;
            try exports.append(ally, .{
                .name = name,
                .offset = loc,
            });
        }

        return Object{
            .code = try self.code.toOwnedSlice(ally),
            .exports = try exports.toOwnedSlice(ally),
        };
    }

    fn resolveBackrefs(self: *const Self) void {
        var backrefs = self.backrefs.iterator();
        while (backrefs.next()) |entry| {
            const lbl = entry.key_ptr.*;
            const backref_list = entry.value_ptr.items;

            const dest = self.labels.items[lbl.index] orelse {
                if (in_debug) {
                    std.debug.panic("unresolved label: {}", .{lbl});
                } else {
                    unreachable;
                }
            };
            const dest_bytes = std.mem.asBytes(&dest);

            for (backref_list) |offset| {
                const reserved = self.code.items[offset .. offset + @sizeOf(u32)];
                @memcpy(reserved, dest_bytes);
            }
        }
    }

    /// add an name to a label
    /// *name must outlive builder + object*
    pub fn global(
        self: *Self,
        name: []const u8,
        ns: Global.Namespace,
        lbl: Label,
    ) Allocator.Error!void {
        std.debug.assert(!self.globals.contains(name));
        try self.globals.put(self.ally, name, Global{
            .ns = ns,
            .label = lbl,
        });
    }

    pub fn getGlobal(self: *const Self, name: []const u8) ?Label {
        if (self.globals.get(name)) |meta| {
            return meta.label;
        }

        return null;
    }

    /// create an unresolved (future) label
    pub fn backref(self: *Self) Allocator.Error!Label {
        const ally = self.ally;

        const lbl = Label{ .index = self.labels.items.len };
        try self.labels.append(ally, null);
        try self.backrefs.put(ally, lbl, .{});

        return lbl;
    }

    /// resolve a backref
    pub fn resolve(self: *Self, lbl: Label) Allocator.Error!void {
        if (in_debug and self.labels.items[lbl.index] != null) {
            std.debug.panic("tried to resolve lbl twice: {}", .{lbl});
        }

        self.labels.items[lbl.index] = @intCast(self.code.items.len);
    }

    /// create and resolve a label
    pub fn label(self: *Self) Allocator.Error!Label {
        const lbl = try self.backref();
        try self.resolve(lbl);
        return lbl;
    }

    /// compile an op and add it to the
    pub fn op(self: *Self, o: Op) Allocator.Error!void {
        const ally = self.ally;

        // construct byteop
        const width: ?ops.Width = switch (o) {
            .halt,
            .label,
            .enter,
            .ret,
            .drop,
            .local,
            .zero,
            .copy,
            .jump,
            .call,
            => null,

            .add,
            .sub,
            .mulu,
            .muli,
            .divu,
            .divi,
            .mod,
            .@"and",
            .@"or",
            .not,
            .bitand,
            .bitor,
            .bitcom,
            .bitxor,
            .eq,
            .ne,
            .neg,
            .sign_extend,
            .sign_narrow,
            => |w| w,

            .constant => |c| c,
            .load, .store => |addr| addr.width,
            .jz, .jnz => |cj| cj.width,
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
            .constant => |data| switch (data) {
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
            inline .label, .jump, .jz, .jnz => |data, tag| {
                const lbl: Label = switch (comptime tag) {
                    .label, .jump => data,
                    .jz, .jnz => data.dest,
                    else => unreachable,
                };

                // label
                const backref_list = self.backrefs.getPtr(lbl).?;
                try backref_list.append(ally, self.code.items.len);

                extra.appendSliceAssumeCapacity(&.{ 0xDE, 0xAD, 0xBE, 0xEF });
            },

            else => {},
        }

        std.debug.assert(extra.len == byteop.extraBytes());
        try self.code.appendSlice(ally, extra.slice());
    }
};

/// links objects into executable form
pub fn link(
    ally: Allocator,
    objects: []const Object,
) Allocator.Error!Module {
    std.debug.assert(objects.len == 1);

    // TODO actually link multiple objects
    // TODO objects importing from each other

    const object = objects[0];
    var exports = Module.Exports{};
    for (object.exports) |e| {
        try exports.put(ally, try ally.dupe(u8, e.name), e.offset);
    }

    return Module{
        .code = try ally.dupe(u8, object.code),
        .exports = exports,
    };
}
