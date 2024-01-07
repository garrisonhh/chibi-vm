const std = @import("std");
const Allocator = std.mem.Allocator;
const ops = @import("ops.zig");
const Op = ops.Op;
const ByteOp = ops.ByteOp;

const Export = struct {
    name: []const u8,
    /// address of label
    offset: usize,
};

const Import = struct {
    name: []const u8,
    /// where the address should be written
    offset: usize,
};

/// a bytecode translation unit
pub const Object = struct {
    code: []const u8,
    exports: []const Export,
    imports: []const Import,

    pub fn deinit(self: Object, ally: Allocator) void {
        ally.free(self.code);
        ally.free(self.exports);
        ally.free(self.imports);
    }
};

/// fully linked and executable bytecode
pub const SharedObject = struct {
    pub const Exports = std.StringHashMapUnmanaged(usize);

    code: []const u8,
    /// maps label -> offset
    exports: Exports,

    // TODO I hate that this has to be mutable
    pub fn deinit(self: *SharedObject, ally: Allocator) void {
        ally.free(self.code);

        var keys = self.exports.keyIterator();
        while (keys.next()) |key| ally.free(key.*);
        self.exports.deinit(ally);
    }
};

/// builder builds objects
pub const Builder = struct {
    const Self = @This();

    ally: Allocator,
    code: std.ArrayListUnmanaged(u8) = .{},
    exports: std.ArrayListUnmanaged(Export) = .{},
    imports: std.ArrayListUnmanaged(Import) = .{},

    pub fn init(ally: Allocator) Self {
        return Self{ .ally = ally };
    }

    /// also deinits builder
    pub fn build(self: *Self) Allocator.Error!Object {
        const ally = self.ally;
        defer self.* = undefined;
        return Object{
            .code = try self.code.toOwnedSlice(ally),
            .exports = try self.exports.toOwnedSlice(ally),
            .imports = try self.imports.toOwnedSlice(ally),
        };
    }

    /// exports a label from the current location in code
    /// *name must outlive builder + object*
    pub fn @"export"(self: *Self, name: []const u8) Allocator.Error!void {
        try self.exports.append(self.ally, Export{
            .name = name,
            .offset = self.code.items.len,
        });
    }

    /// writes an undefined usize and marks down what label to write to these
    /// bytes. this should really only be used for
    /// *name must outlive builder + object*
    fn import(self: *Self, name: []const u8) Allocator.Error!void {
        const ally = self.ally;

        const offset = self.code.items.len;
        try self.code.appendNTimes(ally, undefined, @sizeOf(usize));
        try self.imports.append(ally, Import{
            .name = name,
            .offset = offset,
        });
    }

    pub fn op(self: *Self, o: Op) Allocator.Error!void {
        const ally = self.ally;

        // create byte op from op and append
        var bo = ByteOp{ .opcode = o };
        switch (o) {
            .halt, .enter, .ret => {},
            .constant => |c| {
                bo.width = c;
            },

            .add, .sub, .mulu, .muli, .divu, .divi, .mod, .neg => |w| {
                bo.width = w;
            },
        }
        try self.code.append(ally, @bitCast(bo));

        // add any other data
        switch (o) {
            .enter => |stack_size| {
                try self.code.appendSlice(ally, std.mem.asBytes(&stack_size));
            },
            .constant => |data| switch (data) {
                inline else => |bytes| {
                    try self.code.appendSlice(ally, &bytes);
                },
            },

            else => {},
        }
    }
};

/// links objects into executable form
pub fn link(ally: Allocator, objects: []const Object,) Allocator.Error!SharedObject {
    std.debug.assert(objects.len == 1);
    std.debug.assert(objects[0].imports.len == 0);

    // TODO actually link multiple objects

    const object = objects[0];
    var exports = SharedObject.Exports{};
    for (object.exports) |e| {
        try exports.put(ally, try ally.dupe(u8, e.name), e.offset);
    }

    return SharedObject{
        .code = try ally.dupe(u8, object.code),
        .exports = exports,
    };
}