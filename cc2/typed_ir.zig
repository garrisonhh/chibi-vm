const std = @import("std");
const Allocator = std.mem.Allocator;
const abstract = @import("abstract_ir.zig");
const Ast = abstract.Ast;
const Aid = abstract.Id;

pub const TypeId = enum(u48) { _ };
pub const CodeId = enum(u48) { _ };
pub const DataId = enum(u48) { _ };

/// represents both the typed ir of a translation unit and the types required to
/// understand it. this data structure is designed in the most DOD manner
/// possible to facilitate future implementation of caching and optimization.
pub const Tir = struct {
    const Self = @This();

    /// storage of type or op
    const Storage = packed struct(u64) {
        tag: u16,
        data: u48,
    };

    ally: Allocator,
    storage: std.ArrayListUnmanaged(Storage) = .{},
    data: std.ArrayListUnmanaged(u8) = .{},

    fn init(ally: Allocator) Tir {
        return .{ .ally = ally };
    }

    pub fn deinit(tir: *Tir) void {
        tir.storage.deinit(tir.ally);
        tir.data.deinit(tir.ally);
    }

    fn ByteAlignedInt(comptime I: type) type {
        if (@typeInfo(I) != .int) {
            @compileError("not an int: " ++ @typeName(I));
        }

        const info = @typeInfo(I).int;
        const bits: u16 = @intCast(std.mem.alignForwardLog2(info.bits, 3));
        return std.meta.Int(info.signedness, bits);
    }

    /// number of bytes `T` will take up in the data array
    fn storedSize(comptime T: type) usize {
        comptime {
            return switch (@typeInfo(T)) {
                .bool => 1,
                .int => |int| std.mem.alignForwardLog2(int.bits, 3) / 8,
                .@"enum" => |@"enum"| storedSize(@"enum".tag_type),
                .@"struct" => |@"struct"| st: {
                    var nbytes: usize = 0;
                    for (@"struct".fields) |field| {
                        nbytes += storedSize(field.type);
                    }

                    break :st nbytes;
                },
                else => @compileError("unknown stored size: " ++ @typeName(T)),
            };
        }
    }

    /// assumes capacity of `tir.data`
    fn storeIntAsBig(data: *std.ArrayList(u8), comptime I: type, n: I) DataId {
        const id: DataId = @enumFromInt(data.items.len);
        const be_value = std.mem.nativeToBig(ByteAlignedInt(I), n);
        data.appendSliceAssumeCapacity(std.mem.asBytes(&be_value));
        return id;
    }

    fn storeDataAssumeCapacity(data: *std.ArrayList(u8), comptime T: type, item: T) DataId {
        return switch (@typeInfo(T)) {
            .bool => storeIntAsBig(data, u1, @intFromBool(item)),
            .int => storeIntAsBig(data, T, item),
            .@"enum" => |@"enum"| storeIntAsBig(data, @"enum".tag_type, @intFromEnum(item)),
            .@"struct" => |@"struct"| st: {
                const id: DataId = @enumFromInt(data.items.len);
                inline for (@"struct".fields) |field| {
                    _ = storeDataAssumeCapacity(data, field.type, @field(item, field.name));
                }
                break :st id;
            },
            else => @compileError("don't know how to store: " ++ @typeName(T)),
        };
    }

    fn loadBytesAssumeInbounds(data: []const u8, id: DataId, comptime len: usize) [len]u8 {
        const slice = data[@intFromEnum(id)..len];
        return @as(*const [len]u8, @ptrCast(slice.ptr)).*;
    }

    fn loadBigIntAssumeInbounds(data: []const u8, comptime I: type, id: DataId) I {
        if (@typeInfo(I) != .int) {
            @compileError("not an int: " ++ @typeName(I));
        }

        const BI = ByteAlignedInt(I);
        const bytes = loadBytesAssumeInbounds(data, id, @sizeOf(BI));
        const big = std.mem.bytesToValue(BI, &bytes);
        return @intCast(std.mem.bigToNative(BI, big));
    }

    fn loadDataAssumeInbounds(data: []const u8, comptime T: type, id: DataId) T {
        return switch (@typeInfo(T)) {
            .bool => 0 != loadBigIntAssumeInbounds(data, u8, id),
            .int => loadBigIntAssumeInbounds(data, T, id),
            .@"enum" => |@"enum"| @enumFromInt(
                loadBigIntAssumeInbounds(data, @"enum".tag_type, id),
            ),
            .@"struct" => |@"struct"| st: {
                var value: T = undefined;
                var offset: u48 = 0;
                inline for (@"struct".fields) |field| {
                    const field_id: DataId = @enumFromInt(@intFromEnum(id) + offset);
                    @field(value, field.name) =
                        loadDataAssumeInbounds(data, field.type, field_id);

                    offset += @intCast(comptime storedSize(field.type));
                }

                break :st value;
            },
            else => @compileError("don't know how to load: " ++ @typeName(T)),
        };
    }

    pub const StoreError = Allocator.Error;

    fn storeData(tir: *Tir, comptime T: type, item: T) StoreError!DataId {
        try tir.data.ensureUnusedCapacity(tir.ally, comptime storedSize(T));
        var alist = tir.data.toManaged(tir.ally);
        defer tir.data = alist.moveToUnmanaged();
        return storeDataAssumeCapacity(&alist, T, item);
    }

    fn store(tir: *Tir, comptime T: type, item: T) StoreError!u48 {
        const stored = comptime storedSize(T);
        const u48_bytes = 48 / 8;
        if (stored <= u48_bytes) {
            var buf: [stored]u8 = undefined;
            var fba = std.heap.FixedBufferAllocator.init(&buf);
            var alist = std.ArrayList(u8).initCapacity(fba.allocator(), stored) catch {
                unreachable;
            };
            _ = storeDataAssumeCapacity(&alist, T, item);
            const StoreInt = std.meta.Int(.unsigned, 8 * stored);
            return std.mem.bytesToValue(StoreInt, alist.items);
        }

        return @intFromEnum(try tir.storeData(T, item));
    }

    pub const LoadError = error{OutOfBounds};

    fn loadData(tir: Tir, comptime T: type, id: DataId) LoadError!T {
        if (tir.data.items.len < @intFromEnum(id) + comptime storedSize(T)) {
            return LoadError.OutOfBounds;
        }
        return loadDataAssumeInbounds(tir.data.items, T, id);
    }

    fn load(tir: Tir, comptime T: type, data: u48) LoadError!T {
        const stored = comptime storedSize(T);
        const u48_bytes = 48 / 8;
        if (stored <= u48_bytes) {
            return loadDataAssumeInbounds(std.mem.asBytes(&data), T, @enumFromInt(0));
        }

        return try tir.loadData(T, @enumFromInt(data));
    }

    fn newStorage(tir: *Tir, comptime T: type, item: T) StoreError!u48 {
        const index = tir.storage.items.len;
        try tir.storage.append(.{
            .tag = @intFromEnum(item),
            .data = switch (item) {
                inline else => |data| try tir.store(@TypeOf(data), data),
            },
        });
        return @intCast(index);
    }

    pub fn newType(tir: *Tir, typ: Type) StoreError!TypeId {
        return @enumFromInt(try tir.newStorage(Type, typ));
    }

    pub fn newOp(tir: *Tir, op: Operation) StoreError!CodeId {
        return @enumFromInt(try tir.newStorage(Operation, op));
    }

    fn getStorage(tir: Tir, comptime T: type, index: u48) LoadError!T {
        const storage = tir.storage[index];
        switch (@as(std.meta.Tag(T), @enumFromInt(storage.tag))) {
            inline else => |tag| {
                const field_type = std.meta.FieldType(T, tag);
                const data = try tir.load(field_type, storage.data);
                return @unionInit(T, @tagName(tag), data);
            },
        }
    }

    pub fn getType(tir: Tir, tid: TypeId) LoadError!Type {
        return try tir.getStorage(Type, @intFromEnum(tid));
    }

    pub fn getOp(tir: Tir, cid: CodeId) LoadError!Operation {
        return try tir.getStorage(Type, @intFromEnum(cid));
    }
};

pub const Operation = union(enum(u16)) {
    literal: Tir.Index,
};

pub const Type = union(enum(u16)) {
    pub const Number = struct {
        pub const Repr = enum(u3) { float, int, uint };

        repr: Repr,
        bits: u8,
    };

    number: Number,
};

pub fn analyze(ast: Ast) !Tir {
    _ = ast;
    @panic("TODO");
}

// testing =====================================================================

fn testStoreAndLoad(tir: *Tir, comptime T: type, data: T) !void {
    const id = try tir.store(T, data);
    const loaded = try tir.load(T, id);
    try std.testing.expectEqualDeep(data, loaded);
}

test "Tir store and load data" {
    var tir = Tir.init(std.testing.allocator);
    defer tir.deinit();

    try testStoreAndLoad(&tir, bool, true);
    try testStoreAndLoad(&tir, bool, false);
    try testStoreAndLoad(&tir, u8, 10);
    try testStoreAndLoad(&tir, i32, -42);
    try testStoreAndLoad(&tir, struct { a: u32, b: u32 }, .{ .a = 123, .b = 456 });
    try testStoreAndLoad(&tir, enum { yes, no, maybe }, .maybe);
}
