/// global language systems that need to be accessible throughout the lower
/// stages of compilation
const std = @import("std");
const Allocator = std.mem.Allocator;

// string interning ============================================================

/// an interned string in the string pool
pub const String = packed struct(u64) {
    offset: u32,
    len: u32,

    pub fn eql(a: String, b: String) bool {
        return a.offset == b.offset;
    }

    pub fn slice(s: String) []const u8 {
        return strings.mem.items[s.offset .. s.offset + s.len];
    }

    pub fn format(
        self: String,
        comptime _: []const u8,
        _: std.fmt.FormatOptions,
        writer: anytype,
    ) @TypeOf(writer).Error!void {
        try writer.print("{s}", .{self.slice()});
    }
};

const Strings = struct {
    const Self = @This();

    const hash_seed = 0xDEADBEEF;

    const MapContext = struct {
        data: []const u8,

        pub fn hash(ctx: @This(), k: String) u64 {
            const str = ctx.data[k.offset .. k.offset + k.len];
            return std.hash.Wyhash.hash(hash_seed, str);
        }

        pub fn eql(_: @This(), a: String, b: String) bool {
            return a.eql(b);
        }
    };

    const Map = std.HashMapUnmanaged(
        String,
        void,
        MapContext,
        std.hash_map.default_max_load_percentage,
    );

    /// allows hashing true strings against interned strings
    const StringAdapter = struct {
        data: []const u8,

        pub fn hash(_: @This(), k: []const u8) u64 {
            return std.hash.Wyhash.hash(hash_seed, k);
        }

        pub fn eql(ctx: @This(), a: []const u8, b: String) bool {
            const str = ctx.data[b.offset .. b.offset + b.len];
            return std.mem.eql(u8, a, str);
        }
    };

    mem: std.ArrayListUnmanaged(u8) = .{},
    map: Map = .{},

    fn deinit(self: *Self, ally: Allocator) void {
        self.mem.deinit(ally);
        self.map.deinit(ally);
    }

    fn intern(
        self: *Self,
        ally: Allocator,
        bytes: []const u8,
    ) Allocator.Error!String {
        const ctx = MapContext{ .data = self.mem.items };
        const adapter = StringAdapter{ .data = self.mem.items };

        const res = try self.map.getOrPutContextAdapted(ally, bytes, adapter, ctx);
        if (!res.found_existing) {
            const str = String{
                .offset = @intCast(self.mem.items.len),
                .len = @intCast(bytes.len),
            };
            try self.mem.appendSlice(ally, bytes);

            res.key_ptr.* = str;
            res.value_ptr.* = {};
        }

        return res.key_ptr.*;
    }
};

// unique, pooled names ========================================================

pub const Name = struct {
    id: u32,
};

const NamePool = struct {
    const Self = @This();

    const Node = struct {
        ns: ?Name,
        ident: String,
    };

    const ScopeContext = struct {
        pub fn hash(_: @This(), key: Node) u64 {
            var hasher = std.hash.Wyhash.init(0xDEADBEEF);
            if (key.ns) |ns| hasher.update(std.mem.asBytes(&ns.id));
            hasher.update(std.mem.asBytes(&key.ident));
            return hasher.final();
        }

        pub fn eql(_: @This(), a: Node, b: Node) bool {
            return std.meta.eql(a, b);
        }
    };

    const Scope = std.HashMapUnmanaged(
        Node,
        Name,
        ScopeContext,
        std.hash_map.default_max_load_percentage,
    );

    scope: Scope = .{},

    fn deinit(self: *Self) void {
        self.* = undefined;
    }

    /// create a name in a namespace
    fn intern(
        self: *Self,
        ally: Allocator,
        /// null for a name in the root namespace
        ns: ?Name,
        ident: []const u8,
    ) Allocator.Error!Name {
        const node = Node{
            .ns = ns,
            .ident = string(ident),
        };

        const res = try self.scope.getOrPut(ally, node);
        if (!res.found_existing) {
            const nm = Name{ .id = self.scope.count() };
            res.value_ptr.* = nm;
        }

        return res.value_ptr.*;
    }
};

// interface ===================================================================

var gpa: std.heap.GeneralPurposeAllocator(.{}) = undefined;
var strings: Strings = undefined;
var names: NamePool = undefined;

pub fn init() void {
    gpa = .{};
    strings = .{};
    names = .{};
}

pub fn deinit() void {
    const ally = gpa.allocator();
    names.deinit();
    strings.deinit(ally);
}

fn oom() noreturn {
    std.debug.print("unrecoverable out of memory\n", .{});
    std.process.exit(1);
}

/// place a string in the string pool
pub fn string(str: []const u8) String {
    const ally = gpa.allocator();
    return strings.intern(ally, str) catch oom();
}

/// get/retrieve a name
pub fn name(ns: ?Name, ident: []const u8) Name {
    const ally = gpa.allocator();
    return names.intern(ally, ns, ident) catch oom();
}

// tests =======================================================================

test "string interning" {
    init();
    defer deinit();

    const a = string("hello");
    const b = string("world");
    const c = string("hello");

    try std.testing.expect(a.eql(c));
    try std.testing.expect(!a.eql(b));
    try std.testing.expect(!b.eql(c));

    try std.testing.expectEqualStrings("hello", a.slice());
    try std.testing.expectEqualStrings("world", a.slice());
    try std.testing.expectEqualStrings("hello", c.slice());
}
