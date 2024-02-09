/// global language systems that need to be accessible throughout the lower
/// stages of compilation
const std = @import("std");
const Allocator = std.mem.Allocator;

// string interning ============================================================

/// an interned string in the string pool
pub const String = struct {
    offset: u32,
    len: u32,

    pub fn eql(a: String, b: String) bool {
        return a.offset == b.offset;
    }

    /// this slice may be invalidated with the next string interned, so make
    /// sure to use ephemerally (or copy)
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

    pub fn eql(a: Name, b: Name) bool {
        return a.id == b.id;
    }

    pub fn format(
        self: Name,
        comptime _: []const u8,
        _: std.fmt.FormatOptions,
        writer: anytype,
    ) @TypeOf(writer).Error!void {
        const node = names.get(self);

        if (node.ns) |ns| {
            try writer.print("{}.", .{ns});
        }
        try writer.print("{}", .{node.ident});
    }
};

const NamePool = struct {
    const Self = @This();

    const Node = struct {
        ns: ?Name,
        ident: String,
    };

    const PoolContext = struct {
        pub fn hash(_: @This(), key: Node) u32 {
            var hasher = std.hash.Wyhash.init(0xDEADBEEF);
            if (key.ns) |ns| hasher.update(std.mem.asBytes(&ns.id));
            hasher.update(std.mem.asBytes(&key.ident));
            return @truncate(hasher.final());
        }

        pub fn eql(_: @This(), a: Node, b: Node, _: usize) bool {
            return std.meta.eql(a, b);
        }
    };

    const Pool = std.ArrayHashMapUnmanaged(
        Node,
        Name,
        PoolContext,
        true,
    );

    pool: Pool = .{},

    fn deinit(self: *Self, ally: Allocator) void {
        self.pool.deinit(ally);
        self.* = undefined;
    }

    fn get(self: Self, nm: Name) Node {
        return self.pool.entries.items(.key)[nm.id];
    }

    /// create a name in a namespace
    fn intern(
        self: *Self,
        ally: Allocator,
        /// null for a name in the root namespace
        ns: ?Name,
        ident: String,
    ) Allocator.Error!Name {
        const node = Node{
            .ns = ns,
            .ident = ident,
        };

        const res = try self.pool.getOrPut(ally, node);
        if (!res.found_existing) {
            const count: u32 = @intCast(self.pool.count());
            const nm = Name{ .id = count - 1 };
            res.value_ptr.* = nm;
        }

        return res.value_ptr.*;
    }
};

// types =======================================================================

pub const Type = struct {
    id: u32,

    pub fn eql(a: Type, b: Type) bool {
        return a.id == b.id;
    }

    fn formatChild(
        self: Type,
        child: Type,
        writer: anytype,
    ) @TypeOf(writer).Error!void {
        if (child.eql(self)) {
            try writer.writeAll("Self");
        } else {
            try writer.print("{}", .{child});
        }
    }

    pub fn format(
        ty: Type,
        comptime _: []const u8,
        _: std.fmt.FormatOptions,
        writer: anytype,
    ) @TypeOf(writer).Error!void {
        const info = types.get(ty);
        switch (info) {
            .unit => try writer.writeAll("()"),
            .type, .bool => try writer.writeAll(@tagName(info)),
            .int => |nbytes| try writer.print("i{d}", .{nbytes * 8}),
            .float => |nbytes| try writer.print("f{d}", .{nbytes * 8}),
            .option => |child| try writer.print("(option {})", .{child}),
            .distinct => |d| try writer.print(
                "(distinct {d} {})",
                .{ d.distinct_id, d.child },
            ),
            .ptr => |ptr| try writer.print(
                "(ptr {s} {d} {})",
                .{ @tagName(ptr.kind), ptr.alignment, ptr.child },
            ),
            .function => |func| {
                try writer.writeAll("(-> ");
                for (func.params) |param| {
                    try writer.print("{} ", .{param});
                }
                try writer.print("{})", .{func.returns});
            },
            .@"struct" => |st| {
                try writer.writeAll("(struct");
                for (st.fields) |field| {
                    try writer.print(" {}", .{field.type});
                }
                try writer.writeAll(")");
            },
        }
    }
};

pub const TypeInfo = union(enum) {
    const Self = @This();
    pub const Kind = std.meta.Tag(@This());

    pub const Distinct = struct {
        distinct_id: usize,
        child: Type,
    };

    pub const Pointer = struct {
        pub const Kind = enum { single, many };

        /// pointers can be aligned wider than their natural alignment
        alignment: usize,
        kind: Pointer.Kind,
        child: Type,
    };

    pub const Function = struct {
        params: []const Type,
        returns: Type,
    };

    pub const Field = struct {
        offset: usize,
        type: Type,
    };

    pub const Composite = struct {
        size: usize,
        alignment: usize,
        fields: []const Field,
    };

    unit,
    type,
    bool,
    /// contains byte count
    int: u8,
    /// contains byte count
    float: u8,
    option: Type,
    /// a unique version of a different type
    distinct: Distinct,
    ptr: Pointer,
    function: Function,
    @"struct": Composite,

    fn clone(self: Self, ally: Allocator) Allocator.Error!Self {
        return switch (self) {
            .unit,
            .type,
            .bool,
            .int,
            .float,
            .option,
            .distinct,
            .ptr,
            => self,

            .function => |func| Self{ .function = .{
                .params = try ally.dupe(Type, func.params),
                .returns = func.returns,
            } },
            .@"struct" => |st| Self{ .@"struct" = .{
                .size = st.size,
                .alignment = st.alignment,
                .fields = try ally.dupe(Field, st.fields),
            } },
        };
    }
};

const TypeSet = struct {
    const Self = @This();

    const hash_seed = 0xDEADBEEF;

    const SetContext = struct {
        metas: []const TypeInfo,
        self: ?Type,

        fn hashChild(
            ctx: @This(),
            hasher: *std.hash.Wyhash,
            child: Type,
        ) void {
            // don't recurse on yourself
            if (ctx.self) |itself| {
                if (itself.eql(child)) {
                    hasher.update(&.{0});
                    return;
                }
            }

            ctx.hashRecursive(hasher, ctx.metas[child.id]);
        }

        fn hashRecursive(
            ctx: @This(),
            hasher: *std.hash.Wyhash,
            key: TypeInfo,
        ) void {
            hasher.update(std.mem.asBytes(&@as(TypeInfo.Kind, key)));

            switch (key) {
                .unit, .type, .bool => {},
                .int, .float => |nbytes| {
                    hasher.update(&.{nbytes});
                },
                .option => |child| {
                    ctx.hashChild(hasher, child);
                },
                .function => |func| {
                    for (func.params) |param| {
                        ctx.hashChild(hasher, param);
                    }
                    ctx.hashChild(hasher, func.returns);
                },
                .distinct => |d| {
                    hasher.update(std.mem.asBytes(&d.distinct_id));
                    ctx.hashChild(hasher, d.child);
                },
                .ptr => |ptr| {
                    hasher.update(std.mem.asBytes(&ptr.alignment));
                    hasher.update(std.mem.asBytes(&ptr.kind));
                    ctx.hashChild(hasher, ptr.child);
                },
                .@"struct" => |st| {
                    for (st.fields) |field| {
                        hasher.update(std.mem.asBytes(&field.offset));
                        ctx.hashChild(hasher, field.type);
                    }
                },
            }
        }

        pub fn hash(ctx: @This(), key: TypeInfo) u32 {
            var hasher = std.hash.Wyhash.init(hash_seed);
            ctx.hashRecursive(&hasher, key);
            return @truncate(hasher.final());
        }

        fn isSelf(x: Type, self: ?Type) bool {
            return if (self) |t| x.eql(t) else false;
        }

        /// check if two types are equal with self referentiation awareness
        fn typeEqlSelf(a: Type, aself: ?Type, b: Type, bself: ?Type) bool {
            const a_is_self = isSelf(a, aself);
            const b_is_self = isSelf(b, bself);

            if (a_is_self and b_is_self) {
                return true;
            } else if (a_is_self or b_is_self) {
                return false;
            }

            return a.eql(b);
        }

        pub fn eql(
            ctx: @This(),
            a: TypeInfo,
            b: TypeInfo,
            b_index: usize,
        ) bool {
            if (@as(TypeInfo.Kind, a) != @as(TypeInfo.Kind, b)) {
                return false;
            }

            const aself = ctx.self;
            const bself = Type{ .id = @intCast(b_index) };

            switch (a) {
                .unit, .type, .bool => {},
                inline .int, .float => |a_nbytes, tag| {
                    const b_nbytes = @field(b, @tagName(tag));
                    if (a_nbytes != b_nbytes) return false;
                },
                .option => |achild| {
                    const bchild = b.option;
                    if (!typeEqlSelf(achild, aself, bchild, bself)) {
                        return false;
                    }
                },
                .function => |afunc| {
                    const bfunc = b.function;
                    if (!afunc.returns.eql(bfunc.returns) or
                        afunc.params.len != bfunc.params.len)
                    {
                        return false;
                    }

                    for (afunc.params, bfunc.params) |aparam, bparam| {
                        if (!aparam.eql(bparam)) {
                            return false;
                        }
                    }
                },
                .distinct => |ad| {
                    const bd = b.distinct;
                    if (ad.distinct_id != bd.distinct_id or
                        !typeEqlSelf(ad.child, aself, bd.child, bself))
                    {
                        return false;
                    }
                },
                .ptr => |aptr| {
                    const bptr = b.ptr;
                    if (aptr.alignment != bptr.alignment or
                        aptr.kind != bptr.kind or
                        !typeEqlSelf(aptr.child, aself, bptr.child, bself))
                    {
                        return false;
                    }
                },
                .@"struct" => |ast| {
                    const bst = b.@"struct";

                    if (ast.fields.len != bst.fields.len) {
                        return false;
                    }

                    for (ast.fields, bst.fields) |afield, bfield| {
                        if (afield.offset != bfield.offset or
                            !typeEqlSelf(afield.type, aself, bfield.type, bself))
                        {
                            return false;
                        }
                    }
                },
            }

            return true;
        }
    };

    const Set = std.ArrayHashMapUnmanaged(
        TypeInfo,
        void,
        SetContext,
        true,
    );

    /// used for storing typemetas
    arena: std.heap.ArenaAllocator,
    set: Set = .{},
    distinct_counter: usize = 0,

    /// provide backing ally for typemeta arena
    fn init(backing_ally: Allocator) Self {
        return Self{
            .arena = std.heap.ArenaAllocator.init(backing_ally),
        };
    }

    fn deinit(self: *Self, ally: Allocator) void {
        self.set.deinit(ally);
        self.arena.deinit();
    }

    /// get a distinct number
    fn nextDistinct(self: *Self) usize {
        defer self.distinct_counter += 1;
        return self.distinct_counter;
    }

    /// use this to reserve a self-referential type
    fn reserveType(self: *Self, ally: Allocator) Allocator.Error!Type {
        // creates a (useless) distinct unit type which can later be
        // replaced
        const unit = try self.intern(ally, null, .unit);
        return try self.intern(ally, null, .{ .distinct = .{
            .distinct_id = self.nextDistinct(),
            .child = unit,
        } });
    }

    fn intern(
        self: *Self,
        ally: Allocator,
        itself: ?Type,
        meta: TypeInfo,
    ) Allocator.Error!Type {
        const ctx = SetContext{
            .metas = self.set.keys(),
            .self = itself,
        };

        const res = try self.set.getOrPutContext(ally, meta, ctx);
        if (!res.found_existing) {
            res.key_ptr.* = try meta.clone(self.arena.allocator());
            if (itself) |reserved| {
                // self-referential type should replace its reserved
                // self-reference
                const reserved_meta = self.get(reserved);
                const success = self.set.swapRemoveContext(reserved_meta, ctx);
                std.debug.assert(success);

                return reserved;
            }
        }

        return Type{ .id = @intCast(res.index) };
    }

    fn get(self: Self, ty: Type) TypeInfo {
        return self.set.keys()[ty.id];
    }

    /// get the size in bytes of a type
    ///
    /// *this is a trivial operation*
    fn sizeOf(self: Self, ty: Type) usize {
        return switch (self.get(ty)) {
            .unit => 0,
            .bool => 1,
            .int, .float => |nbytes| nbytes,
            // child value + a byte which contains a validity flag
            .option => |child| self.sizeOf(child) + 1,
            .distinct => |d| self.sizeOf(d.child),
            .ptr => 8,
            .@"struct" => |st| st.size,

            // unsized
            .builtin, .type, .function => 0,
        };
    }

    /// get the alignment in bytes of a type
    ///
    /// *this is a trivial operation*
    fn alignOf(self: Self, ty: Type) usize {
        return switch (self.get(ty)) {
            .unit => 0,
            .bool => 1,
            .int, .float => |nbytes| nbytes,
            .option => |child| self.sizeOf(child),
            .distinct => |d| self.alignOf(d.child),
            .ptr => 8,
            .@"struct" => |st| st.alignment,

            // unsized
            .builtin, .type, .function => 0,
        };
    }

    /// get the aligned size in bytes of a type (for purposes like calculating
    /// array size)
    ///
    /// *this is a trivial operation*
    fn alignedSizeOf(self: Self, ty: Type) usize {
        return std.mem.alignForward(usize, self.sizeOf(ty), self.alignOf(ty));
    }
};

// globals and prelude =========================================================

pub const Global = union(enum) {
    type: Type,
    /// constant bytes
    bytes: []const u8,

    fn clone(self: Global, ally: Allocator) Allocator.Error!Global {
        return switch (self) {
            .type => self,
            .bytes => |bytes| .{ .bytes = try ally.dupe(u8, bytes) },
        };
    }
};

pub const GlobalError = error{NameConflict};

pub const GlobalMeta = struct {
    name: Name,
    type: Type,
    global: Global,
};

const Globals = struct {
    const Self = @This();

    const Map = std.AutoHashMapUnmanaged(Name, GlobalMeta);

    arena: std.heap.ArenaAllocator,
    map: Map = .{},

    /// provide backing allocator for value arena
    fn init(backing_ally: Allocator) Self {
        return .{ .arena = std.heap.ArenaAllocator.init(backing_ally) };
    }

    fn deinit(self: *Self, ally: Allocator) void {
        self.map.deinit(ally);
        self.arena.deinit();
    }

    /// defines a global and clones its value
    fn add(
        self: *Self,
        ally: Allocator,
        nm: Name,
        ty: Type,
        value: Global,
    ) (GlobalError || Allocator.Error)!void {
        const res = try self.map.getOrPut(ally, nm);
        if (res.found_existing) {
            return GlobalError.NameConflict;
        }

        res.value_ptr.* = GlobalMeta{
            .name = nm,
            .type = ty,
            .global = try value.clone(self.arena.allocator()),
        };
    }

    /// debugging function
    fn dump(self: Self) void {
        var entries = self.map.iterator();
        while (entries.next()) |entry| {
            std.debug.print("{}: {} = {}\n", .{
                entry.key_ptr.*,
                entry.value_ptr.type,
                entry.value_ptr.global,
            });
        }
    }
};

/// represents a local scope that can reference the global scope
pub const Scope = struct {
    // TODO
};

// interface ===================================================================

var gpa: std.heap.GeneralPurposeAllocator(.{}) = undefined;
var strings: Strings = undefined;
var names: NamePool = undefined;
var typeset: TypeSet = undefined;
var globals: Globals = undefined;

pub fn init() void {
    // order matters a lot here, these rely extensively on each other
    gpa = .{};
    const ally = gpa.allocator();

    strings = .{};
    names = .{};
    typeset = TypeSet.init(ally);
    globals = Globals.init(ally);

    initPrelude();
}

pub fn deinit() void {
    const ally = gpa.allocator();

    globals.deinit(ally);
    typeset.deinit(ally);
    names.deinit(ally);
    strings.deinit(ally);
}

fn initPrelude() void {
    const Def = struct {
        name: []const u8,
        ty: Type,
        value: Global,

        fn init(nm: []const u8, ty: Type, value: Global) @This() {
            return .{
                .name = nm,
                .ty = ty,
                .value = value,
            };
        }
    };

    const @"type" = types.type();
    const @"bool" = types.bool();

    const d = Def.init;
    const defs = [_]Def{
        d("bool", @"type", .{ .type = @"bool" }),
        d("i8", @"type", .{ .type = types.int(1) }),
        d("i16", @"type", .{ .type = types.int(2) }),
        d("i32", @"type", .{ .type = types.int(4) }),
        d("i64", @"type", .{ .type = types.int(8) }),
        d("f16", @"type", .{ .type = types.float(2) }),
        d("f32", @"type", .{ .type = types.float(4) }),
        d("f64", @"type", .{ .type = types.float(8) }),
    };

    for (defs) |def| {
        const nm = name(null, string(def.name));
        must(define(nm, def.ty, def.value));
    }

    globals.dump();
}

fn unrecoverable(e: anyerror) noreturn {
    std.debug.print("unrecoverable error: {s}", .{@errorName(e)});
    std.process.exit(1);
}

fn must(x: anytype) @typeInfo(@TypeOf(x)).ErrorUnion.payload {
    return x catch |e| unrecoverable(e);
}

/// place a string in the string pool
pub fn string(str: []const u8) String {
    const ally = gpa.allocator();
    return must(strings.intern(ally, str));
}

/// get/retrieve a name
pub fn name(ns: ?Name, ident: String) Name {
    const ally = gpa.allocator();
    return must(names.intern(ally, ns, ident));
}

/// get the namespace of a name
pub fn namespace(nm: Name) ?Name {
    return names.get(nm).ns;
}

/// get the identifier of a name
pub fn namebase(nm: Name) String {
    return names.get(nm).ident;
}

/// define a global
pub fn define(nm: Name, ty: Type, value: Global) GlobalError!void {
    const ally = gpa.allocator();
    return globals.add(ally, nm, ty, value) catch |e| switch (e) {
        GlobalError.NameConflict => @as(GlobalError, @errSetCast(e)),
        else => |bad_err| unrecoverable(bad_err),
    };
}

/// retrieve a global
pub fn get(nm: Name) ?GlobalMeta {
    return globals.map.get(nm);
}

/// look up an identifier in a namespace
pub fn lookup(ns: Name, ident: String) ?GlobalMeta {
    var trav = ns;
    while (true) {
        if (get(name(trav, ident))) |meta| {
            return meta;
        }

        trav = namespace(trav) orelse break;
    }

    return get(name(null, ident));
}

pub const types = struct {
    pub fn get(ty: Type) TypeInfo {
        return typeset.get(ty);
    }

    pub fn sizeOf(ty: Type) usize {
        return typeset.sizeOf(ty);
    }

    pub fn alignOf(ty: Type) usize {
        return typeset.alignOf(ty);
    }

    pub fn alignedSizeOf(ty: Type) usize {
        return typeset.alignedSizeOf(ty);
    }

    fn make(itself: ?Type, meta: TypeInfo) Type {
        const ally = gpa.allocator();
        return must(typeset.intern(ally, itself, meta));
    }

    /// reserve a type to create self referential types
    pub fn self() Type {
        const ally = gpa.allocator();
        return must(typeset.reserveType(ally));
    }

    pub fn freeSelf(ty: Type) void {
        must(typeset.freeUnusedType(ty));
    }

    pub fn unit() Type {
        return make(null, .unit);
    }

    pub fn @"type"() Type {
        return make(null, .type);
    }

    pub fn @"bool"() Type {
        return make(null, .bool);
    }

    pub fn int(nbytes: u8) Type {
        return make(null, .{ .int = nbytes });
    }

    pub fn float(nbytes: u8) Type {
        return make(null, .{ .float = nbytes });
    }

    pub fn option(ty: Type) Type {
        return make(null, .{ .option = ty });
    }

    pub fn distinct(ty: Type) Type {
        return make(null, .{ .distinct = .{
            .distinct_id = typeset.nextDistinct(),
            .child = ty,
        } });
    }

    pub const PointerKind = TypeInfo.Pointer.Kind;

    /// if alignment is null, natural alignment of child will be used
    pub fn ptr(kind: PointerKind, aln: ?usize, child: Type) Type {
        return make(null, .{ .ptr = .{
            .kind = kind,
            .alignment = aln orelse alignOf(child),
            .child = child,
        } });
    }

    pub fn function(params: []const Type, returns: Type) Type {
        return make(null, .{ .function = .{
            .params = params,
            .returns = returns,
        } });
    }

    pub const StructField = struct {
        ident: String,
        type: Type,
    };

    /// sort struct fields by descending alignment
    fn structFieldSorter(_: void, a: TypeInfo.Field, b: TypeInfo.Field) bool {
        const aln_a = alignOf(a.type);
        const aln_b = alignOf(b.type);
        return aln_a > aln_b;
    }

    pub fn @"struct"(itself: ?Type, fields: []const StructField) Type {
        const ally = gpa.allocator();
        const meta_fields = must(ally.alloc(TypeInfo.Field, fields.len));
        defer ally.free(meta_fields);

        // collect struct fields without calculating offset yet
        for (fields, meta_fields) |field, *slot| {
            slot.* = TypeInfo.Field{
                .offset = 0,
                .ident = field.ident,
                .type = field.type,
            };
        }

        // sort by descending alignment
        std.sort.block(TypeInfo.Field, meta_fields, {}, structFieldSorter);

        // deduce actual field offsets
        var size: usize = 0;
        var aln: usize = 0;
        for (meta_fields) |*field| {
            const field_aln = alignOf(field.type);
            size = std.mem.alignForward(usize, size, field_aln);
            aln = @max(aln, field_aln);
            field.offset = size;
            size += sizeOf(field.type);
        }

        return make(itself, .{ .@"struct" = .{
            .size = size,
            .alignment = aln,
            .fields = meta_fields,
        } });
    }
};

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
    try std.testing.expectEqualStrings("world", b.slice());
    try std.testing.expectEqualStrings("hello", c.slice());
}

test "name interning" {
    init();
    defer deinit();

    const a = name(null, string("a"));
    const b = name(null, string("b"));
    const c = name(null, string("c"));
    const bc = name(b, string("c"));
    const bc2 = name(b, string("c"));
    const ac = name(a, string("c"));

    try std.testing.expect(!a.eql(b));
    try std.testing.expect(!b.eql(c));
    try std.testing.expect(bc.eql(bc2));
    try std.testing.expect(!ac.eql(bc));
    try std.testing.expect(if (namespace(bc)) |parent| parent.eql(b) else false);
    try std.testing.expect(if (namespace(bc2)) |parent| parent.eql(b) else false);
    try std.testing.expect(if (namespace(ac)) |parent| parent.eql(a) else false);

    try std.testing.expectFmt(
        "a b c b.c b.c a.c",
        "{} {} {} {} {} {}",
        .{ a, b, c, bc, bc2, ac },
    );
}

test "primitive types" {
    init();
    defer deinit();

    const unit = types.unit();
    const unit2 = types.unit();
    const distinct_unit = types.distinct(types.unit());
    const @"bool" = types.bool();
    const @"i32" = types.int(4);
    const i32_2 = types.int(4);
    const @"f64" = types.float(8);

    try std.testing.expectEqual(unit, unit2);
    try std.testing.expectEqual(@"i32", i32_2);
    try std.testing.expect(!@"i32".eql(unit));
    try std.testing.expect(!distinct_unit.eql(unit));

    try std.testing.expectEqual(@as(usize, 0), types.sizeOf(unit));
    try std.testing.expectEqual(@as(usize, 1), types.sizeOf(@"bool"));
    try std.testing.expectEqual(@as(usize, 4), types.sizeOf(@"i32"));
    try std.testing.expectEqual(@as(usize, 8), types.sizeOf(@"f64"));

    try std.testing.expectEqual(@as(usize, 0), types.alignOf(unit));
    try std.testing.expectEqual(@as(usize, 1), types.alignOf(@"bool"));
    try std.testing.expectEqual(@as(usize, 4), types.alignOf(@"i32"));
    try std.testing.expectEqual(@as(usize, 8), types.alignOf(@"f64"));

    try std.testing.expectEqual(@as(u32, 0), unit.id);
    try std.testing.expectEqual(@as(u32, 1), distinct_unit.id);
    try std.testing.expectEqual(@as(u32, 2), @"bool".id);
    try std.testing.expectEqual(@as(u32, 3), @"i32".id);
    try std.testing.expectEqual(@as(u32, 4), @"f64".id);
}

test "trivial structs" {
    init();
    defer deinit();

    const a = types.@"struct"(null, &.{});
    const b = types.@"struct"(null, &.{});

    try std.testing.expectEqual(a, b);
    try std.testing.expectEqual(@as(usize, 0), types.sizeOf(a));
    try std.testing.expectEqual(@as(usize, 0), types.alignOf(a));

    const c = types.@"struct"(null, &[_]types.StructField{
        .{ .ident = string("x"), .type = types.int(4) },
        .{ .ident = string("y"), .type = types.int(4) },
    });
    try std.testing.expectEqual(@as(usize, 8), types.sizeOf(c));
    try std.testing.expectEqual(@as(usize, 4), types.alignOf(c));
}

test "self-referential struct" {
    init();
    defer deinit();

    const listnode = types.self();
    const ptr_listnode = types.ptr(.single, null, listnode);
    const opt_ptr_listnode = types.option(ptr_listnode);

    const resolved = types.@"struct"(listnode, &[_]types.StructField{
        .{ .ident = string("next"), .type = opt_ptr_listnode },
        .{ .ident = string("data"), .type = types.int(4) },
    });

    const listnode2 = types.self();
    const ptr_listnode2 = types.ptr(.single, null, listnode2);
    const opt_ptr_listnode2 = types.option(ptr_listnode2);

    const resolved2 = types.@"struct"(listnode, &[_]types.StructField{
        .{ .ident = string("next"), .type = opt_ptr_listnode2 },
        .{ .ident = string("data"), .type = types.int(4) },
    });
    const distinct_resolved = types.distinct(resolved);

    try std.testing.expectEqual(resolved, resolved2);
    try std.testing.expect(!resolved.eql(distinct_resolved));
}
