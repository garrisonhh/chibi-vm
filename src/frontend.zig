//! interfaces with chibi and converts chibi types to more human zig types

const std = @import("std");
const Allocator = std.mem.Allocator;
const in_debug = @import("builtin").mode == .Debug;
const chibi = @import("chibi.zig");

// cross-language utils ========================================================

fn cStrSlice(c_str: [*:0]const u8) [:0]const u8 {
    const len = std.mem.indexOfSentinel(u8, 0, c_str);
    return c_str[0..len :0];
}

/// chibi objects tend to use a next pointer for lists
fn countChibi(comptime T: type, list: ?*T) usize {
    var count: usize = 0;
    var trav = list;
    while (trav) |item| : (trav = item.next) {
        count += 1;
    }

    return count;
}

fn ChibiIterator(comptime T: type) type {
    return struct {
        trav: ?*T,

        pub fn next(self: *@This()) ?*T {
            if (self.trav) |item| {
                self.trav = item.next;
                return item;
            }

            return null;
        }
    };
}

fn iterateChibi(comptime T: type, list: ?*T) ChibiIterator(T) {
    return .{ .trav = list };
}

fn chibiTokenStr(name: ?*chibi.Token) ?[]const u8 {
    const tok = name orelse return null;
    return tok.loc[0..@intCast(tok.len)];
}

// zig data structures =========================================================

pub const Source = struct {
    name: [:0]const u8,
    contents: [:0]const u8,
};

pub const Member = struct {
    const Self = @This();

    name: ?[]const u8,
    type: *Type,
    offset: usize,

    fn fromChibi(ally: Allocator, member: *chibi.Member) Allocator.Error!Self {
        const name = chibiTokenStr(member.name);
        const ty = try Type.fromChibiAlloc(ally, member.ty);

        return Self{
            .name = name,
            .type = ty,
            .offset = @intCast(member.offset),
        };
    }

    fn fromChibiAlloc(
        ally: Allocator,
        member: *chibi.Member,
    ) Allocator.Error!*Self {
        const ptr = try ally.create(Self);
        ptr.* = fromChibi(ally, member);
        return ptr;
    }

    fn dumpIndented(self: Self, level: u32) void {
        for (0..level * 2) |_| std.debug.print(" ", .{});
        std.debug.print("(+{d}) ", .{self.offset});
        if (self.name) |name| {
            std.debug.print("{s}: ", .{name});
        }
        std.debug.print("{}\n", .{self.type});
    }
};

/// a more easy to work with chibi type
///
/// chibi types are each individually heap-allocated trees, so this mirrors
/// that design
pub const Type = struct {
    const Self = @This();

    pub const Int = struct {
        signedness: std.builtin.Signedness,
    };

    pub const Ptr = struct {
        child: *Self,
    };

    pub const Array = struct {
        child: *Self,
        len: usize,
    };

    pub const Func = struct {
        params: []const *Self,
        returns: *Self,
    };

    pub const Data = union(chibi.TypeKind) {
        void,
        bool,
        char: Int,
        short: Int,
        int: Int,
        long: Int,
        float,
        double,
        ldouble,
        @"enum",
        ptr: Ptr,
        func: Func,
        array: Array,
        vla,
        @"struct": []const Member,
        @"union",
    };

    name: ?[]const u8,
    size: usize,
    alignment: u29,
    data: Data,

    fn fromChibi(ally: Allocator, ty: *chibi.Type) Allocator.Error!Self {
        const data = switch (ty.kind) {
            inline .void,
            .bool,
            => |kind| @unionInit(Data, @tagName(kind), {}),

            // integers
            inline .char,
            .short,
            .int,
            .long,
            => |kind| @unionInit(Data, @tagName(kind), Int{
                .signedness = if (ty.is_unsigned) .unsigned else .signed,
            }),

            // floats
            inline .float,
            .double,
            .ldouble,
            => |kind| @unionInit(Data, @tagName(kind), {}),

            .ptr => Data{ .ptr = Ptr{
                .child = try fromChibiAlloc(ally, ty.base.?),
            } },

            .func => func: {
                const nparams = countChibi(chibi.Type, ty.params);
                const params = try ally.alloc(*Self, nparams);

                var i: usize = 0;
                var iter = iterateChibi(chibi.Type, ty.params);
                while (iter.next()) |param_ty| : (i += 1) {
                    params[i] = try fromChibiAlloc(ally, param_ty);
                }

                const returns = try fromChibiAlloc(ally, ty.return_ty.?);

                break :func Data{ .func = .{
                    .params = params,
                    .returns = returns,
                } };
            },
            .@"struct" => st: {
                const nmembers = countChibi(chibi.Member, ty.members);
                const members = try ally.alloc(Member, nmembers);

                var i: usize = 0;
                var iter = iterateChibi(chibi.Member, ty.members);
                while (iter.next()) |member| : (i += 1) {
                    members[i] = try Member.fromChibi(ally, member);
                }

                break :st Data{ .@"struct" = members };
            },
            .array => Data{ .array = .{
                .child = try fromChibiAlloc(ally, ty.base.?),
                .len = @intCast(ty.array_len),
            } },

            inline else => |type_kind| @unionInit(
                Data,
                @tagName(type_kind),
                {},
            ),
        };

        return Self{
            .name = chibiTokenStr(ty.name),
            .size = @intCast(ty.size),
            .alignment = @intCast(ty.@"align"),
            .data = data,
        };
    }

    /// just heap allocates the result of `fromChibi`
    fn fromChibiAlloc(ally: Allocator, ty: *chibi.Type) Allocator.Error!*Self {
        const box = try ally.create(Self);
        box.* = try fromChibi(ally, ty);
        return box;
    }

    pub fn eql(self: Self, other: Self) bool {
        if (@as(chibi.TypeKind, self.data) != @as(chibi.TypeKind, other.data)) {
            return false;
        }

        switch (self.data) {
            inline else => |this, tag| {
                const that = @field(other.data, @tagName(tag));
                return switch (@TypeOf(this, that)) {
                    void => true,
                    Int => this.signedness == that.signedness,
                    Ptr => this.child.eql(that.child.*),
                    Func => if (this.params.len != that.params.len) func: {
                        break :func false;
                    } else for (this.params, that.params) |p1, p2| {
                        if (!p1.eql(p2.*)) {
                            break false;
                        }
                    } else this.returns.eql(that.returns.*),
                    []const Member => if (this.len != that.len) membs: {
                        break :membs false;
                    } else for (this, that) |m1, m2| {
                        if (m1.offset != m2.offset or !m1.type.eql(m2.type.*)) {
                            break false;
                        }
                    } else true,

                    else => unreachable,
                };
            },
        }
    }

    pub fn isInt(self: Self) bool {
        return switch (self.data) {
            .char, .short, .int, .long => true,
            else => false,
        };
    }

    pub fn isFloat(self: Self) bool {
        return switch (self.data) {
            .float, .double, .ldouble => true,
            else => false,
        };
    }

    /// formatting these with zig-style syntax for a lot of thigns for my own
    /// readability
    pub fn format(
        self: Self,
        comptime _: []const u8,
        _: std.fmt.FormatOptions,
        writer: anytype,
    ) @TypeOf(writer).Error!void {
        switch (self.data) {
            .void, .bool, .float, .double => {
                try writer.print("{s}", .{@tagName(self.data)});
            },
            .char, .short, .int, .long => |int| {
                if (int.signedness == .unsigned) {
                    try writer.writeAll("unsigned ");
                }

                try writer.writeAll(@tagName(self.data));
            },
            .ldouble => {
                try writer.writeAll("long double");
            },

            .ptr => |ptr| {
                try writer.print("*{}", .{ptr.child});
            },
            .func => |func| {
                try writer.writeAll("fn(");
                for (func.params, 0..) |param, i| {
                    if (i > 0) try writer.writeAll(", ");
                    try writer.print("{}", .{param});
                }
                try writer.print(") {}", .{func.returns});
            },
            .@"struct" => |fields| {
                try writer.writeAll("struct { ");
                for (fields) |field| {
                    if (field.name) |name| {
                        try writer.print("{s}: ", .{name});
                    }
                    try writer.print("{}; ", .{field.type});
                }
                try writer.print("}}", .{});
            },
            .array => |array| {
                try writer.print("{}[{}]", .{array.child, array.len});
            },

            else => @panic("TODO"),
        }
    }
};

pub const Node = struct {
    const Self = @This();

    pub const Var = struct {
        name: []const u8,
        ty: *Type,
    };

    pub const If = struct {
        cond: *Node,
        then: *Node,
        @"else": ?*Node,
    };

    pub const For = struct {
        init: ?*Node,
        cond: ?*Node,
        iter: ?*Node,
        body: *Node,
    };

    pub const Do = struct {
        body: *Node,
        cond: *Node,
    };

    pub const Number = union(enum) {
        int: u64,
        float: f64,
    };

    pub const MemberAccess = struct {
        obj: *Node,
        member: Member,
    };

    pub const Deref = struct {
        obj: *Node,
        member: ?Member,
    };

    pub const Funcall = struct {
        func: *Node,
        args: []const Node,
    };

    pub const Data = union(chibi.NodeKind) {
        null_expr,
        add: [2]*Node,
        sub: [2]*Node,
        mul: [2]*Node,
        div: [2]*Node,
        neg: *Node,
        mod: [2]*Node,
        bitand: [2]*Node,
        bitor: [2]*Node,
        bitxor: [2]*Node,
        shl: [2]*Node,
        shr: [2]*Node,
        eq: [2]*Node,
        ne: [2]*Node,
        lt: [2]*Node,
        le: [2]*Node,
        assign: [2]*Node,
        cond,
        comma: [2]*Node,
        member: MemberAccess,
        addr: *Node,
        deref: Deref,
        not,
        bitnot,
        logand,
        logor,
        @"return": *Node,
        @"if": If,
        @"for": For,
        do: Do,
        @"switch",
        case,
        block: []const Node,
        goto,
        goto_expr,
        label,
        label_val,
        funcall: Funcall,
        expr_stmt: *Node,
        stmt_expr: *Node,
        @"var": Var,
        vla_ptr,
        num: Number,
        cast: *Node,
        memzero: Var,
        @"asm",
        cas,
        exch,
    };

    ty: ?Type,
    data: Data,

    fn fromChibi(ally: Allocator, node: *chibi.Node) Allocator.Error!Self {
        const ty: ?Type = if (node.ty) |chibi_ty| ty: {
            break :ty try Type.fromChibi(ally, chibi_ty);
        } else null;

        const data: Data = switch (node.kind) {
            // unary
            inline .neg,
            .@"return",
            .cast,
            .expr_stmt,
            .stmt_expr,
            .addr,
            => |tag| @unionInit(
                Data,
                @tagName(tag),
                try fromChibiAlloc(ally, node.lhs.?),
            ),

            // binary
            inline .add,
            .sub,
            .mul,
            .div,
            .mod,
            .bitand,
            .bitor,
            .bitxor,
            .shr,
            .shl,
            .eq,
            .ne,
            .lt,
            .le,
            .assign,
            .comma,
            => |tag| @unionInit(
                Data,
                @tagName(tag),
                .{
                    try fromChibiAlloc(ally, node.lhs.?),
                    try fromChibiAlloc(ally, node.rhs.?),
                },
            ),

            // more complex
            .block => Data{
                .block = try fromChibiSlice(ally, node.body),
            },
            .@"if" => Data{
                .@"if" = .{
                    .cond = try fromChibiAlloc(ally, node.cond.?),
                    .then = try fromChibiAlloc(ally, node.then.?),
                    .@"else" = if (node.els) |els| els: {
                        break :els try fromChibiAlloc(ally, els);
                    } else null,
                },
            },
            .@"for" => Data{
                .@"for" = .{
                    .init = if (node.init) |n| try fromChibiAlloc(ally, n) else null,
                    .cond = if (node.cond) |n| try fromChibiAlloc(ally, n) else null,
                    .iter = if (node.inc) |n| try fromChibiAlloc(ally, n) else null,
                    .body = try fromChibiAlloc(ally, node.then.?),
                },
            },
            .do => Data{
                .do = .{
                    .body = try fromChibiAlloc(ally, node.then.?),
                    .cond = try fromChibiAlloc(ally, node.cond.?),
                },
            },
            .num => Data{
                .num = switch (ty.?.data) {
                    .char, .short, .int, .long => .{ .int = node.val },
                    .float, .double, .ldouble => .{ .float = @floatCast(node.fval) },
                    else => unreachable,
                },
            },
            inline .@"var", .memzero => |tag| @unionInit(Data, @tagName(tag), .{
                .ty = try Type.fromChibiAlloc(ally, node.@"var".?.ty),
                .name = cStrSlice(node.@"var".?.name),
            }),
            .funcall => Data{ .funcall = .{
                .func = try fromChibiAlloc(ally, node.lhs.?),
                .args = try fromChibiSlice(ally, node.args),
            } },
            .member => Data{ .member = .{
                .obj = try fromChibiAlloc(ally, node.lhs.?),
                .member = try Member.fromChibi(ally, node.member.?),
            } },
            .deref => Data{ .deref = .{
                .obj = try fromChibiAlloc(ally, node.lhs.?),
                .member = if (node.member) |m| try Member.fromChibi(ally, m) else null,
            } },

            inline else => |tag| @unionInit(Data, @tagName(tag), {}),
        };

        return Self{
            .ty = ty,
            .data = data,
        };
    }

    fn fromChibiAlloc(ally: Allocator, ty: *chibi.Node) Allocator.Error!*Self {
        const box = try ally.create(Self);
        box.* = try fromChibi(ally, ty);
        return box;
    }

    fn fromChibiSlice(
        ally: Allocator,
        list: ?*chibi.Node,
    ) Allocator.Error![]const Self {
        var nodes = std.ArrayListUnmanaged(Node){};
        var node_iter = iterateChibi(chibi.Node, list);
        while (node_iter.next()) |chibi_node| {
            const node = try Node.fromChibi(ally, chibi_node);
            try nodes.append(ally, node);
        }

        return try nodes.toOwnedSlice(ally);
    }

    fn dumpIndented(self: Self, id: ?[]const u8, level: u32) void {
        for (0..level * 2) |_| std.debug.print(" ", .{});
        if (id) |got| {
            std.debug.print("{s}: ", .{got});
        }
        std.debug.print("[{s}] {?}\n", .{ @tagName(self.data), self.ty });

        switch (self.data) {
            inline else => |meta| switch (@TypeOf(meta)) {
                void => {},
                Var => {
                    for (0..(level + 1) * 2) |_| std.debug.print(" ", .{});
                    std.debug.print("{s}: {}\n", .{ meta.name, meta.ty });
                },
                If => {
                    meta.cond.dumpIndented("cond", level + 1);
                    meta.then.dumpIndented("then", level + 1);
                    if (meta.@"else") |@"else"| {
                        @"else".dumpIndented("else", level + 1);
                    }
                },
                For => {
                    if (meta.init) |child| {
                        child.dumpIndented("init", level + 1);
                    }
                    if (meta.cond) |child| {
                        child.dumpIndented("cond", level + 1);
                    }
                    if (meta.iter) |child| {
                        child.dumpIndented("iter", level + 1);
                    }
                    meta.body.dumpIndented("body", level + 1);
                },
                Funcall => {
                    meta.func.dumpIndented("function", level + 1);
                    for (meta.args) |arg| {
                        arg.dumpIndented("argument", level + 1);
                    }
                },
                Deref => {
                    meta.obj.dumpIndented("of", level + 1);
                    if (meta.member) |m| {
                        m.dumpIndented(level + 1);
                    }
                },
                MemberAccess => {
                    meta.obj.dumpIndented("of", level + 1);
                    meta.member.dumpIndented(level + 1);
                },
                Number => {
                    for (0..(level + 1) * 2) |_| std.debug.print(" ", .{});
                    switch (meta) {
                        inline else => |data| {
                            std.debug.print("{d}\n", .{data});
                        },
                    }
                },
                *Node, *Object => {
                    meta.dumpIndented(null, level + 1);
                },
                []const Node, [2]*Node => {
                    for (meta) |child| {
                        child.dumpIndented(null, level + 1);
                    }
                },
                else => unreachable,
            },
        }
    }

    fn dump(self: Self) void {
        self.dumpIndented(null, 0);
    }
};

pub const Object = struct {
    const Self = @This();

    pub const VarDef = struct {
        data: ?[*]const u8,
    };
    pub const FuncDef = struct {
        params: []const Self,
        locals: []const Self,
        body: []const Node,
    };

    pub const VarRef = struct {};
    pub const FuncRef = struct {};

    pub const Data = union(enum) {
        var_def: VarDef,
        func_def: FuncDef,
        var_ref: VarRef,
        func_ref: FuncRef,
    };

    name: []const u8,
    ty: Type,
    data: Data,

    fn fromChibi(ally: Allocator, obj: *chibi.Obj) Allocator.Error!Self {
        const name = cStrSlice(obj.name);
        const ty = try Type.fromChibi(ally, obj.ty);

        if (obj.init_data) |init_data| {
            const slice = init_data[0..ty.size];
            std.debug.print("init data for {s}: {any}\n", .{ name, slice });
        }

        const data: Data = data: {
            if (obj.is_definition) {
                if (obj.is_function) {
                    const params = try fromChibiSlice(ally, obj.params);
                    const locals = try fromChibiSlice(ally, obj.locals);
                    const body = try Node.fromChibiSlice(ally, obj.body);

                    break :data Data{ .func_def = .{
                        .params = params,
                        .locals = locals,
                        .body = body,
                    } };
                } else {
                    break :data Data{ .var_def = .{
                        .data = if (obj.init_data) |data| data else null,
                    } };
                }
            } else {
                if (obj.is_function) {
                    break :data Data{ .func_ref = .{} };
                } else {
                    break :data Data{ .var_ref = .{} };
                }
            }

            unreachable;
        };

        return Self{
            .name = name,
            .ty = ty,
            .data = data,
        };
    }

    fn fromChibiAlloc(ally: Allocator, obj: *chibi.Obj) Allocator.Error!*Self {
        const box = try ally.create(Self);
        box.* = try fromChibi(ally, obj);
        return box;
    }

    fn fromChibiSlice(
        ally: Allocator,
        list: ?*chibi.Obj,
    ) Allocator.Error![]const Self {
        var objects = std.ArrayListUnmanaged(Self){};
        var node_iter = iterateChibi(chibi.Obj, list);
        while (node_iter.next()) |chibi_node| {
            const node = try fromChibi(ally, chibi_node);
            try objects.append(ally, node);
        }

        return try objects.toOwnedSlice(ally);
    }

    fn dumpIndented(self: Self, level: u32) void {
        for (0..level * 2) |_| std.debug.print(" ", .{});
        std.debug.print(
            "[{s}] {s}: {}\n",
            .{ @tagName(self.data), self.name, self.ty },
        );

        if (level > 0) return;

        switch (self.data) {
            .var_def => |vd| {
                for (0..level * 2) |_| std.debug.print(" ", .{});
                std.debug.print("data: {?*}\n", .{vd.data});
            },

            .func_def => |fd| {
                for (fd.params) |param| {
                    param.dumpIndented(level + 1);
                }

                for (fd.locals) |local| {
                    local.dumpIndented(level + 1);
                }

                for (fd.body) |node| {
                    node.dumpIndented("body", level + 1);
                }
            },

            else => {},
        }
    }

    fn dump(self: Self) void {
        self.dumpIndented(0);
        std.debug.print("\n", .{});
    }
};

// =============================================================================

var files: std.ArrayList(*chibi.File) = undefined;

pub fn init(ally: Allocator) void {
    chibi.init_macros();
    files = std.ArrayList(*chibi.File).init(ally);
}

pub fn deinit() void {
    files.deinit();
}

fn loadSource(source: Source) Allocator.Error!*chibi.File {
    const file = chibi.new_file(
        source.name.ptr,
        @intCast(files.items.len),
        source.contents.ptr,
    );
    try files.append(file);

    return file;
}

pub const FrontendError = Allocator.Error || error{
    TokenizeError,
    ParseError,
};

/// parses a source file with chibi
/// *treats ally as an arena*
pub fn parse(ally: Allocator, source: Source) FrontendError![]const Object {
    const file = try loadSource(source);

    // tokenize
    const raw_tokens = chibi.tokenize(file) orelse {
        // no tokens, TODO check for errors
        std.debug.print("failed to tokenize {s}\n", .{source.name});
        return FrontendError.TokenizeError;
    };
    const tokens = chibi.preprocess(raw_tokens);

    // parse
    const chibi_program = chibi.parse(tokens) orelse {
        std.debug.print("failed to parse {s}\n", .{source.name});
        return FrontendError.ParseError;
    };

    var objects = std.ArrayListUnmanaged(Object){};
    var trav: ?*chibi.Obj = chibi_program;
    while (trav) |chibi_obj| : (trav = chibi_obj.next) {
        if (!chibi_obj.is_live) continue;

        const object = try Object.fromChibi(ally, chibi_obj);
        try objects.append(ally, object);

        // TODO make this a cli flag?
        if (in_debug) {
            object.dump();
        }
    }

    return try objects.toOwnedSlice(ally);
}
