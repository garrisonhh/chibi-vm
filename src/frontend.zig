//! interfaces with chibi and converts chibi types to more human zig types

const std = @import("std");
const Allocator = std.mem.Allocator;
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

// zig data structures =========================================================

pub const Source = struct {
    name: [:0]const u8,
    contents: [:0]const u8,
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
        array,
        vla,
        @"struct",
        @"union",
    };

    name: ?[]const u8,
    size: usize,
    alignment: u29,
    data: Data,

    fn fromChibi(ally: Allocator, ty: *chibi.Type) Allocator.Error!Self {
        const name: ?[]const u8 = if (ty.name) |name| name: {
            break :name name.loc[0..@intCast(name.len)];
        } else null;

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

            inline else => |type_kind| @unionInit(
                Data,
                @tagName(type_kind),
                {},
            ),
        };

        return Self{
            .name = name,
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

    pub const Number = union(enum) {
        int: u64,
        float: f64,
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
        shl,
        shr,
        eq,
        ne,
        lt,
        le,
        assign: [2]*Node,
        cond,
        comma: [2]*Node,
        member,
        addr,
        deref: *Node,
        not,
        bitnot,
        logand,
        logor,
        @"return": *Node,
        @"if": If,
        @"for",
        do,
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
            .deref,
            .@"return",
            .cast,
            .expr_stmt,
            .stmt_expr,
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

    fn dumpIndented(self: Self, level: u32) void {
        for (0..level * 2) |_| std.debug.print(" ", .{});
        std.debug.print("[{s}] {?}\n", .{ @tagName(self.data), self.ty });

        switch (self.data) {
            inline else => |meta| switch (@TypeOf(meta)) {
                void => {},
                Var => {
                    for (0..(level + 1) * 2) |_| std.debug.print(" ", .{});
                    std.debug.print("{s}: {}\n", .{meta.name, meta.ty});
                },
                If => {
                    meta.cond.dumpIndented(level + 1);
                    meta.then.dumpIndented(level + 1);
                    if (meta.@"else") |@"else"| {
                        @"else".dumpIndented(level + 1);
                    }
                },
                Funcall => {
                    meta.func.dumpIndented(level + 1);
                    for (meta.args) |arg| {
                        arg.dumpIndented(level + 1);
                    }
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
                    meta.dumpIndented(level + 1);
                },
                []const Node, [2]*Node => {
                    for (meta) |child| {
                        child.dumpIndented(level + 1);
                    }
                },
                else => unreachable,
            },
        }
    }

    fn dump(self: Self) void {
        self.dumpIndented(0);
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
                for (0..level * 2) |_| std.debug.print(" ", .{});
                std.debug.print("params:\n", .{});

                for (fd.params) |param| {
                    param.dumpIndented(level + 1);
                }

                for (0..level * 2) |_| std.debug.print(" ", .{});
                std.debug.print("locals:\n", .{});

                for (fd.locals) |local| {
                    local.dumpIndented(level + 1);
                }

                for (0..level * 2) |_| std.debug.print(" ", .{});
                std.debug.print("body:\n", .{});

                for (fd.body) |node| {
                    node.dumpIndented(level + 1);
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

        object.dump();
    }

    return try objects.toOwnedSlice(ally);
}
