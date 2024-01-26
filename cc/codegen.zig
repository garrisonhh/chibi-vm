//! generating vm bytecode from the frontend ast.

const std = @import("std");
const Allocator = std.mem.Allocator;
const in_debug = @import("builtin").mode == .Debug;
const frontend = @import("frontend.zig");
const AstObject = frontend.Object;
const Node = frontend.Node;
const Type = frontend.Type;
const vm = @import("vm");
const Width = vm.Width;
const Op = vm.Op;
const Builder = vm.Builder;
const Segment = vm.Segment;

const Globals = std.StringHashMap(Segment);

const Context = struct {
    const Local = struct {
        offset: i16,
        ty: *const Type,
    };

    const Locals = std.StringHashMapUnmanaged(Local);

    ret_params: u8,
    /// maps locals to metadata
    locals: Locals = .{},
    globals: *const Globals,

    fn init(globals: *const Globals, ret_params: u8) Context {
        return Context{
            .ret_params = ret_params,
            .globals = globals,
        };
    }

    fn deinit(ctx: *Context, ally: Allocator) void {
        ctx.locals.deinit(ally);
    }
};

const UnimplementedError = error{Unimplemented};

fn unimplemented(
    comptime fmt: []const u8,
    args: anytype,
) UnimplementedError!void {
    std.debug.print("unimplemented: " ++ fmt ++ "\n", args);
    return error.Unimplemented;
}

// helpers =====================================================================

fn getIntSignedness(t: Type) std.builtin.Signedness {
    return switch (t.data) {
        .char, .short, .int, .long => |meta| meta.signedness,
        else => unreachable,
    };
}

// lowering ====================================================================

pub const Error = Allocator.Error || Builder.SymbolError ||  UnimplementedError;

/// lower a global var
fn lowerGlobalData(
    b: *Builder,
    globals: *Globals,
    name: []const u8,
    meta: AstObject.Var,
) Error!void {
    const bytes = meta.data orelse return;

    // TODO respect static
    _ = try b.define(name, .exported, .data);
    try b.data(bytes);

    try globals.put(name, .data);
}

/// lower a node as its address (for lvalues, for example)
fn lowerAddr(
    b: *Builder,
    ctx: *const Context,
    node: *const Node,
) Error!void {
    switch (node.data) {
        .assign => |args| {
            try lowerAddr(b, ctx, args[0]);

            const size = args[0].ty.?.size;
            std.debug.assert(size == args[1].ty.?.size);

            if (vm.Width.fromBytesExact(size)) |width| {
                try b.op(.dup);
                try lowerNode(b, ctx, args[1]);
                try b.op(.{ .store = .{
                    .width = width,
                    .offset = 0,
                } });
            } else {
                try lowerAddr(b, ctx, args[1]);
                try b.op(.{ .copy = @intCast(size) });
            }
        },
        .@"var" => |obj| {
            if (ctx.locals.get(obj.name)) |local| {
                try b.op(.{ .local = local.offset });
            } else if (ctx.globals.get(obj.name)) |segment| {
                const lbl = try b.symbol(obj.name);
                const op: Op = switch (segment) {
                    .code => .{ .label = lbl },
                    .data => .{ .data = lbl },
                    .bss => .{ .bss = lbl },
                };
                try b.op(op);
            }
        },
        .member => |access| {
            try lowerAddr(b, ctx, access.obj);
            try b.constant(u32, @as(u32, @intCast(access.member.offset)));
            try b.op(.{ .add = .half });
        },
        .deref => |deref| {
            try lowerNode(b, ctx, deref.obj);
            if (deref.member) |member| {
                try b.constant(u32, @as(u32, @intCast(member.offset)));
                try b.op(.{ .add = .half });
            }
        },
        .cast => |child| {
            // this should always be a noop
            try lowerAddr(b, ctx, child);
        },

        else => {
            try unimplemented("lower addr for {s}", .{@tagName(node.data)});
        },
    }
}

fn lowerLoad(b: *Builder, ty: *const Type) Error!void {
    if (Width.fromBytesFit(ty.size)) |width| {
        try b.op(.{ .load = .{
            .width = width,
            .offset = 0,
        } });
    } else {
        try unimplemented("load large value", .{});
    }
}

/// lower a node addr and then load the result
fn lowerAddrLoad(
    b: *Builder,
    ctx: *const Context,
    node: *const Node,
) Error!void {
    try lowerAddr(b, ctx, node);
    try lowerLoad(b, &node.ty.?);
}

/// lower a statement node or a node being read as a value
fn lowerNode(b: *Builder, ctx: *const Context, node: *const Node) Error!void {
    switch (node.data) {
        .assign,
        .deref,
        .member,
        => try lowerAddrLoad(b, ctx, node),
        .addr => |child| try lowerAddr(b, ctx, child),

        // as far as I can tell, this returns void/undefined
        .null_expr => {
            try b.constant(u8, 0);
        },
        inline .neg, .bitnot => |child, tag| {
            const width = Width.fromBytesExact(node.ty.?.size).?;

            try lowerNode(b, ctx, child);

            const op: Op = switch (tag) {
                .neg => .{ .neg = width },
                .bitnot => .{ .bitcom = width },
                else => unreachable,
            };

            try b.op(op);
        },
        inline .add, .sub, .bitand, .bitor, .bitxor => |args, tag| {
            const width = Width.fromBytesExact(node.ty.?.size).?;

            try lowerNode(b, ctx, args[0]);
            try lowerNode(b, ctx, args[1]);

            try b.op(@unionInit(Op, @tagName(tag), width));
        },
        inline .mul, .div, .mod => |args, tag| {
            const ty = node.ty.?;
            const width = Width.fromBytesExact(ty.size).?;

            try lowerNode(b, ctx, args[0]);
            try lowerNode(b, ctx, args[1]);

            if (ty.isInt()) {
                const sign = switch (node.ty.?.data) {
                    .char, .short, .int, .long => |meta| meta.signedness,
                    else => unreachable,
                };

                const op: Op = switch (sign) {
                    .signed => switch (tag) {
                        .mul => .{ .muli = width },
                        .div => .{ .divi = width },
                        .mod => .{ .modi = width },
                        else => unreachable,
                    },
                    .unsigned => switch (tag) {
                        .mul => .{ .mulu = width },
                        .div => .{ .divu = width },
                        .mod => .{ .modu = width },
                        else => unreachable,
                    },
                };

                try b.op(op);
            } else {
                std.debug.assert(ty.isFloat());
                try unimplemented("mul/div float", .{});
            }
        },
        inline .eq, .ne => |args, tag| {
            const width = Width.fromBytesExact(node.ty.?.size).?;

            try lowerNode(b, ctx, args[0]);
            try lowerNode(b, ctx, args[1]);

            try b.op(@unionInit(Op, @tagName(tag), width));
            // in c, conditions return integers
            try b.op(.{ .extend = .byte });
        },
        inline .expr_stmt, .@"return" => |child, tag| {
            try lowerNode(b, ctx, child);

            const op: Op = switch (comptime tag) {
                .expr_stmt => .drop,
                .@"return" => .{ .ret = ctx.ret_params },
                .deref => .{ .load = .{
                    .width = Width.fromBytesExact(node.ty.?.size).?,
                    .offset = 0,
                } },
                else => unreachable,
            };

            try b.op(op);
        },
        .comma => |args| {
            try lowerNode(b, ctx, args[0]);
            try b.op(.drop);
            try lowerNode(b, ctx, args[1]);
        },
        .block => |nodes| {
            for (nodes) |*child| {
                try lowerNode(b, ctx, child);
            }
        },
        .cast => |child| {
            const into = node.ty.?;
            const from = child.ty.?;

            // pointer decay
            if (into.data == .ptr and from.data == .array) {
                try lowerAddr(b, ctx, child);
                return;
            }

            try lowerNode(b, ctx, child);

            if (into.eql(from)) {
                // no cast required
            } else if (into.data == .void) {
                // value will be dropped
            } else if (into.data == .bool) {
                // bool cast (just compare to zero)
                const w = Width.fromBytesFit(from.size).?;
                try b.op(.{ .eqz = w });
            } else if (from.isInt() and into.isFloat()) {
                // int to float
                return unimplemented("int to float", .{});
            } else if (from.isFloat() and into.isInt()) {
                // float to int
                return unimplemented("float to int", .{});
            } else if (from.size != into.size and
                into.isInt() and from.isInt())
            {
                const from_w = Width.fromBytesExact(from.size).?;

                switch (getIntSignedness(into)) {
                    .unsigned => {
                        if (into.size > from.size) {
                            try b.op(.{ .extend = from_w });
                        }
                    },
                    .signed => switch (getIntSignedness(from)) {
                        .unsigned => {
                            if (into.size > from.size) {
                                try b.op(.{ .extend = from_w });
                            }
                        },
                        .signed => {
                            try b.op(.{ .sign_extend = from_w });

                            const into_w = Width.fromBytesExact(into.size).?;
                            if (into_w != .word) {
                                try b.op(.{ .sign_narrow = into_w });
                            }
                        },
                    },
                }
            }

            // all other casts are noops
        },
        .@"if" => |meta| {
            if (meta.@"else") |@"else"| {
                const else_branch = try b.backref();
                const end = try b.backref();

                try lowerNode(b, ctx, meta.cond);
                try b.op(.{ .jz = .{
                    .width = Width.fromBytesFit(meta.cond.ty.?.size).?,
                    .dest = else_branch,
                } });

                try lowerNode(b, ctx, meta.then);
                try b.op(.{ .jump = end });

                b.resolve(else_branch, .code);
                try lowerNode(b, ctx, @"else");

                b.resolve(end, .code);
            } else {
                const end = try b.backref();

                try lowerNode(b, ctx, meta.cond);
                try b.op(.{ .jz = .{
                    .width = Width.fromBytesFit(meta.cond.ty.?.size).?,
                    .dest = end,
                } });

                try lowerNode(b, ctx, meta.then);
                b.resolve(end, .code);
            }
        },
        .@"for" => |meta| {
            const start = try b.backref();
            const end = try b.backref();

            if (meta.init) |n| {
                try lowerNode(b, ctx, n);
            }

            b.resolve(start, .code);

            if (meta.cond) |cond| {
                try lowerNode(b, ctx, cond);
                try b.op(.{ .jz = .{
                    .width = Width.fromBytesFit(cond.ty.?.size).?,
                    .dest = end,
                } });
            }

            try lowerNode(b, ctx, meta.body);

            if (meta.iter) |iter| {
                try lowerNode(b, ctx, iter);
            }

            try b.op(.{ .jump = start });
            b.resolve(end, .code);
        },
        .do => |meta| {
            const start = try b.label(.code);

            try lowerNode(b, ctx, meta.body);
            try lowerNode(b, ctx, meta.cond);
            try b.op(.{ .jnz = .{
                .width = Width.fromBytesFit(meta.cond.ty.?.size).?,
                .dest = start,
            } });
        },
        .funcall => |fc| {
            for (fc.args) |*arg| {
                try lowerNode(b, ctx, arg);
            }

            try lowerAddr(b, ctx, fc.func);
            try b.op(.call);
        },
        .num => |num| {
            switch (node.ty.?.data) {
                inline else => |_, tag| {
                    const T = switch (tag) {
                        .char => u8,
                        .short => u16,
                        .int => u32,
                        .long => u64,
                        .float => f32,
                        .double, .ldouble => f64,
                        else => unreachable,
                    };

                    const value: T = switch (comptime @typeInfo(T)) {
                        .Int => @intCast(num.int),
                        .Float => @floatCast(num.float),
                        else => unreachable,
                    };

                    try b.constant(T, value);
                },
            }
        },
        .memzero => |obj| {
            if (ctx.locals.get(obj.name)) |local| {
                try b.op(.{ .local = local.offset });
                try b.op(.{ .zero = @intCast(local.ty.size) });
            } else {
                try unimplemented("global namespace", .{});
            }
        },
        .@"var" => |meta| {
            if (ctx.locals.get(meta.name)) |local| {
                try b.op(.{ .local = local.offset });
                try lowerLoad(b, meta.ty);
            } else if (ctx.globals.get(meta.name)) |segment| {
                const lbl = try b.symbol(meta.name);
                const op: Op = switch (segment) {
                    .code => .{ .label = lbl },
                    .data => .{ .data = lbl },
                    .bss => .{ .bss = lbl },
                };
                try b.op(op);
            }
        },

        else => {
            try unimplemented("{}", .{@as(std.meta.Tag(Node.Data), node.data)});
        },
    }
}

fn lowerFunction(
    b: *Builder,
    globals: *Globals,
    name: []const u8,
    ty: Type,
    func: AstObject.Func,
) !void {
    try globals.put(name, .code);

    const ret_params: u8 = @intCast(func.params.len);
    var ctx = Context.init(globals, ret_params);
    defer ctx.deinit(b.ally);

    // params are pushed in order below the stack frame
    const param_fat_size = func.params.len * 8 + vm.Env.Frame.aligned_size;

    var param_offset = -@as(i16, @intCast(param_fat_size));
    for (func.params) |param| {
        try ctx.locals.put(b.ally, param.name, .{
            .offset = param_offset,
            .ty = &param.ty,
        });
        param_offset += 8;
    }

    // locals get the size of their type, aligned as needed
    // NOTE this could be more efficient in the future by sorting by inverse
    // alignment
    var stack_size: u15 = 0;
    for (func.locals) |local| {
        // ignore params
        if (ctx.locals.contains(local.name)) {
            continue;
        } else if (std.mem.eql(u8, local.name, "__alloca_size__")) {
            // ignore a chibi artifact
            continue;
        }

        // reserve stack space for local, ensuring alignment
        stack_size = std.mem.alignForward(
            u15,
            stack_size,
            @as(u15, @intCast(local.ty.alignment)),
        );

        const offset: i16 = stack_size;
        stack_size += @intCast(local.ty.size);

        try ctx.locals.put(b.ally, local.name, .{
            .offset = offset,
            .ty = &local.ty,
        });
    }

    // write code
    // TODO respect static functions with ns vvv
    _ = try b.define(name, .exported, .code);

    try b.op(.{ .enter = stack_size });

    for (func.body) |node| {
        try lowerNode(b, &ctx, &node);
    }

    // handle implicit return for void functions
    if (ty.data.func.returns.data == .void) {
        try b.op(.{ .constant = .{ .byte = .{0} } });
        try b.op(.{ .ret = ctx.ret_params });
    }
}

pub fn lower(ally: Allocator, ast: []const frontend.Object) !vm.Unit {
    var b = Builder.init(ally);
    defer b.deinit();

    var globals = Globals.init(ally);
    defer globals.deinit();

    // ast comes in reverse order
    var iter = std.mem.reverseIterator(ast);
    while (iter.next()) |obj| {
        switch (obj.data) {
            .@"var" => |meta| try lowerGlobalData(&b, &globals, obj.name, meta),
            .func => |func| try lowerFunction(&b, &globals, obj.name, obj.ty, func),
        }
    }

    return try b.build(ally);
}
