//! generating vm bytecode from the frontend ast.

const std = @import("std");
const Allocator = std.mem.Allocator;
const frontend = @import("frontend.zig");
const AstObject = frontend.Object;
const Node = frontend.Node;
const Type = frontend.Type;
const vm = @import("vm.zig");
const Width = vm.Width;
const Op = vm.Op;
const Builder = vm.Builder;

const Context = struct {
    const Local = struct {
        offset: i16,
        ty: *const Type,
    };
    const Locals = std.StringHashMapUnmanaged(Local);

    ret_params: u8,
    /// maps locals to metadata
    locals: Locals = .{},

    fn deinit(ctx: *Context, ally: Allocator) void {
        ctx.locals.deinit(ally);
    }
};

fn unimplemented(comptime fmt: []const u8, args: anytype) !void {
    std.debug.print("unimplemented: " ++ fmt ++ "\n", args);
    return error.Unimplemented;
}

fn lowerNode(b: *Builder, ctx: *const Context, node: *const Node) !void {
    switch (node.data) {
        // as far as I can tell, this returns void/undefined
        .null_expr => {
            try b.op(.{ .constant = .{ .byte = .{0} } });
        },
        inline .add, .sub, .mod => |args, tag| {
            const width = Width.fromBytesExact(node.ty.?.size).?;

            try lowerNode(b, ctx, args[0]);
            try lowerNode(b, ctx, args[1]);

            // TODO floats

            switch (comptime tag) {
                .add => try b.op(.{ .add = width }),
                .sub => try b.op(.{ .sub = width }),
                .mod => try b.op(.{ .mod = width }),
                else => unreachable,
            }
        },
        inline .mul, .div => |args, tag| {
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
                        else => unreachable,
                    },
                    .unsigned => switch (tag) {
                        .mul => .{ .mulu = width },
                        .div => .{ .divu = width },
                        else => unreachable,
                    },
                };

                try b.op(op);
            } else {
                std.debug.assert(ty.isFloat());
                try unimplemented("mul/div float", .{});
            }
        },
        inline .deref, .expr_stmt, .@"return" => |child, tag| {
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
            try lowerNode(b, ctx, args[1]);
        },
        .block => |nodes| {
            for (nodes) |*child| {
                try lowerNode(b, ctx, child);
            }
        },
        .cast => |child| {
            try lowerNode(b, ctx, child);

            const into = node.ty.?;
            const from = child.ty.?;

            if (into.data == .void) {
                // value will be ignored
            } else if (into.data == .bool) {
                // bool cast (just compare to zero)
                return unimplemented("cast to bool", .{});
            } else if (from.isInt() and into.isFloat()) {
                // int to float
                return unimplemented("int to float", .{});
            } else if (from.isFloat() and into.isInt()) {
                // float to int
                return unimplemented("float to int", .{});
            } else if (from.size != into.size and
                into.isInt() and from.isInt())
            {
                // intcast
                return unimplemented("intcast", .{});
            }

            // all other casts are noops
        },
        .num => |num| {
            const constant: Op.Constant = switch (node.ty.?.data) {
                inline else => |_, tag| c: {
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

                    const width = comptime Width.fromBytesExact(@sizeOf(T)).?;
                    const arr: [width.bytes()]u8 = @bitCast(value);
                    break :c @unionInit(Op.Constant, @tagName(width), arr);
                },
            };

            try b.op(.{ .constant = constant });
        },
        .@"var" => |obj| {
            if (ctx.locals.get(obj.name)) |local| {
                try b.op(.{ .local = local.offset });
                try b.op(.{ .load = .{
                    .width = Width.fromBytesFit(local.ty.size).?,
                    .offset = 0,
                } });
            } else {
                try unimplemented("global namespace", .{});
            }
        },
        .memzero => |obj| {
            try unimplemented("TODO memzero {s}", .{obj.name});
        },

        else => {
            try unimplemented("{}", .{@as(std.meta.Tag(Node.Data), node.data)});
        },
    }
}

fn lowerFunction(
    b: *Builder,
    name: []const u8,
    ty: Type,
    func: AstObject.FuncDef,
) !void {
    var ctx = Context{
        .ret_params = @intCast(func.params.len),
    };
    defer ctx.deinit(b.ally);

    // params are pushed in order below the stack frame
    const param_fat_size = func.params.len * 8 + vm.Env.frame_size;

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
    try b.@"export"(name);
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

pub fn lower(ally: Allocator, ast: []const frontend.Object) !vm.Object {
    var b = Builder.init(ally);
    for (ast) |it| {
        try lowerFunction(&b, it.name, it.ty, it.data.func_def);
    }

    return try b.build();
}
