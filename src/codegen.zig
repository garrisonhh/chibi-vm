//! generating vm bytecode from the frontend ast.

const std = @import("std");
const Allocator = std.mem.Allocator;
const frontend = @import("frontend.zig");
const AstObject = frontend.Object;
const Node = frontend.Node;
const Type = frontend.Type;
const vm = @import("vm.zig");
const Builder = vm.Builder;

const Local = struct {
    offset: i16,
    size: usize,
};

/// maps locals to metadata
const Locals = std.StringHashMapUnmanaged(Local);

fn unimplemented(comptime fmt: []const u8, args: anytype) !void {
    std.debug.print("unimplemented: " ++ fmt ++ "\n", args);
    return error.Unimplemented;
}

fn typeWidth(t: Type) ?vm.Width {
    return switch (t.size) {
        inline 1, 2, 4, 8 => |sz| vm.Width.fromBytes(sz),
        else => null,
    };
}

fn lowerNode(b: *Builder, locals: *const Locals, node: *const Node) !void {
    switch (node.data) {
        // as far as I can tell, this returns void/undefined
        .null_expr => {
            try b.op(.{ .constant = .{ .byte = .{0} } });
        },
        inline .add, .sub, .mod => |args, tag| {
            const width = typeWidth(node.ty.?).?;

            try lowerNode(b, locals, args[0]);
            try lowerNode(b, locals, args[1]);

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
            const width = typeWidth(ty).?;

            try lowerNode(b, locals, args[0]);
            try lowerNode(b, locals, args[1]);

            if (ty.isInt()) {
                const sign = switch (node.ty.?.data) {
                    .char, .short, .int, .long => |meta| meta.signedness,
                    else => unreachable,
                };

                const op: vm.Op = switch (sign) {
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
            try lowerNode(b, locals, child);

            const op: vm.Op = switch (comptime tag) {
                .expr_stmt => .drop,
                .@"return" => .ret,
                .deref => .{ .load = .{
                    .width = typeWidth(node.ty.?).?,
                    .offset = 0,
                } },
                else => unreachable,
            };

            try b.op(op);
        },
        .comma => |args| {
            try lowerNode(b, locals, args[0]);
            try lowerNode(b, locals, args[1]);
        },
        .block => |nodes| {
            for (nodes) |*child| {
                try lowerNode(b, locals, child);
            }
        },
        .cast => |child| {
            try lowerNode(b, locals, child);

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
            const constant: vm.Op.Constant = switch (node.ty.?.data) {
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

                    const width = comptime vm.Width.fromBytes(@sizeOf(T));
                    const arr: [width.bytes()]u8 = @bitCast(value);
                    break :c @unionInit(vm.Op.Constant, @tagName(width), arr);
                },
            };

            try b.op(.{ .constant = constant });
        },
        .@"var" => |obj| {
            if (locals.get(obj.name)) |meta| {
                switch (meta.size) {
                    1, 2, 4, 8 => {
                        const width = vm.Width.fromBytes(@intCast(meta.size));
                        try b.op(.{ .get_local = .{
                            .width = width,
                            .offset = meta.offset,
                        } });
                    },
                    else => {
                        try unimplemented("big/weird width locals", .{});
                    },
                }
            } else {
                try unimplemented("global namespace", .{});
            }
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
    // figure out stack
    var locals = Locals{};
    defer locals.deinit(b.ally);

    for (func.params, 0..) |param, i| {
        // this has to be in reverse order because params are pushed in order
        const offset = @as(i16, @intCast(func.params.len - i)) * -8;
        try locals.put(b.ally, param.name, Local{
            .offset = offset,
            .size = param.ty.size,
        });
    }

    var stack_size: usize = 0;
    for (func.locals) |local| {
        // ignore params
        if (locals.contains(local.name)) continue;

        // reserve stack space for local
        stack_size = std.mem.alignForward(usize, stack_size, local.ty.alignment);
        const offset: i16 = @intCast(stack_size);
        stack_size += local.ty.size;

        try locals.put(b.ally, local.name, Local{
            .offset = offset,
            .size = local.ty.size,
        });
    }

    // TODO remove
    std.debug.print("[allocated {s} locals]\n", .{name});
    for (func.locals) |local| {
        const meta = locals.get(local.name).?;
        std.debug.print("{s} -> {} bytes @ {}\n", .{ local.name, meta.size, meta.offset });
    }
    std.debug.print("\n", .{});

    // write code
    try b.@"export"(name);
    try b.op(.{ .enter = @intCast(stack_size) });

    for (func.body) |node| {
        try lowerNode(b, &locals, &node);
    }

    // handle implicit return for void functions
    if (ty.data.func.returns.data == .void) {
        try b.op(.{ .constant = .{ .byte = .{0} } });
        try b.op(.ret);
    }
}

pub fn lower(ally: Allocator, ast: []const frontend.Object) !vm.Object {
    var b = Builder.init(ally);
    for (ast) |it| {
        try lowerFunction(&b, it.name, it.ty, it.data.func_def);
    }

    return try b.build();
}
