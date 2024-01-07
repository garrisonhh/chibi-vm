//! generating vm bytecode from the frontend ast.

const std = @import("std");
const Allocator = std.mem.Allocator;
const frontend = @import("frontend.zig");
const AstObject = frontend.Object;
const Node = frontend.Node;
const Type = frontend.Type;
const vm = @import("vm.zig");
const Builder = vm.Builder;

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

fn lowerNode(b: *Builder, node: *const Node) !void {
    switch (node.data) {
        .null_expr => unreachable,
        inline .add, .sub => |args, tag| {
            const width = typeWidth(node.ty.?).?;

            try lowerNode(b, args[0]);
            try lowerNode(b, args[1]);

            switch (comptime tag) {
                .add => try b.op(.{ .add = width }),
                .sub => try b.op(.{ .sub = width }),
                else => unreachable,
            }
        },
        .@"return" => |child| {
            try lowerNode(b, child);
            try b.op(.ret);
        },
        .block => |nodes| {
            for (nodes) |*child| {
                try lowerNode(b, child);
            }
        },
        .cast => |child| {
            try lowerNode(b, child);

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

        else => {
            try unimplemented("{}", .{@as(std.meta.Tag(Node.Data), node.data)});
        },
    }
}

fn lowerToplevel(b: *Builder, ast: AstObject) !void {
    try b.@"export"(ast.name);

    switch (ast.data) {
        .func_def => |fd| {
            // TODO handle parameters
            for (fd.body) |node| {
                try lowerNode(b, &node);
            }
        },
        else => try unimplemented(
            "{}",
            .{@as(std.meta.Tag(frontend.Object.Data), ast.data)},
        ),
    }
}

pub fn lower(ally: Allocator, ast: []const frontend.Object) !vm.Object {
    var b = Builder.init(ally);
    for (ast) |ast_obj| {
        try lowerToplevel(&b, ast_obj);
    }

    return try b.build();
}
