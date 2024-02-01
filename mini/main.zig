const std = @import("std");
const parser = @import("parser.zig");

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const ally = gpa.allocator();

    var ast = try parser.parse(ally, "test",
        \\(this (is) (a (test program)))
        \\
    );
    defer ast.deinit();

    if (ast.err) |err| {
        std.debug.print("error: {s} at {d}\n", .{@tagName(err.kind), err.start});
        return;
    }

    for (ast.toplevel.items) |expr| {
        std.debug.print("{}\n", .{expr});
    }
}