const std = @import("std");
const parser = @import("parser.zig");
const mini = @import("mini.zig");

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const ally = gpa.allocator();

    mini.init();
    defer mini.deinit();

    var ast = try parser.parse(ally, "test",
        \\(this (is) (a (test program)) (0 1 2 -3 1.234))
        \\
    );
    defer ast.deinit();

    if (ast.err) |err| {
        std.debug.print("error: {s} at {d}\n", .{ @tagName(err.kind), err.start });
        return;
    }

    for (ast.toplevel.items) |expr| {
        std.debug.print("{}\n", .{expr});
    }
}

// testing =====================================================================

comptime {
    std.testing.refAllDeclsRecursive(@This());
}

// TODO end-to-end testing