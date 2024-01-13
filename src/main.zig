//! entry point and some pipeline functions.

const std = @import("std");
const Allocator = std.mem.Allocator;
const frontend = @import("frontend.zig");
const Source = frontend.Source;
const codegen = @import("codegen.zig");
const vm = @import("vm.zig");

// enable all tests
comptime {
    std.testing.refAllDeclsRecursive(@This());
}

fn compile(
    backing_ally: Allocator,
    sources: []const Source,
) !vm.Module {
    var arena = std.heap.ArenaAllocator.init(backing_ally);
    defer arena.deinit();
    const ally = arena.allocator();

    var objects = std.ArrayListUnmanaged(vm.Object){};

    for (sources) |source| {
        const ast = try frontend.parse(ally, source);
        const object = try codegen.lower(ally, ast);
        try objects.append(ally, object);
    }

    return try vm.link(backing_ally, objects.items);
}

pub fn main() !void {
    // init stuff
    var gpa = std.heap.GeneralPurposeAllocator(.{}){
        // may as well use the same allocator as chibi
        .backing_allocator = std.heap.c_allocator,
    };
    defer _ = gpa.deinit();
    const ally = gpa.allocator();

    frontend.init(ally);
    defer frontend.deinit();

    // interpret sources
    var sources = [_]Source{
        .{
            .name = "test_file",
            .contents =
            \\int fib(int x) {
            \\  if (x == 0) {
            \\    return 1;
            \\  } else if (x == 1) {
            \\    return 1;
            \\  }
            \\
            \\  return fib(x - 1) + fib(x - 2);
            \\}
            \\
            \\int strlen(const char *str) {
            \\  if (*str) {
            \\    return 1 + strlen(str + 1);
            \\  }
            \\  return 0;
            \\}
            \\
        },
    };

    var mod = try compile(ally, &sources);
    defer mod.deinit(ally);

    var env = try vm.Env.init(ally, .{});
    defer env.deinit(ally);

    for (0..10) |n| {
        try env.push(i32, @as(i32, @intCast(n)));
        try env.exec(&mod, "fib");
        std.debug.print("fib({}) = {}\n", .{n, try env.pop(i32)});
    }

    const CStr = *const [*:0]u8;
    const str = "hello, world!";
    try env.push(CStr, @as(CStr, @alignCast(@ptrCast(str))));
    try env.exec(&mod, "strlen");
    std.debug.print("strlen(\"{s}\") = {}\n", .{str, try env.pop(i32)});
}
