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
) !vm.SharedObject {
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
            \\int number(void) {
            \\  return 4;
            \\}
            \\
            ,
        },
    };

    var so = try compile(ally, &sources);
    defer so.deinit(ally);

    var env = try vm.Env.init(ally, .{});
    defer env.deinit(ally);

    try env.exec(&so, "number");
    std.debug.print("output: {}\n", .{try env.pop(i32)});
}
