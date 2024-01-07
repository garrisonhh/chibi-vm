//! entry point and some pipeline functions.

const std = @import("std");
const Allocator = std.mem.Allocator;
const frontend = @import("frontend.zig");
const Source = frontend.Source;

fn interpret(
    backing_ally: Allocator,
    sources: []const Source,
) frontend.FrontendError!void {
    var arena = std.heap.ArenaAllocator.init(backing_ally);
    defer arena.deinit();
    const ally = arena.allocator();

    for (sources) |source| {
        const parsed = try frontend.parse(ally, source);
        _ = parsed;
    }
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
            \\int add(int a, int b) {
            \\  return a + b;
            \\}
            \\
            ,
        },
    };

    try interpret(ally, &sources);

    // TODO testing remove
    const vm = @import("vm.zig");
    var builder = vm.Builder.init(ally);

    try builder.@"export"("add");
    try builder.op(.{ .add = .word });
    try builder.op(.ret);

    const obj = try builder.build();
    defer obj.deinit(ally);

    var so = try vm.link(ally, &.{obj});
    defer so.deinit(ally);

    var env = try vm.Env.init(ally, .{});
    defer env.deinit(ally);

    try env.push(i64, 24);
    try env.push(i64, 18);
    try env.exec(&so, "add");
    const res = try env.pop(i64);

    std.debug.print("res: {}\n", .{res});
}
