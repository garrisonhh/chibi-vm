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
            \\int main(int argc, char **argv) {
            \\  return 0;
            \\}
            \\
            ,
        },
    };

    try interpret(ally, &sources);
}
