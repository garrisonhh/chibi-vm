const std = @import("std");
const stderr = std.io.getStdErr().writer();
const Allocator = std.mem.Allocator;
const argz = @import("argz.zig");

const CliOptions = enum {
    run,
    source_file,
};
const Cli = argz.Interface(CliOptions);

const cli = Cli.command("cc", "a c interpreter.", .{ .subcommands = &.{
    Cli.command("run", "interpret a program", .{ .args = .{
        .id = .run,
        .args = &.{
            Cli.arg(
                .source_file,
                "source files to be interpreted",
                .{ .positional = .{ .metavar = "sources", .count = .many } },
            ),
        },
    } }),
} });

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const ally = gpa.allocator();

    const raw_args = try std.process.argsAlloc(ally);
    defer std.process.argsFree(ally, raw_args);

    const args = try cli.parse(ally, raw_args) orelse {
        try cli.usage(raw_args, stderr);
        std.process.exit(1);
    };
    defer args.deinit(ally);
}
