const std = @import("std");
const stderr = std.io.getStdErr().writer();
const Allocator = std.mem.Allocator;
const argz = @import("argz.zig");
const sources = @import("sources.zig");
const Lexer = @import("Lexer.zig");

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

/// initialize compiler
fn init() !void {
}

/// deinitialize compiler
fn deinit() void {
    sources.deinit();
}

fn run(ally: Allocator, args: []const Cli.Arg) !void {
    try init();
    defer deinit();

    _ = ally;

    for (args) |arg| {
        switch (arg.id) {
            .source_file => {
                const src = try sources.addPath(arg.data.?);

                var lexer = Lexer.init(src);
                while (try lexer.next()) |token| {
                    std.debug.print("{} `{s}`\n", .{token, token.slice()});
                }
            },
            else => unreachable,
        }
    }
}

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const ally = gpa.allocator();

    const raw_args = try std.process.argsAlloc(ally);
    defer std.process.argsFree(ally, raw_args);

    const res = try cli.parse(ally, raw_args) orelse {
        try cli.usage(raw_args, stderr);
        std.process.exit(1);
    };
    defer res.deinit(ally);

    switch (res.id) {
        .run => try run(ally, res.args),
        else => unreachable,
    }
}
