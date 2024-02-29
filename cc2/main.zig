const std = @import("std");
const stderr = std.io.getStdErr().writer();
const Allocator = std.mem.Allocator;
const argz = @import("argz.zig");
const errors = @import("errors.zig");
const ErrorBuffer = errors.ErrorBuffer;
const sources = @import("sources.zig");
const Source = sources.Source;
const pp = @import("preprocess.zig");

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

fn init() !void {}

fn deinit() void {
    sources.deinit();
}

fn run(ally: Allocator, args: []const Cli.Arg) !void {
    try init();
    defer deinit();

    var eb = ErrorBuffer.init(ally);
    defer eb.deinit();
    var files = std.ArrayList(Source).init(ally);
    defer files.deinit();

    // handle args
    for (args) |arg| {
        switch (arg.id) {
            .source_file => {
                const src = try sources.addPath(arg.data.?);
                try files.append(src);
            },
            else => unreachable,
        }
    }

    // compile
    for (files.items) |src| {
        const tokens = try pp.preprocess(ally, &eb, src) orelse {
            std.debug.assert(eb.hasErrors());

            try eb.display(stderr);
            std.process.exit(1);
        };
        defer ally.free(tokens);

        // TODO parse
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
