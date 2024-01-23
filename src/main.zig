//! entry point and some pipeline functions.

const std = @import("std");
const stderr = std.io.getStdErr().writer();
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

// subcommands =================================================================

fn run(ally: Allocator, source_paths: []const [:0]const u8) !void {
    if (source_paths.len == 0) {
        usage(.run, "no source paths provided");
    } else if (source_paths.len > 1) {
        usage(.run, "more than one input file is not currently supported");
    }

    var arena = std.heap.ArenaAllocator.init(ally);
    defer arena.deinit();
    const arena_ally = arena.allocator();

    var sources = try arena_ally.alloc(Source, source_paths.len);

    const cwd = std.fs.cwd();
    for (source_paths, sources) |path, *source| {
        const contents = try cwd.readFileAllocOptions(
            arena_ally,
            path,
            std.math.maxInt(usize),
            null,
            @alignOf(u8),
            0
        );

        source.* = Source{
            .name = path,
            .contents = contents,
        };
    }

    var mod = try compile(ally, sources);
    defer mod.deinit(ally);

    var env = try vm.Env.init(ally, .{});
    defer env.deinit(ally);

    // TODO take argc, argv from cli
    try env.push(i32, 0);
    try env.push(?[*]const [*:0]const u8, null);

    env.exec(&mod, "main") catch |e| switch (e) {
        vm.Env.ExecError.NoSuchFunction => {
            try stderr.print("error: no main function found\n", .{});
            std.process.exit(1);
        },
        else => return e,
    };

    const exit_code = try env.pop(i32);
    if (exit_code > 0) {
        try stderr.print("program exited unsuccessfully: {}\n", .{exit_code});
        const exit_code_byte = std.math.cast(u8, exit_code) orelse 1;
        std.process.exit(exit_code_byte);
    }
}

// =============================================================================

const Args = union(enum) {
    const Subcommand = std.meta.Tag(@This());

    const Run = struct {
        source_paths: []const [:0]const u8,
    };

    run: Run,

    fn deinit(args: Args, ally: Allocator) void {
        switch (args) {
            .run => |r| ally.free(r.source_paths),
        }
    }

    fn dispatch(args: Args, ally: Allocator) !void {
        switch (args) {
            .run => |r| try run(ally, r.source_paths),
        }
    }
};

fn usagePrint(comptime fmt: []const u8, args: anytype) void {
    stderr.print(fmt, args) catch {};
}

fn usage(choice: ?Args.Subcommand, msg: []const u8) noreturn {
    var args = std.process.args();
    const exe = args.next().?;

    usagePrint("error: {s}\n\n", .{msg});

    const SubUsage = struct {
        interface: []const u8,
        help: []const u8,
    };

    const sub_usages = std.EnumArray(Args.Subcommand, SubUsage).init(.{
        .run = .{
            .interface = "[sources...]",
            .help = "compile and execute C sources.",
        },
    });

    if (choice) |sub| {
        const meta = sub_usages.get(sub);
        usagePrint("usage: {s} {s} {s}\n\n", .{exe, @tagName(sub), meta.interface});
        usagePrint("{s}\n", .{meta.help});
    } else {
        usagePrint(
            \\usage: {s} <subcommand>
            \\
            \\a C interpreter built with chibi-cc.
            \\
            \\subcommands:
            \\
            ,
            .{exe},
        );

        for (std.enums.values(Args.Subcommand)) |sub| {
            const meta = sub_usages.get(sub);
            usagePrint("  {s}\t{s}\n", .{@tagName(sub), meta.help});
        }
    }

    usagePrint("\n", .{});
    std.process.exit(1);
}

fn parseArgs(ally: Allocator) !Args {
    var args = std.process.args();
    _ = args.next().?; // discard exe

    // get subcommand
    const sub_str = args.next() orelse {
        usage(null, "no subcommand provided");
    };
    const sub = std.meta.stringToEnum(Args.Subcommand, sub_str) orelse {
        usage(null, "invalid subcommand");
    };

    return switch (sub) {
        .run => run: {
            var source_paths = std.ArrayList([:0]const u8).init(ally);
            defer source_paths.deinit();

            while (args.next()) |arg| {
                try source_paths.append(arg);
            }

            break :run Args{ .run = .{
                .source_paths = try source_paths.toOwnedSlice(),
            } };
        }
    };
}

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){
        // may as well use the same allocator as chibi
        .backing_allocator = std.heap.c_allocator,
    };
    defer _ = gpa.deinit();
    const ally = gpa.allocator();

    frontend.init(ally);
    defer frontend.deinit();

    const args = try parseArgs(ally);
    defer args.deinit(ally);

    try args.dispatch(ally);
}
