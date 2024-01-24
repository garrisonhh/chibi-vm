const std = @import("std");

const chibi = struct {
    const c_sources = [_][]const u8{
        "chibi/codegen.c",
        "chibi/hashmap.c",
        "chibi/main.c",
        "chibi/parse.c",
        "chibi/preprocess.c",
        "chibi/strings.c",
        "chibi/tokenize.c",
        "chibi/type.c",
        "chibi/unicode.c",
    };

    fn cFlags(optimize: std.builtin.Mode) []const []const u8 {
        // taken from the original makefile
        const common_c_flags = [_][]const u8{
            "-std=c11",
            "-fno-common",
            "-Wall",
            "-Wno-switch",
        };

        return switch (optimize) {
            .Debug => comptime common_c_flags ++ &[_][]const u8{"-ggdb"},
            else => comptime common_c_flags ++ &[_][]const u8{"-O2"},
        };
    }
};

pub fn build(b: *std.Build) void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});

    // modules
    const chibi_mod = b.addModule("chibi", .{
        .source_file = .{ .path = "chibi/chibi.zig" },
    });
    const vm = b.addModule("vm", .{
        .source_file = .{ .path = "vm/main.zig" },
    });

    // exe
    const exe = b.addExecutable(.{
        .name = "chibi-vm",
        .root_source_file = .{ .path = "cc/main.zig" },
        .target = target,
        .optimize = optimize,
    });

    exe.linkLibC();
    exe.addCSourceFiles(&chibi.c_sources, chibi.cFlags(optimize));
    exe.addModule("chibi", chibi_mod);
    exe.addModule("vm", vm);

    b.installArtifact(exe);

    // run
    const run_cmd = b.addRunArtifact(exe);
    run_cmd.step.dependOn(b.getInstallStep());
    if (b.args) |args| run_cmd.addArgs(args);

    const run_step = b.step("run", "Run the app");
    run_step.dependOn(&run_cmd.step);

    // vm tests
    const unit_tests = b.addTest(.{
        .root_source_file = .{ .path = "vm/main.zig" },
        .target = target,
        .optimize = optimize,
    });

    unit_tests.linkLibC();
    unit_tests.addCSourceFiles(&chibi.c_sources, chibi.cFlags(optimize));

    const run_unit_tests = b.addRunArtifact(unit_tests);

    const test_step = b.step("test", "Run vm tests");
    test_step.dependOn(&run_unit_tests.step);
}
