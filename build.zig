const std = @import("std");

const chibi = struct {
    const c_include_path = "src/chibi/";

    const c_sources = [_][]const u8{
        "src/chibi/codegen.c",
        "src/chibi/hashmap.c",
        "src/chibi/main.c",
        "src/chibi/parse.c",
        "src/chibi/preprocess.c",
        "src/chibi/strings.c",
        "src/chibi/tokenize.c",
        "src/chibi/type.c",
        "src/chibi/unicode.c",
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

    // exe
    const exe = b.addExecutable(.{
        .name = "chibi-vm",
        .root_source_file = .{ .path = "src/main.zig" },
        .target = target,
        .optimize = optimize,
    });

    exe.linkLibC();
    exe.addIncludePath(.{ .path = chibi.c_include_path });
    exe.addCSourceFiles(&chibi.c_sources, chibi.cFlags(optimize));

    b.installArtifact(exe);

    // run
    const run_cmd = b.addRunArtifact(exe);
    run_cmd.step.dependOn(b.getInstallStep());
    if (b.args) |args| run_cmd.addArgs(args);

    const run_step = b.step("run", "Run the app");
    run_step.dependOn(&run_cmd.step);

    // tests
    const unit_tests = b.addTest(.{
        .root_source_file = .{ .path = "src/main.zig" },
        .target = target,
        .optimize = optimize,
    });

    unit_tests.linkLibC();
    unit_tests.addIncludePath(.{ .path = chibi.c_include_path });
    unit_tests.addCSourceFiles(&chibi.c_sources, chibi.cFlags(optimize));

    const run_unit_tests = b.addRunArtifact(unit_tests);

    const test_step = b.step("test", "Run unit tests");
    test_step.dependOn(&run_unit_tests.step);
}
