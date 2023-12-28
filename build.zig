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

    // taken from the original makefile
    const c_flags = [_][]const u8{
        "-std=c11",
        "-fno-common",
        "-Wall",
        "-Wno-switch",
        "-ggdb", // TODO remove in release mode
    };
};

pub fn build(b: *std.Build) void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});

    // exe
    const exe = b.addExecutable(.{
        .name = "chibi-fluent-vm",
        .root_source_file = .{ .path = "src/main.zig" },
        .target = target,
        .optimize = optimize,
    });

    exe.linkLibC();
    exe.addIncludePath(.{ .path = chibi.c_include_path });
    exe.addCSourceFiles(&chibi.c_sources, &chibi.c_flags);

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

    const run_unit_tests = b.addRunArtifact(unit_tests);

    const test_step = b.step("test", "Run unit tests");
    test_step.dependOn(&run_unit_tests.step);
}
