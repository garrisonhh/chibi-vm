const std = @import("std");

const chibi = struct {
    const c_sources = [_][]const u8{
        "cc/chibi/codegen.c",
        "cc/chibi/hashmap.c",
        "cc/chibi/main.c",
        "cc/chibi/parse.c",
        "cc/chibi/preprocess.c",
        "cc/chibi/strings.c",
        "cc/chibi/tokenize.c",
        "cc/chibi/type.c",
        "cc/chibi/unicode.c",
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
        .source_file = .{ .path = "cc/chibi/chibi.zig" },
    });
    const vm = b.addModule("vm", .{
        .source_file = .{ .path = "vm/main.zig" },
    });

    // cc
    const cc = b.addExecutable(.{
        .name = "chibi-vm",
        .root_source_file = .{ .path = "cc/main.zig" },
        .target = target,
        .optimize = optimize,
    });

    cc.linkLibC();
    cc.addCSourceFiles(&chibi.c_sources, chibi.cFlags(optimize));
    cc.addModule("chibi", chibi_mod);
    cc.addModule("vm", vm);

    b.installArtifact(cc);

    // run cc
    const run_cc_cmd = b.addRunArtifact(cc);
    run_cc_cmd.step.dependOn(b.getInstallStep());
    if (b.args) |args| run_cc_cmd.addArgs(args);

    const run_cc_step = b.step("run-cc", "Run chibi cc");
    run_cc_step.dependOn(&run_cc_cmd.step);

    // cc2
    const cc2 = b.addExecutable(.{
        .name = "cc2",
        .root_source_file = .{ .path = "cc2/main.zig" },
        .target = target,
        .optimize = optimize,
    });

    cc2.addModule("vm", vm);

    b.installArtifact(cc2);

    // run cc2
    const run_cc2_cmd = b.addRunArtifact(cc2);
    run_cc2_cmd.step.dependOn(b.getInstallStep());
    if (b.args) |args| run_cc2_cmd.addArgs(args);

    const run_cc2_step = b.step("run-cc2", "Run cc2");
    run_cc2_step.dependOn(&run_cc2_cmd.step);

    // vm tests
    const unit_tests = b.addTest(.{
        .root_source_file = .{ .path = "vm/tests/tests.zig" },
        .target = target,
        .optimize = optimize,
    });

    unit_tests.addModule("vm", vm);

    const run_unit_tests = b.addRunArtifact(unit_tests);

    const test_step = b.step("test", "Run vm tests");
    test_step.dependOn(&run_unit_tests.step);
}
