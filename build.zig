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

    const cc_install = b.addInstallArtifact(cc, .{});
    const cc_step = b.step("cc", "Build the C interpreter");
    cc_step.dependOn(&cc_install.step);

    const run_cc = b.addRunArtifact(cc);
    run_cc.step.dependOn(&cc_install.step);
    if (b.args) |args| run_cc.addArgs(args);

    const run_cc_step = b.step("run-cc", "Run the C interpreter");
    run_cc_step.dependOn(&run_cc.step);

    // mini
    const mini = b.addExecutable(.{
        .name = "mini",
        .root_source_file = .{ .path = "mini/main.zig" },
        .target = target,
        .optimize = optimize,
    });

    mini.addModule("vm", vm);

    const mini_install = b.addInstallArtifact(mini, .{});
    const mini_step = b.step("mini", "Build mini");
    mini_step.dependOn(&mini_install.step);

    const run_mini = b.addRunArtifact(mini);
    run_mini.step.dependOn(&mini_install.step);
    if (b.args) |args| run_mini.addArgs(args);

    const run_mini_step = b.step("run-mini", "Run mini");
    run_mini_step.dependOn(&run_mini.step);

    // build everything with default zig build step
    b.installArtifact(cc);
    b.installArtifact(mini);

    // vm tests
    const vm_tests = b.addTest(.{
        .root_source_file = .{ .path = "vm/tests/tests.zig" },
        .target = target,
        .optimize = optimize,
    });

    vm_tests.addModule("vm", vm);

    const run_vm_tests = b.addRunArtifact(vm_tests);

    const vm_test_step = b.step("test-vm", "Run vm tests");
    vm_test_step.dependOn(&run_vm_tests.step);

    // mini tests
    const mini_tests = b.addTest(.{
        .root_source_file = .{ .path = "mini/main.zig" },
        .target = target,
        .optimize = optimize,
    });

    mini_tests.addModule("vm", vm);

    const run_mini_tests = b.addRunArtifact(vm_tests);

    const mini_test_step = b.step("test-mini", "Run mini tests");
    mini_test_step.dependOn(&run_mini_tests.step);
}
