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
        .root_source_file = b.path("cc/chibi/chibi.zig"),
    });
    const vm = b.addModule("vm", .{
        .root_source_file = b.path("vm/main.zig"),
    });

    // cc
    const cc = b.addExecutable(.{
        .name = "chibi-vm",
        .root_source_file = b.path("cc/main.zig"),
        .target = target,
        .optimize = optimize,
    });

    cc.linkLibC();
    cc.addCSourceFiles(.{
        .files = &chibi.c_sources,
        .flags = chibi.cFlags(optimize),
    });
    cc.root_module.addImport("chibi", chibi_mod);
    cc.root_module.addImport("vm", vm);

    const install_cc = b.addInstallArtifact(cc, .{});
    const install_cc_step = b.step("cc", "Build chibi cc");
    install_cc_step.dependOn(&install_cc.step);

    b.installArtifact(cc);

    // run cc
    const run_cc_cmd = b.addRunArtifact(cc);
    run_cc_cmd.step.dependOn(&install_cc.step);
    if (b.args) |args| run_cc_cmd.addArgs(args);

    const run_cc_step = b.step("run-cc", "Run chibi cc");
    run_cc_step.dependOn(&run_cc_cmd.step);

    // cc2
    const cc2 = b.addExecutable(.{
        .name = "cc2",
        .root_source_file = b.path("cc2/main.zig"),
        .target = target,
        .optimize = optimize,
    });

    cc2.root_module.addImport("vm", vm);

    const install_cc2 = b.addInstallArtifact(cc2, .{});
    const install_cc2_step = b.step("cc2", "Build cc2");
    install_cc2_step.dependOn(&install_cc2.step);

    b.installArtifact(cc2);

    // run cc2
    const run_cc2_cmd = b.addRunArtifact(cc2);
    run_cc2_cmd.step.dependOn(&install_cc2.step);
    if (b.args) |args| run_cc2_cmd.addArgs(args);

    const run_cc2_step = b.step("run-cc2", "Run cc2");
    run_cc2_step.dependOn(&run_cc2_cmd.step);

    // cc2 tests
    const cc2_tests = b.addTest(.{
        .root_source_file = b.path("cc2/main.zig"),
        .target = target,
        .optimize = optimize,
    });

    const run_cc2_tests = b.addRunArtifact(cc2_tests);
    const cc2_test_step = b.step("test-cc2", "Run cc2 tests");
    cc2_test_step.dependOn(&run_cc2_tests.step);

    // vm tests
    const vm_tests = b.addTest(.{
        .root_source_file = b.path("vm/tests/tests.zig"),
        .target = target,
        .optimize = optimize,
    });

    vm_tests.root_module.addImport("vm", vm);

    const run_vm_tests = b.addRunArtifact(vm_tests);
    const vm_test_step = b.step("test-vm", "Run vm tests");
    vm_test_step.dependOn(&run_vm_tests.step);
}
