const std = @import("std");
const ops = @import("ops.zig");
const Width = ops.Width;
const Op = ops.Op;
const objects = @import("objects.zig");
const Builder = objects.Builder;
const Module = objects.Module;
const Env = @import("Env.zig");

const ally = std.testing.allocator;

/// namespace for shared behavior for simple tests
const simple = struct {
    const func_name = "simple_test";
    var env: Env = undefined;

    fn init() !void {
        env = try Env.init(ally, .{});
    }

    fn deinit() void {
        env.deinit(ally);
    }

    fn build(code: []const Op) !Module {
        var builder = Builder.init(ally);
        defer builder.deinit();

        const lbl = try builder.label();
        _ = try builder.global(func_name, .global, lbl);

        for (code) |o| {
            try builder.op(o);
        }

        const obj = try builder.build();
        defer obj.deinit(ally);

        const mod = try objects.link(ally, &.{obj});

        return mod;
    }

    fn int(comptime w: Width) type {
        return std.meta.Int(.signed, @as(u16, w.bytes()) * 8);
    }

    fn uint(comptime w: Width) type {
        return std.meta.Int(.unsigned, @as(u16, w.bytes()) * 8);
    }

    fn run(mod: *const Module) !void {
        const offset = mod.exports.get(func_name).?;
        try env.execBytecode(mod.code, offset);
    }

    fn push(comptime T: type, value: T) !void {
        try env.push(T, value);
    }

    /// pops and expects value
    fn expect(comptime T: type, expected: T) !void {
        const actual = try env.pop(T);
        try std.testing.expectEqual(expected, actual);
    }

    fn expectEmpty() !void {
        if (env.stack.base != env.stack.mem.ptr) {
            return error.StackNotEmpty;
        }
    }
};

test "add" {
    try simple.init();
    defer simple.deinit();

    inline for (comptime std.enums.values(Width)) |w| {
        var mod = try simple.build(&.{
            Op{ .add = w },
        });
        defer mod.deinit(ally);

        const I = simple.int(w);
        const cases = [_][3]I{
            .{0, 0, 0},
            .{1, 2, 3},
            .{std.math.maxInt(I), 1, std.math.minInt(I)},
        };

        for (cases) |case| {
            try simple.push(I, case[0]);
            try simple.push(I, case[1]);
            try simple.run(&mod);
            try simple.expect(I, case[2]);
            try simple.expectEmpty();
        }
    }
}