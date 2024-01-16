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

    fn reset() void {
        env.reset();
    }

    fn push(comptime T: type, value: T) !void {
        try env.push(T, value);
    }

    /// pops and expects value
    fn expect(comptime T: type, expected: T) !void {
        const actual = try env.pop(T);
        try std.testing.expectEqual(expected, actual);
    }

    fn expectStackSize(expected: usize) !void {
        const size = @intFromPtr(env.stack.top) - @intFromPtr(env.stack.mem.ptr);
        if (size != expected) {
            std.debug.print("expected stack size: {} found: {}\n", .{expected, size});
            return error.TestUnexpectedStackSize;
        }
    }

    fn expectEmpty() !void {
        try expectStackSize(0);
    }
};

test "enter" {
    try simple.init();
    defer simple.deinit();

    const cases = [_]u16 {
        0,
        16,
        22,
        std.math.maxInt(u16),
        std.math.maxInt(u16) - 1,
    };

    for (cases) |reserve| {
        var mod = try simple.build(&.{
            Op{ .enter = reserve },
        });
        defer mod.deinit(ally);

        const expected = std.mem.alignForward(usize, reserve, 8);

        try simple.run(&mod);
        try simple.expectStackSize(expected);
        simple.reset();
    }
}

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