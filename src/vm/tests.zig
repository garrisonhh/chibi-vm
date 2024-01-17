const std = @import("std");
const ops = @import("ops.zig");
const Width = ops.Width;
const Op = ops.Op;
const Opcode = ops.Opcode;
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

    fn pop(comptime T: type) !T {
        return try env.pop(T);
    }

    /// returns stack size in bytes
    fn getStackSize() usize {
        const size = @intFromPtr(env.stack.top) - @intFromPtr(env.stack.mem.ptr);
        return size;
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

        const stack_size = simple.getStackSize();
        if (stack_size != expected) {
            std.debug.print("stack size expected: {} actual {}\n", .{
                expected,
                stack_size,
            });
            return error.TestWrongStackSize;
        }

        simple.reset();
    }
}

/// functions for sized, binary ops which encode the expected behavior
const sized_binary_verifiers = struct {
    fn add(comptime T: type, a: T, b: T) T {
        return a +% b;
    }

    fn sub(comptime T: type, a: T, b: T) T {
        return a -% b;
    }

    fn mulu(comptime T: type, a: T, b: T) T {
        const U = std.meta.Int(.unsigned, @bitSizeOf(T));
        return @bitCast(@as(U, @bitCast(a)) *% @as(U, @bitCast(b)));
    }

    fn muli(comptime T: type, a: T, b: T) T {
        const I = std.meta.Int(.signed, @bitSizeOf(T));
        return @bitCast(@as(I, @bitCast(a)) *% @as(I, @bitCast(b)));
    }

    fn divu(comptime T: type, a: T, b: T) T {
        const U = std.meta.Int(.unsigned, @bitSizeOf(T));
        return @bitCast(@as(U, @bitCast(a)) / @as(U, @bitCast(b)));
    }

    fn divi(comptime T: type, a: T, b: T) T {
        const I = std.meta.Int(.signed, @bitSizeOf(T));
        return @bitCast(@as(I, @bitCast(a)) / @as(I, @bitCast(b)));
    }

    fn mod(comptime T: type, a: T, b: T) T {
        return a % b;
    }

    fn bitand(comptime T: type, a: T, b: T) T {
        return a & b;
    }

    fn bitor(comptime T: type, a: T, b: T) T {
        return a | b;
    }

    fn bitxor(comptime T: type, a: T, b: T) T {
        return a ^ b;
    }

    fn eq(comptime T: type, a: T, b: T) T {
        return a == b;
    }

    fn ne(comptime T: type, a: T, b: T) T {
        return a != b;
    }
};

test "sized-binary-ops" {
    try simple.init();
    defer simple.deinit();

    inline for (comptime std.enums.values(Width)) |w| {
        const I = simple.int(w);

        // generate cases
        var cases = std.ArrayList([2]I).init(ally);
        defer cases.deinit();

        const max_int = std.math.maxInt(I);
        const min_int = std.math.minInt(I);

        // common edge cases
        try cases.append(.{0, 0});
        try cases.append(.{1, 1});
        try cases.append(.{-1, -1});
        try cases.append(.{max_int, 1});
        try cases.append(.{1, max_int});
        try cases.append(.{min_int, 1});
        try cases.append(.{1, max_int});
        try cases.append(.{max_int, min_int});
        try cases.append(.{min_int, max_int});
        try cases.append(.{max_int, max_int});
        try cases.append(.{min_int, min_int});

        var prng = std.rand.DefaultPrng.init(0);
        const random_cases = 1000;
        for (0..random_cases) |_| {
            try cases.append(.{
                prng.random().int(I),
                prng.random().int(I),
            });
        }

        // run case on each verifier
        const verifiers = @typeInfo(sized_binary_verifiers).Struct.decls;
        inline for (verifiers) |name| {
            std.debug.assert(std.meta.stringToEnum(Opcode, name) != null);
            const func = @field(sized_binary_verifiers, name);

            var mod = try simple.build(&.{
                @unionInit(Op, name, w),
            });
            defer mod.deinit(ally);

            for (cases.items) |case| {
                const lhs = case[0];
                const rhs = case[1];

                try simple.push(I, lhs);
                try simple.push(I, rhs);
                try simple.run(&mod);

                const actual = try simple.pop(I);
                const expected = func(I, );

                if (actual != expected) {
                    std.debug.print(
                        \\case:     {s}({}, {})
                        \\expected: {}
                        \\actual:   {}
                        \\
                    ,
                        .{name, lhs, rhs, expected, actual},
                    );
                    return error.TestUnexpectedResult;
                }

                const stack_size = simple.getStackSize();
                if (stack_size != 0) {
                    std.debug.print(
                        \\case:       {s}({}, {})
                        \\stack left: {}
                        \\
                    ,
                        .{name, lhs, rhs, stack_size},
                    );
                    return error.TestStackNotEmpty;
                }
            }
        }
    }
}