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

// manually tweaked tests ======================================================

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

// generated tests =============================================================

const Prng = std.rand.DefaultPrng;
const random_seed = 0;
const num_random_cases = 1000;

fn generateRandom(prng: *Prng, comptime T: type) T {
    return switch (@typeInfo(T)) {
        .Bool => prng.random().boolean(),
        .Int => prng.random().int(T),
        else => @compileError("generate random for type: " ++ @typeName(T)),
    };
}

/// generates a whole bunch of random cases for a type, edge cases and all
fn generateCases(comptime N: comptime_int, comptime T: type) ![]const [N]T {
    var cases = std.ArrayList([N]T).init(ally);
    defer cases.deinit();

    // add common edge cases for types where this is useful
    switch (@typeInfo(T)) {
        .Int => {
            const max_int = std.math.maxInt(T);
            const min_int = std.math.minInt(T);
            const edges = [_]T{0, 1, max_int, min_int};

            // generate all N-length combinations of edges
            var indices: [N]usize = undefined;
            @memset(&indices, 0);

            combos: while (true) {
                var combo: [N]T = undefined;
                for (&combo, indices) |*slot, i| {
                    slot.* = edges[i];
                }

                try cases.append(combo);

                var i: usize = 0;
                while (true) {
                    indices[i] += 1;
                    if (indices[i] < edges.len) break;

                    indices[i] = 0;
                    i += 1;
                    if (i >= N) break :combos;
                }
            }
        },
        else => {},
    }

    // add random cases
    var prng = Prng.init(random_seed);
    for (0..num_random_cases) |_| {
        var arr: [N]T = undefined;
        for (&arr) |*slot| {
            slot.* = generateRandom(&prng, T);
        }

        try cases.append(arr);
    }

    return try cases.toOwnedSlice();
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
        const cases = try generateCases(2, I);
        defer ally.free(cases);

        // run case on each verifier
        const verifiers = @typeInfo(sized_binary_verifiers).Struct.decls;
        inline for (verifiers) |name| {
            std.debug.assert(std.meta.stringToEnum(Opcode, name) != null);
            const func = @field(sized_binary_verifiers, name);

            var mod = try simple.build(&.{
                @unionInit(Op, name, w),
            });
            defer mod.deinit(ally);

            for (cases) |case| {
                const lhs = case[0];
                const rhs = case[1];

                try simple.push(I, lhs);
                try simple.push(I, rhs);
                try simple.run(&mod);

                const actual = try simple.pop(I);
                const expected = func(I, lhs, rhs);

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