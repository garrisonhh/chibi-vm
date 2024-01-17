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

    const cases = [_]u16{
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

fn generateRandom(prng: *Prng, comptime T: type) T {
    return switch (@typeInfo(T)) {
        .Bool => prng.random().boolean(),
        .Int => prng.random().int(T),
        else => @compileError("generate random for type: " ++ @typeName(T)),
    };
}

/// generates a whole bunch of random cases for a type, edge cases and all
fn generatePrimitiveCases(comptime T: type, count: usize) ![]const T {
    var cases = std.ArrayList(T).init(ally);
    defer cases.deinit();

    // add common edge cases for types where this is useful
    switch (@typeInfo(T)) {
        .Bool => {},
        .Int => {
            const max_int = std.math.maxInt(T);
            const min_int = std.math.minInt(T);
            const edges = [_]T{ 0, 1, max_int, min_int };
            try cases.appendSlice(&edges);
        },
        else => @compileError("unsupported type: " ++ @typeName(T)),
    }

    // if this fails, raise 'count' or you will miss edge cases
    std.debug.assert(count >= cases.items.len);

    // add random cases
    var prng = Prng.init(random_seed);
    for (0..count - cases.items.len) |_| {
        try cases.append(generateRandom(&prng, T));
    }

    return try cases.toOwnedSlice();
}

/// generates a tuple compatible with @call for this function
fn CaseArgs(comptime F: type) type {
    comptime {
        const info = @typeInfo(F).Fn;
        if (info.params.len > 10) {
            @compileError("supports up to 10 arguments");
        }

        var field_names: [20]u8 = undefined;
        for (0..10) |i| {
            field_names[i * 2] = '0' + @as(u8, @intCast(i));
            field_names[i * 2 + 1] = 0;
        }

        var fields: [info.params.len]std.builtin.Type.StructField = undefined;
        for (&fields, info.params, 0..) |*field, param, i| {
            const ParamType = param.type.?;
            field.* = .{
                .name = field_names[i * 2 .. i * 2 + 1 :0],
                .type = ParamType,
                .default_value = null,
                .is_comptime = false,
                .alignment = @alignOf(ParamType),
            };
        }

        return @Type(.{
            .Struct = .{
                .layout = .Auto,
                .fields = &fields,
                .decls = &.{},
                .is_tuple = true,
            },
        });
    }
}

/// return value of function without error union
fn CaseReturns(comptime F: type) type {
    comptime {
        const eu = @typeInfo(F).Fn.return_type.?;
        return @typeInfo(eu).ErrorUnion.payload;
    }
}

/// CaseArgs, but each field is a slice of the original type
fn CaseArgSlices(comptime F: type) type {
    comptime {
        const Args = CaseArgs(F);
        const arg_fields = @typeInfo(Args).Struct.fields;

        var fields: [arg_fields.len]std.builtin.Type.StructField = undefined;
        @memcpy(&fields, arg_fields);
        for (&fields) |*field| {
            field.type = []const field.type;
        }

        var info = @typeInfo(Args);
        info.Struct.fields = &fields;
        return @Type(info);
    }
}

fn CombinationIterator(comptime N: comptime_int) type {
    comptime {
        if (N <= 0) @compileError("N must be more than zero");
    }

    return struct {
        const Self = @This();

        done: bool = false,
        indices: [N]usize = [1]usize{0} ** N,
        k: usize,

        fn next(self: *Self) ?[N]usize {
            if (self.done) return null;

            const result = self.indices;

            self.indices[0] += 1;
            for (1..N) |i| {
                if (self.indices[i - 1] == self.k) {
                    self.indices[i - 1] = 0;
                    self.indices[i] += 1;
                } else {
                    break;
                }
            }

            self.done = for (self.indices) |index| {
                if (index != self.k - 1) {
                    break false;
                }
            } else true;

            return result;
        }
    };
}

fn combinations(comptime N: comptime_int, k: usize) CombinationIterator(N) {
    return .{ .k = k };
}

/// generates test arguments for a verifier function
///
/// due to combinations of args, count will be exponentiated by the number of
/// fields in the args tuple (so keep it low-ish!)
fn generateArgCases(comptime F: type, count: usize) ![]const CaseArgs(F) {
    const Args = CaseArgs(F);
    const ArgSlices = CaseArgSlices(F);
    const arg_count = @typeInfo(Args).Struct.fields.len;

    // generate individual cases
    var slices: ArgSlices = undefined;
    inline for (0..arg_count) |i| {
        const T = @typeInfo(Args).Struct.fields[i].type;
        slices[i] = try generatePrimitiveCases(T, count);
    }
    defer {
        inline for (0..arg_count) |i| {
            ally.free(slices[i]);
        }
    }

    // generate combinations
    var cases = std.ArrayList(Args).init(ally);
    defer cases.deinit();

    var combo_iter = combinations(arg_count, count);
    while (combo_iter.next()) |combo| {
        var args: Args = undefined;
        inline for (0..arg_count) |i| {
            args[i] = slices[i][combo[i]];
        }

        try cases.append(args);
    }

    return try cases.toOwnedSlice();
}

fn testFailed(
    comptime F: type,
    opcode: Opcode,
    args: CaseArgs(F),
    comptime fmt: []const u8,
    fmt_args: anytype,
) error{TestFailure} {
    std.debug.print("[test failure]\n", .{});
    std.debug.print("{s}(", .{@tagName(opcode)});
    inline for (args, 0..) |arg, i| {
        if (i > 0) std.debug.print(", ", .{});
        std.debug.print("({}) {}", .{ @TypeOf(arg), i });
    }
    std.debug.print(")\n", .{});
    std.debug.print(fmt ++ "\n", fmt_args);

    return error.TestFailure;
}

fn VerifierResult(comptime T: type) type {
    return union(enum) {
        const Self = @This();

        err: Env.Error,
        payload: T,

        fn from(eu: Env.Error!T) Self {
            if (eu) |data| {
                return .{ .payload = data };
            } else |e| {
                return .{ .err = e };
            }
        }

        fn eql(self: Self, other: Self) bool {
            return std.meta.eql(self, other);
        }

        pub fn format(
            self: Self,
            comptime _: []const u8,
            _: std.fmt.FormatOptions,
            writer: anytype,
        ) @TypeOf(writer).Error!void {
            switch (self) {
                inline else => |data| {
                    try writer.print("{}", .{data});
                },
            }
        }
    };
}

/// does automatic testing on a function
fn applyVerifier(
    comptime F: type,
    comptime function: F,
    op: Op,
) !void {
    // generate test cases
    const count = 16;
    const cases = switch (@typeInfo(F).Fn.params.len) {
        0...10 => try generateArgCases(F, count),
        else => unreachable,
    };
    defer ally.free(cases);

    // create module for function's opcode
    var mod = try simple.build(&.{op});
    defer mod.deinit(ally);

    // run the tests
    const Returns = CaseReturns(F);
    const Result = VerifierResult(Returns);
    for (cases) |args| {
        inline for (args) |arg| {
            try simple.push(@TypeOf(arg), arg);
        }

        const actual: Result = res: {
            simple.run(&mod) catch |e| {
                break :res .{ .err = e };
            };

            break :res Result.from(simple.pop(Returns));
        };
        const expected = Result.from(@call(.auto, function, args));

        if (!actual.eql(expected)) {
            const fmt =
                \\actual:   {}
                \\expected: {}
                \\
            ;

            return testFailed(F, op, args, fmt, .{ actual, expected });
        }

        const stack_size = simple.getStackSize();
        if (stack_size != 0) {
            return testFailed(F, op, args, "stack not empty", .{});
        }
    }
}

fn sized_verifiers(comptime width: Width) type {
    const I = std.meta.Int(.signed, @as(u16, width.bytes()) * 8);
    const U = std.meta.Int(.unsigned, @as(u16, width.bytes()) * 8);
    return struct {
        pub fn add(a: U, b: U) Env.Error!U {
            return a +% b;
        }

        pub fn sub(a: U, b: U) Env.Error!U {
            return a -% b;
        }

        pub fn mulu(a: U, b: U) Env.Error!U {
            return a *% b;
        }

        pub fn muli(a: I, b: I) Env.Error!I {
            return a *% b;
        }

        pub fn divu(a: U, b: U) Env.Error!U {
            return std.math.divFloor(U, a, b) catch |e| switch (e) {
                error.DivisionByZero => Env.Error.VmDivideByZero,
            };
        }

        pub fn divi(a: I, b: I) Env.Error!I {
            return std.math.divFloor(I, a, b) catch |e| switch (e) {
                error.Overflow => Env.Error.VmIntegerOverflow,
                error.DivisionByZero => Env.Error.VmDivideByZero,
            };
        }

        pub fn modu(a: U, b: U) Env.Error!U {
            return std.math.mod(U, a, b) catch |e| switch (e) {
                error.DivisionByZero => Env.Error.VmDivideByZero,
            };
        }

        pub fn modi(a: I, b: I) Env.Error!I {
            if (b == 0) {
                return Env.Error.VmDivideByZero;
            } else if (b < 0) {
                const denom = std.math.negate(b) catch |e| switch (e) {
                    error.Overflow => return Env.Error.VmIntegerOverflow,
                };
                return std.math.negate(@rem(a, denom)) catch |e| switch (e) {
                    error.Overflow => Env.Error.VmIntegerOverflow,
                };
            } else {
                return @rem(a, b);
            }
        }

        pub fn neg(a: I) Env.Error!I {
            return std.math.negate(a) catch |e| switch (e) {
                error.Overflow => error.VmIntegerOverflow,
            };
        }

        pub fn bitand(a: U, b: U) Env.Error!U {
            return a & b;
        }

        pub fn bitor(a: U, b: U) Env.Error!U {
            return a | b;
        }

        pub fn bitxor(a: U, b: U) Env.Error!U {
            return a ^ b;
        }

        pub fn bitcom(a: U) Env.Error!U {
            return ~a;
        }

        pub fn eq(a: U, b: U) Env.Error!bool {
            return a == b;
        }

        pub fn ne(a: U, b: U) Env.Error!bool {
            return a != b;
        }

        pub fn eqz(a: U) Env.Error!bool {
            return a == 0;
        }

        pub fn extend(a: U) Env.Error!u64 {
            return a;
        }

        pub fn sign_extend(a: I) Env.Error!i64 {
            return a;
        }

        pub fn sign_narrow(a: i64) Env.Error!I {
            return std.math.cast(I, a) orelse Env.Error.VmIntegerOverflow;
        }
    };
}

// runs generated tests on all of the verifier namespaces
test "generated" {
    try simple.init();
    defer simple.deinit();

    inline for (comptime std.enums.values(Width)) |width| {
        const ns = sized_verifiers(width);
        inline for (@typeInfo(ns).Struct.decls) |decl| {
            const function = @field(ns, decl.name);
            const op = @unionInit(Op, decl.name, width);
            try applyVerifier(@TypeOf(function), function, op);
        }
    }
}
