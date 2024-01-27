//! testing for low level vm behavior.
//!
//! the goal here is that every opcode should match its intended behavior, and
//! always produce an error instead of panicing, so basically enforcing as much
//! runtime safety as is possible for a language at the level of C or Zig.
//!
//! errors produced by the vm should be treated as a kind of panic for the
//! program running on the vm itself, users of the vm should be able to rely on
//! it to produce a reasonable runtime error, and generate code that prevents
//! these conditions if that is desired.
//!
//! duplicating the behavior here also acts as an important step towards a well
//! specified vm which is also a goal.

const std = @import("std");
const Allocator = std.mem.Allocator;
const vm = @import("vm");
const Width = vm.Width;
const Op = vm.Op;
const Opcode = vm.Opcode;
const Builder = vm.Builder;
const Module = vm.Module;
const Env = vm.Env;

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

        try builder.define(func_name, .exported, .code);
        for (code) |o| {
            try builder.op(o);
        }

        const unit = try builder.build(ally);
        defer unit.deinit(ally);

        return try vm.link(ally, &.{unit});
    }

    fn int(comptime w: Width) type {
        return std.meta.Int(.signed, @as(u16, w.bytes()) * 8);
    }

    fn uint(comptime w: Width) type {
        return std.meta.Int(.unsigned, @as(u16, w.bytes()) * 8);
    }

    fn runModule(mod: Module) !void {
        const loc = mod.get(func_name).?;
        std.debug.assert(loc.segment == .code);

        var state = try Env.State.init(ally, mod, loc.offset);
        defer state.deinit(ally);

        try env.run(&state);
    }

    fn run(state: *Env.State) !void {
        try env.run(state);
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

    /// pop a value and expect its result
    fn expect(comptime T: type, expected: T) !void {
        const actual = try pop(T);
        try std.testing.expectEqual(expected, actual);
    }

    /// returns stack size in bytes
    fn getStackSize() usize {
        const size = @intFromPtr(env.stack.top) - @intFromPtr(env.stack.mem.ptr);
        return size;
    }

    fn expectStackSize(expected: usize) !void {
        const stack_size = simple.getStackSize();
        if (stack_size != expected) {
            std.debug.print("stack size expected: {} actual {}\n", .{
                expected,
                stack_size,
            });
            return error.TestWrongStackSize;
        }
    }
};

// test generation =============================================================

const Prng = std.rand.DefaultPrng;
const random_seed = 0;

fn generateRandom(
    arena_ally: Allocator,
    prng: *Prng,
    comptime T: type,
) !T {
    return switch (@typeInfo(T)) {
        .Bool => prng.random().boolean(),
        .Int => prng.random().int(T),
        .Array => |meta| array: {
            var array: T = undefined;
            for (&array) |*slot| {
                slot.* = try generateRandom(arena_ally, prng, meta.child);
            }

            break :array array;
        },
        .Pointer => |meta| switch (meta.size) {
            .One => sg: {
                const ptr = try arena_ally.create(meta.child);
                ptr.* = try generateRandom(arena_ally, prng, meta.child);
                break :sg ptr;
            },
            else => @compileError("generate random pointer: " ++ @typeName(T)),
        },
        else => @compileError("generate random: " ++ @typeName(T)),
    };
}

/// generates a whole bunch of random cases for a type, edge cases and all
fn generatePrimitiveCases(
    arena_ally: Allocator,
    comptime T: type,
    count: usize,
) ![]const T {
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
        try cases.append(try generateRandom(arena_ally, &prng, T));
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
        if (N <= 0) @compileError("N must be greater than zero");
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
fn generateArgCases(
    arena_ally: Allocator,
    comptime F: type,
    count: usize,
) ![]const CaseArgs(F) {
    const Args = CaseArgs(F);
    const ArgSlices = CaseArgSlices(F);
    const arg_count = @typeInfo(Args).Struct.fields.len;

    // generate individual cases
    var slices: ArgSlices = undefined;
    inline for (0..arg_count) |i| {
        const T = @typeInfo(Args).Struct.fields[i].type;
        slices[i] = try generatePrimitiveCases(arena_ally, T, count);
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

        err: Env.ExecError,
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
    var arena = std.heap.ArenaAllocator.init(ally);
    defer arena.deinit();
    const arena_ally = arena.allocator();

    // generate test cases
    const count = 16;
    const cases = switch (@typeInfo(F).Fn.params.len) {
        0...10 => try generateArgCases(arena_ally, F, count),
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
            simple.runModule(mod) catch |e| {
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

// =============================================================================

const unique_verifiers = struct {
    pub fn @"and"(a: bool, b: bool) Env.Error!bool {
        return a and b;
    }

    pub fn @"or"(a: bool, b: bool) Env.Error!bool {
        return a or b;
    }

    pub fn not(a: bool) Env.Error!bool {
        return !a;
    }
};

fn generic_verifiers(comptime width: Width) type {
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

// operators that transform values into an output value can all be automated
// with very similar inputs
test "generated" {
    @setEvalBranchQuota(10_000);

    try simple.init();
    defer simple.deinit();

    const uvs = unique_verifiers;
    inline for (@typeInfo(uvs).Struct.decls) |decl| {
        const function = @field(uvs, decl.name);

        const opcode = comptime std.meta.stringToEnum(Opcode, decl.name).?;
        const op = comptime switch (opcode) {
            .@"and", .@"or", .not => @unionInit(Op, decl.name, {}),
            else => unreachable,
        };

        try applyVerifier(@TypeOf(function), function, op);
    }

    inline for (comptime std.enums.values(Width)) |width| {
        const ns = generic_verifiers(width);
        inline for (@typeInfo(ns).Struct.decls) |decl| {
            const function = @field(ns, decl.name);
            const op = @unionInit(Op, decl.name, width);
            try applyVerifier(@TypeOf(function), function, op);
        }
    }
}

test "halt" {
    try simple.init();
    defer simple.deinit();

    const mod = try simple.build(&.{.halt});
    defer mod.deinit(ally);

    const result = simple.runModule(mod);
    try std.testing.expectError(Env.Error.VmHalt, result);
}

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
        const mod = try simple.build(&.{
            Op{ .enter = reserve },
        });
        defer mod.deinit(ally);

        try simple.runModule(mod);

        const expected = std.mem.alignForward(usize, reserve, 8);
        try simple.expectStackSize(expected);

        simple.reset();
    }
}

test "dup" {
    try simple.init();
    defer simple.deinit();

    const mod = try simple.build(&.{.dup});
    defer mod.deinit(ally);

    try simple.push(u64, 42);
    try simple.runModule(mod);
    try simple.expectStackSize(16);
    try simple.expect(u64, 42);
    try simple.expect(u64, 42);
    try simple.expectStackSize(0);
}

test "drop" {
    try simple.init();
    defer simple.deinit();

    const mod = try simple.build(&.{.drop});
    defer mod.deinit(ally);

    try simple.push(u64, 0);
    try simple.expectStackSize(8);
    try simple.runModule(mod);
    try simple.expectStackSize(0);
}

test "constant" {
    const count = 64;

    try simple.init();
    defer simple.deinit();

    var prng = Prng.init(random_seed);

    inline for (comptime std.enums.values(Width)) |width| {
        const Bytes = [comptime width.bytes()]u8;

        for (0..count) |_| {
            var arena = std.heap.ArenaAllocator.init(ally);
            defer arena.deinit();
            const arena_ally = arena.allocator();

            const bytes = try generateRandom(arena_ally, &prng, Bytes);
            const op = Op{
                .constant = @unionInit(Op.Constant, @tagName(width), bytes),
            };

            const mod = try simple.build(&.{op});
            defer mod.deinit(ally);

            try simple.runModule(mod);
            try simple.expectStackSize(8);
            try simple.expect(Bytes, bytes);
            try simple.expectStackSize(0);
        }
    }
}

test "local" {
    const count = 256;

    var prng = Prng.init(random_seed);

    try simple.init();
    defer simple.deinit();

    for (0..count) |_| {
        const offset = prng.random().int(i16);

        const mod = try simple.build(&.{
            Op{ .local = offset },
        });
        defer mod.deinit(ally);

        try simple.runModule(mod);

        const base = @intFromPtr(simple.env.stack.base);
        const expected = if (offset > 0) pos: {
            break :pos base + @as(usize, @intCast(offset));
        } else neg: {
            break :neg base - @as(usize, @intCast(-offset));
        };

        try simple.expectStackSize(8);
        try simple.expect(usize, expected);
    }
}

test "load" {
    const data_len = 8192;
    const count = 128;

    try simple.init();
    defer simple.deinit();

    var prng = Prng.init(random_seed);

    inline for (comptime std.enums.values(Width)) |width| {
        const Bytes = [comptime width.bytes()]u8;

        var arena = std.heap.ArenaAllocator.init(ally);
        defer arena.deinit();
        const arena_ally = arena.allocator();

        // create some random bytes to retrieve some data from
        const data = try arena_ally.alloc(u8, data_len);
        prng.random().bytes(data);

        // attempt to load from some random offsets
        for (0..count) |_| {
            const nbytes: u16 = width.bytes();
            const rand_offset = prng.random().uintLessThan(u16, data_len);
            const byte_offset = std.mem.alignBackward(u16, rand_offset, nbytes);
            const offset = @divExact(byte_offset, nbytes);

            const expected: *const Bytes = @ptrCast(data[byte_offset..]);

            const op = Op{
                .load = Op.Address{
                    .width = width,
                    .offset = offset,
                },
            };

            const mod = try simple.build(&.{op});
            defer mod.deinit(ally);

            try simple.push(*anyopaque, @as(*anyopaque, @ptrCast(data)));
            try simple.runModule(mod);
            try simple.expectStackSize(8);
            try simple.expect(Bytes, expected.*);
            try simple.expectStackSize(0);
        }
    }
}

test "store" {
    const data_len = 8192;
    const count = 128;

    try simple.init();
    defer simple.deinit();

    var prng = Prng.init(random_seed);

    inline for (comptime std.enums.values(Width)) |width| {
        const Bytes = [comptime width.bytes()]u8;

        var arena = std.heap.ArenaAllocator.init(ally);
        defer arena.deinit();
        const arena_ally = arena.allocator();

        // create some random bytes to retrieve some data from
        const data = try arena_ally.alloc(u8, data_len);
        prng.random().bytes(data);

        // attempt to load from some random offsets
        for (0..count) |_| {
            const nbytes: u16 = width.bytes();
            const rand_offset = prng.random().uintLessThan(u16, data_len);
            const byte_offset = std.mem.alignBackward(u16, rand_offset, nbytes);
            const offset = @divExact(byte_offset, nbytes);

            const input = try generateRandom(arena_ally, &prng, Bytes);

            const op = Op{
                .store = Op.Address{
                    .width = width,
                    .offset = offset,
                },
            };

            const mod = try simple.build(&.{op});
            defer mod.deinit(ally);

            try simple.push(*anyopaque, @as(*anyopaque, @ptrCast(data)));
            try simple.push(Bytes, input);
            try simple.runModule(mod);
            try simple.expectStackSize(0);

            const actual: *const Bytes = @ptrCast(data[byte_offset .. byte_offset + nbytes]);
            try std.testing.expectEqual(input, actual.*);
        }
    }
}

test "copy" {
    const count = 128;

    try simple.init();
    defer simple.deinit();

    var prng = Prng.init(random_seed);

    var arena = std.heap.ArenaAllocator.init(ally);
    defer arena.deinit();
    const arena_ally = arena.allocator();

    const data_lengths = try generatePrimitiveCases(arena_ally, u16, count);
    defer ally.free(data_lengths);

    for (data_lengths) |data_len| {
        const data = try ally.alloc(u8, data_len);
        defer ally.free(data);
        prng.random().bytes(data);

        const dest = try ally.alloc(u8, data_len);
        defer ally.free(dest);

        const mod = try simple.build(&.{
            .{ .copy = data_len },
        });
        defer mod.deinit(ally);

        const data_ptr: *anyopaque = @ptrCast(data);
        try simple.push(*anyopaque, data_ptr);
        try simple.push(*anyopaque, @as(*anyopaque, @ptrCast(dest)));
        try simple.runModule(mod);
        try simple.expect(*anyopaque, data_ptr);
        try simple.expectStackSize(0);

        try std.testing.expectEqualSlices(u8, data, dest);
    }
}

test "zero" {
    const count = 128;

    try simple.init();
    defer simple.deinit();

    var prng = Prng.init(random_seed);

    var arena = std.heap.ArenaAllocator.init(ally);
    defer arena.deinit();
    const arena_ally = arena.allocator();

    const data_lengths = try generatePrimitiveCases(arena_ally, u16, count);
    defer ally.free(data_lengths);

    for (data_lengths) |data_len| {
        const data = try ally.alloc(u8, data_len);
        defer ally.free(data);
        prng.random().bytes(data);

        const mod = try simple.build(&.{
            .{ .zero = data_len },
        });
        defer mod.deinit(ally);

        const data_ptr: *anyopaque = @ptrCast(data);
        try simple.push(*anyopaque, data_ptr);
        try simple.runModule(mod);
        try simple.expect(*anyopaque, data_ptr);
        try simple.expectStackSize(0);

        for (data) |byte| {
            if (byte != 0) {
                return error.TestFailure;
            }
        }
    }
}

test "jump" {
    const count = 8;

    try simple.init();
    defer simple.deinit();

    for (0..count) |n| {
        var b = Builder.init(ally);
        defer b.deinit();

        const dest = try b.backref();

        try b.op(.{ .jump = dest });
        for (0..n) |_| try b.op(.halt);

        b.resolve(dest);

        const unit = try b.build(ally);
        defer unit.deinit(ally);

        const mod = try vm.link(ally, &.{unit});
        defer mod.deinit(ally);

        var state = try Env.State.init(ally, mod, 0);
        defer state.deinit(ally);

        try simple.run(&state);

        try std.testing.expectEqual(state.code_len, state.pc);
    }
}

test "jz" {
    try simple.init();
    defer simple.deinit();

    inline for (comptime std.enums.values(Width)) |width| {
        const I = simple.int(width);

        var b = Builder.init(ally);
        defer b.deinit();

        const dest = try b.backref();

        try b.op(.{ .jz = .{ .width = width, .dest = dest } });
        try b.define("nonzero", .exported, .code);
        try b.op(.halt);

        b.resolve(dest);
        try b.define("zero", .exported, .code);
        try b.op(.halt);

        const unit = try b.build(ally);
        defer unit.deinit(ally);

        const mod = try vm.link(ally, &.{unit});
        defer mod.deinit(ally);

        const loc_nonzero = mod.get("nonzero").?;
        const loc_zero = mod.get("zero").?;

        for (0..2) |n| {
            try simple.push(I, @as(I, @intCast(n)));

            var state = try Env.State.init(ally, mod, 0);
            defer state.deinit(ally);

            const res = simple.run(&state);
            try std.testing.expectError(Env.Error.VmHalt, res);

            const expected: usize = switch (n) {
                0 => loc_zero.offset + 1,
                1 => loc_nonzero.offset + 1,
                else => unreachable,
            };
            try std.testing.expectEqual(expected, state.pc);
        }
    }
}

test "jnz" {
    try simple.init();
    defer simple.deinit();

    inline for (comptime std.enums.values(Width)) |width| {
        const I = simple.int(width);

        var b = Builder.init(ally);
        defer b.deinit();

        const dest = try b.backref();

        try b.op(.{ .jnz = .{ .width = width, .dest = dest } });
        try b.define("zero", .exported, .code);
        try b.op(.halt);

        b.resolve(dest);
        try b.define("nonzero", .exported, .code);
        try b.op(.halt);

        const unit = try b.build(ally);
        defer unit.deinit(ally);

        const mod = try vm.link(ally, &.{unit});
        defer mod.deinit(ally);

        const loc_nonzero = mod.get("nonzero").?;
        const loc_zero = mod.get("zero").?;

        for (0..2) |n| {
            try simple.push(I, @as(I, @intCast(n)));

            var state = try Env.State.init(ally, mod, 0);
            defer state.deinit(ally);

            const res = simple.run(&state);
            try std.testing.expectError(Env.Error.VmHalt, res);

            const expected: usize = switch (n) {
                0 => loc_zero.offset + 1,
                1 => loc_nonzero.offset + 1,
                else => unreachable,
            };
            try std.testing.expectEqual(expected, state.pc);
        }
    }
}

test "label" {
    const count = 8;

    try simple.init();
    defer simple.deinit();

    for (0..count) |n| {
        var b = Builder.init(ally);
        defer b.deinit();

        const dest = try b.backref();
        try b.op(.{ .label = dest });
        try b.op(.halt);

        for (0..n) |_| {
            try b.op(.halt);
        }

        b.resolve(dest);

        const unit = try b.build(ally);
        defer unit.deinit(ally);

        const mod = try vm.link(ally, &.{unit});
        defer mod.deinit(ally);

        var state = try Env.State.init(ally, mod, 0);
        defer state.deinit(ally);

        const res = simple.run(&state);
        try std.testing.expectError(Env.Error.VmHalt, res);
        try simple.expect(u32, 6 + @as(u32, @intCast(n)));
    }
}

test "data" {
    try simple.init();
    defer simple.deinit();

    var b = Builder.init(ally);
    defer b.deinit();

    const label = try b.data("hello, world");
    try b.op(.{ .data = label });

    const unit = try b.build(ally);
    defer unit.deinit(ally);

    const mod = try vm.link(ally, &.{unit});
    defer mod.deinit(ally);

    var state = try Env.State.init(ally, mod, 0);
    defer state.deinit(ally);

    try simple.run(&state);

    const ptr = try simple.pop([*]u8);
    try std.testing.expectEqualStrings("hello, world", ptr[0..12]);
}

test "bss" {
    try simple.init();
    defer simple.deinit();

    var b = Builder.init(ally);
    defer b.deinit();

    const label = try b.bss(@sizeOf(u32));
    try b.op(.{ .bss = label });
    try b.op(.dup);
    try b.constant(u32, 42);
    try b.op(.{ .store = .{
        .width = .half,
        .offset = 0,
    } });

    const unit = try b.build(ally);
    defer unit.deinit(ally);

    const mod = try vm.link(ally, &.{unit});
    defer mod.deinit(ally);

    var state = try Env.State.init(ally, mod, 0);
    defer state.deinit(ally);

    try simple.run(&state);

    const ptr = try simple.pop(*const u32);
    try std.testing.expectEqual(@as(u32, 42), ptr.*);
}

test "call" {
    try simple.init();
    defer simple.deinit();

    var b = Builder.init(ally);
    defer b.deinit();

    const dest = try b.backref();
    try b.op(.{ .label = dest });
    try b.op(.call);

    b.resolve(dest);
    try b.op(.halt);
    try b.op(.halt);

    const unit = try b.build(ally);
    defer unit.deinit(ally);

    const mod = try vm.link(ally, &.{unit});
    defer mod.deinit(ally);

    var state = try Env.State.init(ally, mod, 0);
    defer state.deinit(ally);

    const res = simple.run(&state);
    try std.testing.expectError(Env.Error.VmHalt, res);
    try std.testing.expectEqual(state.code_len - 1, state.pc);

    const expected = Env.Frame{
        .base = simple.env.stack.mem.ptr,
        .pc = 6,
    };
    const frame = try simple.pop(Env.Frame);
    try std.testing.expectEqual(expected, frame);
}

test "ret" {
    const max_params = std.math.maxInt(u8);

    var prng = Prng.init(random_seed);

    try simple.init();
    defer simple.deinit();

    inline for (comptime std.enums.values(Width)) |width| {
        const I = simple.int(width);

        for (0..max_params) |n| {
            var b = Builder.init(ally);
            defer b.deinit();

            try b.constant(I, 42);
            try b.op(.{ .ret = @intCast(n) });
            try b.op(.halt);

            const unit = try b.build(ally);
            defer unit.deinit(ally);

            const mod = try vm.link(ally, &.{unit});
            defer mod.deinit(ally);

            const start_pc: u32 = std.math.maxInt(u32);
            const start_base = simple.env.stack.base;
            const start_top = simple.env.stack.top;

            // fake params
            for (0..n) |_| {
                const val = prng.random().uintAtMost(u64, std.math.maxInt(u64));
                try simple.push(u64, val);
            }

            var state = try Env.State.init(ally, mod, start_pc);
            defer state.deinit(ally);

            try simple.env.call(&state, 0);
            try simple.run(&state);

            try simple.expect(u8, 42);
            try std.testing.expectEqual(start_pc, state.pc);
            try std.testing.expectEqual(start_base, simple.env.stack.base);
            try std.testing.expectEqual(start_top, simple.env.stack.top);
        }
    }
}

fn nativeMulAdd(env: *Env) Env.Error!void {
    const c = try env.pop(u64);
    const b = try env.pop(u64);
    const a = try env.pop(u64);
    try env.push(u64, a * b + c);
}

test "native_call" {
    try simple.init();
    defer simple.deinit();

    var b = Builder.init(ally);
    defer b.deinit();

    try b.constant(*const Env.NativeFn, &nativeMulAdd);
    try b.op(.native_call);

    const unit = try b.build(ally);
    defer unit.deinit(ally);

    const mod = try vm.link(ally, &.{unit});
    defer mod.deinit(ally);

    var state = try Env.State.init(ally, mod, 0);
    defer state.deinit(ally);

    try simple.push(u64, 3);
    try simple.push(u64, 4);
    try simple.push(u64, 5);
    try simple.run(&state);
    try simple.expect(u64, 17);
    try simple.expectStackSize(0);
}