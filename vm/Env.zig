//! the runtime environment for code execution

const std = @import("std");
const Allocator = std.mem.Allocator;
const in_debug = @import("builtin").mode == .Debug;
const ops = @import("ops.zig");
const ByteOp = ops.ByteOp;
const Opcode = ops.Opcode;
const Width = ops.Width;
const target = @import("target.zig");
const Module = target.Module;

// utils =======================================================================

fn align8(comptime T: type, value: T) T {
    return std.mem.alignForward(T, value, 8);
}

fn ptrAdd(comptime T: type, ptr: T, offset: isize) T {
    std.debug.assert(@typeInfo(T) == .Pointer);

    const iaddr: isize = @intCast(@intFromPtr(ptr));
    const offset_addr: usize = @intCast(iaddr + offset);
    return @ptrFromInt(offset_addr);
}

// env =========================================================================

const Env = @This();

pub const Error = error{
    VmHalt,
    VmStackOverflow,
    VmStackUnderflow,
    VmDivideByZero,
    VmIntegerOverflow,
};

/// a native function callable by the vm
///
/// this function must follow the same rules as a typical vm function. internal
/// errors should be handled by returning them by value into the vm.
pub const NativeFn = fn(*Env) Error!void;

/// encodes env state that only persists during execution of module code
pub const State = struct {
    /// current env memory
    ///
    /// *code is loaded to offset 0*
    memory: []align(8) u8,
    /// program counter
    pc: u32,

    code_len: u32,
    /// data segment offset
    data: u32,
    /// bss segment offset
    bss: u32,

    pub fn init(
        ally: Allocator,
        mod: Module,
        initial_pc: u32,
    ) Allocator.Error!State {
        const code_len: u32 = @intCast(mod.code.len);
        const data_len: u32 = @intCast(mod.data.len);

        const data_offset = align8(u32, code_len);
        const bss_offset = align8(u32, data_len + data_offset);
        const size = bss_offset + mod.bss;

        const memory = try ally.alignedAlloc(u8, 8, size);
        @memcpy(memory[0..code_len], mod.code);
        @memcpy(memory[data_offset .. data_offset + mod.data.len], mod.data);

        return State{
            .memory = memory,
            .pc = initial_pc,
            .code_len = code_len,
            .data = data_offset,
            .bss = bss_offset,
        };
    }

    pub fn deinit(state: State, ally: Allocator) void {
        ally.free(state.memory);
    }

    fn stackSlice(state: State) []align(8) u8 {
        return state.memory[state.stack..];
    }

    fn readByte(state: *State) u8 {
        defer state.pc += 1;
        return state.memory[state.pc];
    }

    fn readNBytes(state: *State, comptime N: comptime_int) [N]u8 {
        defer state.pc += N;
        const slice = state.memory[state.pc .. state.pc + N];
        const ptr: *const [N]u8 = @ptrCast(slice.ptr);
        return ptr.*;
    }

    fn readValue(state: *State, comptime T: type) T {
        return @bitCast(state.readNBytes(@sizeOf(T)));
    }
};

/// format of a call stack frame
pub const Frame = struct {
    /// how much space this takes on the stack
    pub const aligned_size = Stack.aligned8Size(Frame);

    base: [*]align(8) u8,
    pc: u32,
};

/// operation stack
const Stack = struct {
    mem: []align(8) u8,
    base: [*]align(8) u8,
    top: [*]align(8) u8,

    fn init(mem: []align(8) u8) Stack {
        return Stack{
            .mem = mem,
            .base = mem.ptr,
            .top = mem.ptr,
        };
    }

    /// reset to original state
    fn reset(self: *Stack) void {
        self.base = self.mem.ptr;
        self.top = self.mem.ptr;
    }

    /// total stack used
    fn used(self: Stack) usize {
        return @intFromPtr(self.top) - @intFromPtr(self.mem.ptr);
    }

    /// size of T when accounting for 8-bit alignment
    fn aligned8Size(comptime T: type) comptime_int {
        comptime {
            return align8(usize, @sizeOf(T));
        }
    }

    /// reserve some bytes from the stack
    fn reserve(self: *Stack, nbytes: usize) Error!void {
        const sz = align8(usize, nbytes);
        if (self.used() + sz > self.mem.len) {
            return Error.VmStackOverflow;
        }

        self.top = ptrAdd([*]align(8) u8, self.top, @intCast(sz));
    }

    /// drop some bytes from the stack
    fn vacate(self: *Stack, nbytes: usize) Error!void {
        const sz = align8(usize, nbytes);
        if (self.used() < sz) {
            return Error.VmStackUnderflow;
        }

        self.top = ptrAdd([*]align(8) u8, self.top, -@as(isize, @intCast(sz)));
    }

    fn peek(self: Stack, comptime T: type) ?T {
        const sz = aligned8Size(T);
        if (self.used() < sz) return null;

        const ptr: *const T = @ptrCast(ptrAdd([*]align(8) u8, self.top, -sz));
        return ptr.*;
    }

    fn push(self: *Stack, comptime T: type, value: T) Error!void {
        const sz = aligned8Size(T);
        if (self.used() + sz > self.mem.len) {
            return Error.VmStackOverflow;
        }

        @as(*T, @ptrCast(self.top)).* = value;
        self.top = ptrAdd([*]align(8) u8, self.top, sz);
    }

    fn pop(self: *Stack, comptime T: type) Error!T {
        const value = self.peek(T) orelse {
            return Error.VmStackUnderflow;
        };

        const sz = aligned8Size(T);
        self.top = ptrAdd([*]align(8) u8, self.top, -sz);
        return value;
    }
};

stack: Stack,

pub const Config = struct {
    stack_size: usize = 64 * 1024,
};

pub fn init(ally: Allocator, cfg: Config) Allocator.Error!Env {
    const mem = try ally.alignedAlloc(u8, 8, cfg.stack_size);
    return Env{ .stack = Stack.init(mem) };
}

pub fn deinit(env: *Env, ally: Allocator) void {
    ally.free(env.stack.mem);
}

/// reset env to default state
pub fn reset(env: *Env) void {
    env.stack.reset();
}

pub fn peek(env: Env, comptime T: type) Error!T {
    return env.stack.peek(T) orelse Error.VmStackUnderflow;
}

pub fn push(env: *Env, comptime T: type, data: T) Error!void {
    try env.stack.push(T, data);
}

pub fn pop(env: *Env, comptime T: type) Error!T {
    return env.stack.pop(T);
}

/// does the necessary stack and state twiddling for calling a function
pub fn call(env: *Env, state: *State, dest: u32) Error!void {
    try env.push(Frame, Frame{
        .base = env.stack.base,
        .pc = state.pc,
    });
    env.stack.base = env.stack.top;
    state.pc = dest;
}

// execution ===================================================================

/// writes the next op to stderr for debugging
fn dumpNext(env: *const Env, state: State) void {
    const code = state.memory[state.pc..];
    const byteop: ByteOp = @bitCast(code[0]);
    const extra = code[1 .. 1 + byteop.extraBytes()];

    std.debug.print(
        "{d:>6} | {d:>6} | {s}",
        .{ state.pc, env.stack.used(), @tagName(byteop.opcode) },
    );
    if (byteop.opcode.meta().sized) {
        std.debug.print(" {s}", .{@tagName(byteop.width)});
    }

    if (extra.len > 0) {
        switch (extra.len) {
            0 => {},
            inline 1, 2, 4, 8 => |sz| {
                const bytes: *const [sz]u8 = @ptrCast(extra.ptr);

                switch (byteop.opcode) {
                    .local => {
                        const I = std.meta.Int(.signed, 8 * sz);
                        const n = std.mem.bytesAsValue(I, bytes).*;
                        std.debug.print(" {d}", .{n});
                    },
                    else => {
                        const U = std.meta.Int(.unsigned, 8 * sz);
                        const n = std.mem.bytesAsValue(U, bytes).*;
                        std.debug.print(" {d}", .{n});
                    },
                }
            },
            else => unreachable,
        }
    }

    std.debug.print("\n", .{});
}

/// dumps stack to stderr for debugging
fn dumpStack(env: *const Env) void {
    const start: [*]const u64 = @ptrCast(env.stack.mem.ptr);
    const base = (@intFromPtr(env.stack.base) - @intFromPtr(start)) / 8;
    const top = (@intFromPtr(env.stack.top) - @intFromPtr(start)) / 8;

    std.debug.print("[stack]\n", .{});
    for (0..top) |i| {
        if (i == base) {
            std.debug.print("<base>\n", .{});
        }
        std.debug.print("{} | {}\n", .{ i, start[i] });
    }
    std.debug.print("\n", .{});
}

/// execute ops until complete with state provided
pub fn run(env: *Env, state: *State) Error!void {
    while (state.pc < state.code_len) {
        const byte = state.readByte();
        const sub = byte_subs[byte].?;

        try sub(env, state);
    }
}

pub const ExecError = Allocator.Error || Error || error{
    NoSuchFunction,
    NotAFunction,
};

/// execute a function exported from a module
///
/// *allocator is used to allocate state memory*
pub fn exec(
    env: *Env,
    ally: Allocator,
    mod: Module,
    name: []const u8,
) ExecError!void {
    const loc = mod.get(name) orelse {
        return ExecError.NoSuchFunction;
    };
    if (loc.segment != .code) {
        return ExecError.NotAFunction;
    }

    // add call frame from the end of the code so that execution stops cleanly
    // when returning from the exported function
    var state = try State.init(ally, mod, @intCast(mod.code.len));
    defer state.deinit(ally);
    try env.call(&state, loc.offset);

    try env.run(&state);
}

// opcode functions ============================================================

/// a subroutine for an op
const OpSub = fn (env: *Env, state: *State) Error!void;

/// a computed array which maps ByteOps to subroutines
const byte_subs = blk: {
    var arr: [std.math.maxInt(u8)]?*const OpSub = undefined;
    @memset(&arr, null);

    const generic_sub_namespaces = [_]type{
        generic_subs(.byte),
        generic_subs(.short),
        generic_subs(.half),
        generic_subs(.word),
    };

    for (std.enums.values(Opcode)) |opcode| {
        const op_name = @tagName(opcode);
        const is_sized = opcode.meta().sized;

        if (!is_sized) {
            if (!@hasDecl(monomorphic_subs, op_name)) {
                @compileError("no implementation for opcode: " ++ op_name);
            }

            const func = @field(monomorphic_subs, op_name);
            const bo = ByteOp{ .opcode = opcode };

            arr[@as(u8, @bitCast(bo))] = &func;
        } else for (generic_sub_namespaces) |ns| {
            if (!@hasDecl(ns, op_name)) {
                @compileError("no implementation for opcode: " ++ op_name);
            }

            const func = @field(ns, op_name);
            const bo = ByteOp{
                .opcode = opcode,
                .width = ns.width,
            };

            arr[@as(u8, @bitCast(bo))] = &func;
        }
    }

    break :blk arr;
};

/// subroutines for opcodes which aren't generic over width
const monomorphic_subs = struct {
    fn halt(_: *Env, _: *State) Error!void {
        return Error.VmHalt;
    }

    fn enter(env: *Env, state: *State) Error!void {
        const stack_size = state.readValue(u16);
        try env.stack.reserve(stack_size);
    }

    fn ret(env: *Env, state: *State) Error!void {
        const ret_params: usize = state.readValue(u8);

        // get return value
        const return_value = try env.pop([8]u8);

        // revert frame and pc
        env.stack.top = env.stack.base;
        const frame = try env.pop(Frame);
        env.stack.base = frame.base;
        state.pc = frame.pc;

        // remove params
        try env.stack.vacate(ret_params * 8);

        // push back return value
        try env.push([8]u8, return_value);
    }

    fn label(env: *Env, state: *State) Error!void {
        const dest = state.readValue(u32);
        try env.push(u32, dest);
    }

    fn data(env: *Env, state: *State) Error!void {
        const offset = state.readValue(u32);
        const dest = @intFromPtr(state.memory.ptr) + state.data + offset;
        try env.push(usize, dest);
    }

    fn bss(env: *Env, state: *State) Error!void {
        const offset = state.readValue(u32);
        const dest = @intFromPtr(state.memory.ptr) + state.bss + offset;
        try env.push(usize, dest);
    }

    fn dup(env: *Env, _: *State) Error!void {
        const val = try env.pop(u64);
        try env.push(u64, val);
        try env.push(u64, val);
    }

    fn drop(env: *Env, _: *State) Error!void {
        _ = try env.pop(u8);
    }

    fn local(env: *Env, state: *State) Error!void {
        const offset = state.readValue(i16);
        const ptr = ptrAdd(*anyopaque, env.stack.base, offset);
        try env.push(*anyopaque, ptr);
    }

    fn @"and"(env: *Env, _: *State) Error!void {
        const a = try env.pop(bool);
        const b = try env.pop(bool);
        try env.push(bool, a and b);
    }

    fn @"or"(env: *Env, _: *State) Error!void {
        const a = try env.pop(bool);
        const b = try env.pop(bool);
        try env.push(bool, a or b);
    }

    fn not(env: *Env, _: *State) Error!void {
        const val = try env.pop(bool);
        try env.push(bool, !val);
    }

    fn zero(env: *Env, state: *State) Error!void {
        const nbytes = state.readValue(u32);
        const ptr = try env.peek([*]u8);
        @memset(ptr[0..nbytes], 0);
    }

    fn copy(env: *Env, state: *State) Error!void {
        const nbytes = state.readValue(u32);
        const src = try env.pop([*]u8);
        const dst = try env.peek([*]u8);
        @memcpy(dst[0..nbytes], src[0..nbytes]);
    }

    fn jump(_: *Env, state: *State) Error!void {
        const dest = state.readValue(u32);
        state.pc = dest;
    }

    fn call(env: *Env, state: *State) Error!void {
        const dest = try env.pop(u32);
        try env.call(state, dest);
    }

    fn native_call(env: *Env, _: *State) Error!void {
        const native_fn = try env.pop(*const NativeFn);
        try native_fn(env);
    }
};

/// subroutines for opcodes which are generic over width
fn generic_subs(comptime W: Width) type {
    return struct {
        const width = W;
        const nbytes = W.bytes();
        const nbits = @as(u7, nbytes) * 8;
        const I = std.meta.Int(.signed, nbits);
        const U = std.meta.Int(.unsigned, nbits);
        const F = std.meta.Float(nbits);

        fn constant(env: *Env, state: *State) Error!void {
            const bytes = state.readNBytes(nbytes);
            try env.push([nbytes]u8, bytes);
        }

        fn add(env: *Env, _: *State) Error!void {
            try env.push(U, try env.pop(U) +% try env.pop(U));
        }

        fn sub(env: *Env, _: *State) Error!void {
            const b = try env.pop(U);
            const a = try env.pop(U);
            try env.push(U, a -% b);
        }

        fn mulu(env: *Env, _: *State) Error!void {
            try env.push(U, try env.pop(U) *% try env.pop(U));
        }

        fn muli(env: *Env, _: *State) Error!void {
            try env.push(I, try env.pop(I) *% try env.pop(I));
        }

        fn divu(env: *Env, _: *State) Error!void {
            const b = try env.pop(U);
            const a = try env.pop(U);
            const res = std.math.divFloor(U, a, b) catch |e| {
                return switch (e) {
                    error.DivisionByZero => Error.VmDivideByZero,
                };
            };
            try env.push(U, res);
        }

        fn divi(env: *Env, _: *State) Error!void {
            const b = try env.pop(I);
            const a = try env.pop(I);
            const res = std.math.divFloor(I, a, b) catch |e| {
                return switch (e) {
                    error.Overflow => Error.VmIntegerOverflow,
                    error.DivisionByZero => Error.VmDivideByZero,
                };
            };
            try env.push(I, res);
        }

        fn modu(env: *Env, _: *State) Error!void {
            const b = try env.pop(U);
            const a = try env.pop(U);
            const res = std.math.mod(U, a, b) catch |e| switch (e) {
                error.DivisionByZero => return Error.VmDivideByZero,
            };
            try env.push(U, res);
        }

        // TODO is this too complicated to have as an instruction?
        fn modi(env: *Env, _: *State) Error!void {
            const b = try env.pop(I);
            const a = try env.pop(I);

            if (b == 0) {
                return Error.VmDivideByZero;
            } else if (b > 0) {
                try env.push(I, @rem(a, b));
            } else {
                const denom = std.math.negate(b) catch |e| switch (e) {
                    error.Overflow => return Error.VmIntegerOverflow,
                };
                const res = std.math.negate(@rem(a, denom)) catch |e| switch (e) {
                    error.Overflow => return Error.VmIntegerOverflow,
                };
                try env.push(I, res);
            }
        }

        fn bitand(env: *Env, _: *State) Error!void {
            try env.push(U, try env.pop(U) & try env.pop(U));
        }

        fn bitor(env: *Env, _: *State) Error!void {
            try env.push(U, try env.pop(U) | try env.pop(U));
        }

        fn bitxor(env: *Env, _: *State) Error!void {
            try env.push(U, try env.pop(U) ^ try env.pop(U));
        }

        fn bitcom(env: *Env, _: *State) Error!void {
            const val = try env.pop(U);
            try env.push(U, ~val);
        }

        fn neg(env: *Env, _: *State) Error!void {
            const val = try env.pop(I);
            const res = std.math.negate(val) catch |e| switch (e) {
                error.Overflow => return error.VmIntegerOverflow,
            };
            try env.push(I, res);
        }

        fn eq(env: *Env, _: *State) Error!void {
            try env.push(bool, try env.pop(U) == try env.pop(U));
        }

        fn ne(env: *Env, _: *State) Error!void {
            try env.push(bool, try env.pop(U) != try env.pop(U));
        }

        fn eqz(env: *Env, _: *State) Error!void {
            try env.push(bool, try env.pop(U) == 0);
        }

        fn extend(env: *Env, _: *State) Error!void {
            try env.push(u64, try env.pop(U));
        }

        fn sign_extend(env: *Env, _: *State) Error!void {
            try env.push(i64, try env.pop(I));
        }

        fn sign_narrow(env: *Env, _: *State) Error!void {
            const res = std.math.cast(I, try env.pop(i64)) orelse {
                return Error.VmIntegerOverflow;
            };
            try env.push(I, res);
        }

        fn load(env: *Env, state: *State) Error!void {
            const offset = state.readValue(u16);
            const start = try env.pop([*]U);
            try env.push(U, start[offset]);
        }

        fn store(env: *Env, state: *State) Error!void {
            const offset = state.readValue(u16);
            const data = try env.pop(U);
            const start = try env.pop([*]U);
            start[offset] = data;
        }

        fn jz(env: *Env, state: *State) Error!void {
            const value = try env.pop(U);
            const dest = state.readValue(u32);
            if (value == 0) {
                state.pc = dest;
            }
        }

        fn jnz(env: *Env, state: *State) Error!void {
            const value = try env.pop(U);
            const dest = state.readValue(u32);
            if (value != 0) {
                state.pc = dest;
            }
        }
    };
}
