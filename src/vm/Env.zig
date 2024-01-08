//! the runtime environment for code execution

const std = @import("std");
const Allocator = std.mem.Allocator;
const ops = @import("ops.zig");
const ByteOp = ops.ByteOp;
const Opcode = ops.Opcode;
const Width = ops.Width;
const objects = @import("objects.zig");
const SharedObject = objects.SharedObject;

const Env = @This();

pub const Error = error{
    VmStackOverflow,
    VmStackUnderflow,
};

const State = struct {
    /// loaded code
    code: []const u8,
    /// program counter
    pc: usize,

    fn readByte(state: *State) u8 {
        defer state.pc += 1;
        return state.code[state.pc];
    }

    fn readNBytes(state: *State, comptime N: comptime_int) [N]u8 {
        defer state.pc += N;
        const slice = state.code[state.pc .. state.pc + N];
        const ptr: *const [N]u8 = @ptrCast(slice.ptr);
        return ptr.*;
    }

    fn readValue(state: *State, comptime T: type) T {
        return @bitCast(state.readNBytes(@sizeOf(T)));
    }
};

/// operation stack
const Stack = struct {
    mem: []align(8) u8,
    base: [*]align(8) u8,
    top: [*]align(8) u8,

    const Trace = struct {
        base: [*]align(8) u8,
        pc: usize,
    };

    fn init(mem: []align(8) u8) Stack {
        return Stack{
            .mem = mem,
            .base = mem.ptr,
            .top = mem.ptr,
        };
    }

    /// total stack used
    fn used(self: Stack) usize {
        return @intFromPtr(self.top) - @intFromPtr(self.mem.ptr);
    }

    /// size of T when accounting for 8-bit alignment
    fn aligned8Size(comptime T: type) comptime_int {
        comptime {
            return std.mem.alignForward(usize, @sizeOf(T), 8);
        }
    }

    fn reserve(self: *Stack, nbytes: usize) Error!void {
        const sz = std.mem.alignForward(usize, nbytes, 8);
        if (self.used() + sz > self.mem.len) {
            return Error.VmStackOverflow;
        }

        self.top = @ptrFromInt(@intFromPtr(self.top) + sz);
    }

    fn peek(self: Stack, comptime T: type) ?T {
        const sz = aligned8Size(T);
        if (self.used() < sz) return null;

        const ptr: *const T = @ptrFromInt(@intFromPtr(self.top) - sz);
        return ptr.*;
    }

    fn push(self: *Stack, comptime T: type, value: T) Error!void {
        const sz = aligned8Size(T);
        if (self.used() + sz > self.mem.len) {
            return Error.VmStackOverflow;
        }

        @as(*T, @ptrCast(self.top)).* = value;
        self.top = @ptrFromInt(@intFromPtr(self.top) + sz);
    }

    fn pop(self: *Stack, comptime T: type) Error!T {
        const value = self.peek(T) orelse {
            return Error.VmStackUnderflow;
        };

        const sz = aligned8Size(T);
        self.top = @ptrFromInt(@intFromPtr(self.top) - sz);
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

pub fn peek(env: Env, comptime T: type) ?T {
    return env.stack.peek(T);
}

pub fn push(env: *Env, comptime T: type, data: T) Error!void {
    try env.stack.push(T, data);
}

pub fn pop(env: *Env, comptime T: type) Error!T {
    return env.stack.pop(T);
}

fn call(env: *Env, state: *State, dest: usize) Error!void {
    env.stack.base = env.stack.top;
    try env.push(Stack.Trace, .{
        .base = env.stack.top,
        .pc = state.pc,
    });
    state.pc = dest;
}

/// reads i16 offset and returns pointer to it (used for get_local and set_local)
fn localPtr(env: *Env, state: *State, comptime T: type) *T {
    const offset = state.readValue(i16);
    const abs_offset: usize = std.math.absCast(offset);

    const base = @intFromPtr(env.stack.base);
    const addr = if (offset < 0) base - abs_offset else base + abs_offset;
    const ptr: *T = @ptrFromInt(addr);

    // TODO verify addr

    return ptr;
}

// execution ===================================================================

pub const ExecError = Allocator.Error || Error || error{
    NoSuchFunction,
    InvalidByteOp,
};

/// execute code exported from a shared object
pub fn exec(
    env: *Env,
    so: *const SharedObject,
    name: []const u8,
) ExecError!void {
    const start = so.exports.get(name) orelse {
        return ExecError.NoSuchFunction;
    };

    // start in a halted state so that returning from the first function will
    // halt execution
    var state = State{
        .code = so.code,
        .pc = so.code.len,
    };
    try env.call(&state, start);

    // execute ops until halted
    while (state.pc < state.code.len) {
        const byte = state.readByte();
        const sub = byte_subs[byte] orelse {
            return ExecError.InvalidByteOp;
        };

        try sub(env, &state);
    }
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

        if (@hasDecl(monomorphic_subs, op_name)) {
            const func = @field(monomorphic_subs, op_name);
            const bo = ByteOp{ .opcode = opcode };

            arr[@as(u8, @bitCast(bo))] = &func;
        } else for (generic_sub_namespaces) |ns| {
            if (!@hasDecl(ns, op_name)) continue;

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
    fn halt(_: *Env, state: *State) Error!void {
        state.pc = state.code.len;
    }

    fn enter(env: *Env, state: *State) Error!void {
        const stack_size = state.readValue(u16);
        try env.stack.reserve(stack_size);
    }

    fn ret(env: *Env, state: *State) Error!void {
        const return_value = try env.pop(u64);

        const trace: *const Stack.Trace = @ptrCast(env.stack.base);
        env.stack.top = env.stack.base;
        env.stack.base = trace.base;
        state.pc = trace.pc;

        try env.push(u64, return_value);
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

        fn get_local(env: *Env, state: *State) Error!void {
            const ptr = env.localPtr(state, U);
            try env.push(U, ptr.*);
        }

        fn set_local(env: *Env, state: *State) Error!void {
            const ptr = env.localPtr(state, U);
            ptr.* = try env.pop(U);
        }

        fn add(env: *Env, _: *State) Error!void {
            try env.push(U, try env.pop(U) +% try env.pop(U));
        }

        fn sub(env: *Env, _: *State) Error!void {
            try env.push(U, try env.pop(U) -% try env.pop(U));
        }

        fn mulu(env: *Env, _: *State) Error!void {
            try env.push(U, try env.pop(U) *% try env.pop(U));
        }

        fn muli(env: *Env, _: *State) Error!void {
            try env.push(I, try env.pop(I) *% try env.pop(I));
        }

        fn divu(env: *Env, _: *State) Error!void {
            try env.push(U, try env.pop(U) / try env.pop(U));
        }

        fn divi(env: *Env, _: *State) Error!void {
            try env.push(I, @divFloor(try env.pop(I), try env.pop(I)));
        }

        fn mod(env: *Env, _: *State) Error!void {
            try env.push(U, try env.pop(U) % try env.pop(U));
        }

        fn neg(env: *Env, _: *State) Error!void {
            try env.push(I, -try env.pop(I));
        }
    };
}
