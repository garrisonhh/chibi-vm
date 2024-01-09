//! the runtime environment for code execution

const std = @import("std");
const Allocator = std.mem.Allocator;
const ops = @import("ops.zig");
const ByteOp = ops.ByteOp;
const Opcode = ops.Opcode;
const Width = ops.Width;
const objects = @import("objects.zig");
const SharedObject = objects.SharedObject;

// utils =======================================================================

fn ptrAdd(comptime T: type, ptr: T, offset: isize) T {
    std.debug.assert(@typeInfo(T) == .Pointer);

    const iaddr: isize = @intCast(@intFromPtr(ptr));
    const offset_addr: usize = @intCast(iaddr + offset);
    return @ptrFromInt(offset_addr);
}

// env =========================================================================

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

/// needed for function codegen
pub const frame_size = Stack.aligned8Size(Stack.Frame);

/// operation stack
const Stack = struct {
    mem: []align(8) u8,
    base: [*]align(8) u8,
    top: [*]align(8) u8,

    /// saved execution frame of a previous call
    const Frame = struct {
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

    /// reserve some bytes from the stack
    fn reserve(self: *Stack, nbytes: usize) Error!void {
        const sz = std.mem.alignForward(usize, nbytes, 8);
        if (self.used() + sz > self.mem.len) {
            return Error.VmStackOverflow;
        }

        self.top = ptrAdd([*]align(8) u8, self.top, @intCast(sz));
    }

    /// drop some bytes from the stack
    fn vacate(self: *Stack, nbytes: usize) Error!void {
        const sz = std.mem.alignForward(usize, nbytes, 8);
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
    try env.push(Stack.Frame, .{
        .base = env.stack.top,
        .pc = state.pc,
    });
    env.stack.base = env.stack.top;
    state.pc = dest;
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
            const byte_op: ByteOp = @bitCast(byte);
            std.debug.print("couldn't find byte op: {}\n", .{byte_op});
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
        const ret_params: usize = state.readValue(u8);

        // get return value
        const return_value = try env.pop([8]u8);

        // revert frame and pc
        env.stack.top = env.stack.base;
        const frame = try env.pop(Stack.Frame);
        env.stack.base = frame.base;
        state.pc = frame.pc;

        // remove params
        try env.stack.vacate(ret_params * 8);

        // push back return value
        try env.push([8]u8, return_value);
    }

    fn drop(env: *Env, _: *State) Error!void {
        _ = try env.pop(u8);
    }

    fn local(env: *Env, state: *State) Error!void {
        const offset = state.readValue(i16);
        const ptr = ptrAdd(*anyopaque, env.stack.base, offset);
        try env.push(*anyopaque, ptr);
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

        fn load(env: *Env, state: *State) Error!void {
            const offset = state.readValue(i16);
            const start = try env.pop(*U);
            const ptr = ptrAdd(*U, start, offset);
            try env.push(U, ptr.*);
        }
    };
}
