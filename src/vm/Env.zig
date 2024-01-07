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
    VmTraceOverflow,
    VmTraceUnderflow,
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

/// call stack
const Trace = struct {
    const Entry = struct {
        /// previous stack top TODO this might actually be useless lol
        base: usize,
        /// where to return
        ret_loc: usize,
    };

    entries: []Entry,
    top: usize = 0,

    fn peek(self: Trace) ?Entry {
        return if (self.top > 0) self.entries[self.top - 1] else null;
    }

    fn push(self: *Trace, entry: Entry) Error!void {
        if (self.top >= self.entries.len) {
            return Error.VmTraceOverflow;
        }

        self.entries[self.top] = entry;
        self.top += 1;
    }

    fn pop(self: *Trace) Error!Entry {
        return self.peek() orelse Error.VmTraceUnderflow;
    }
};

/// operable value
const Value = packed struct(u64) {
    bytes: u64,

    fn from(comptime T: type, data: T) Value {
        if (comptime @sizeOf(T) > 8) {
            @compileError(@typeName(T) ++ " is more than 8 bytes");
        }

        var v: Value = undefined;
        const dst = std.mem.asBytes(&v);
        const src = std.mem.asBytes(&data);
        @memcpy(dst[0..src.len], src);

        return v;
    }

    fn into(v: Value, comptime T: type) T {
        if (comptime @sizeOf(T) > 8) {
            @compileError(@typeName(T) ++ " is more than 8 bytes");
        }

        var t: T = undefined;
        const dst = std.mem.asBytes(&t);
        const src = std.mem.asBytes(&v);
        @memcpy(dst, src[0..dst.len]);

        return t;
    }
};

/// operation stack
const Stack = struct {
    values: []Value,
    top: usize = 0,

    fn peek(self: Stack) ?Value {
        return if (self.top > 0) self.values[self.top - 1] else null;
    }

    fn push(self: *Stack, v: Value) Error!void {
        if (self.top >= self.values.len) {
            return Error.VmStackOverflow;
        }

        self.values[self.top] = v;
        self.top += 1;
    }

    fn pop(self: *Stack) Error!Value {
        const value = self.peek() orelse {
            return Error.VmStackUnderflow;
        };

        self.top -= 1;
        return value;
    }
};

trace: Trace,
stack: Stack,

pub const Config = struct {
    max_calls: usize = 512,
    max_values: usize = 512,
};

pub fn init(ally: Allocator, cfg: Config) Allocator.Error!Env {
    const entries = try ally.alloc(Trace.Entry, cfg.max_calls);
    const values = try ally.alloc(Value, cfg.max_values);

    return Env{
        .trace = Trace{ .entries = entries },
        .stack = Stack{ .values = values },
    };
}

pub fn deinit(env: *Env, ally: Allocator) void {
    ally.free(env.stack.values);
    ally.free(env.trace.entries);
}

pub fn peek(env: Env, comptime T: type) ?T {
    const v = env.stack.peek() orelse return null;
    return v.into(T);
}

pub fn push(env: *Env, comptime T: type, data: T) Error!void {
    try env.stack.push(Value.from(T, data));
}

pub fn pop(env: *Env, comptime T: type) Error!T {
    const v = try env.stack.pop();
    return v.into(T);
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
    const offset = so.exports.get(name) orelse {
        return ExecError.NoSuchFunction;
    };

    var state = State{
        .code = so.code,
        .pc = offset,
    };

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
        GenericSubs(.byte),
        GenericSubs(.short),
        GenericSubs(.half),
        GenericSubs(.word),
    };

    for (std.enums.values(Opcode)) |opcode| {
        const op_name = @tagName(opcode);

        if (@hasDecl(MonomorphicSubs, op_name)) {
            const func = @field(MonomorphicSubs, op_name);
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
const MonomorphicSubs = struct {
    fn halt(_: *Env, state: *State) Error!void {
        state.pc = state.code.len;
    }

    fn enter(env: *Env, state: *State) Error!void {
        const stack_size = state.readValue(u16);
        try env.trace.push(Trace.Entry{
            .base = env.stack.top,
            .ret_loc = state.pc,
        });

        // TODO write locals to some kind of working memory for the function
        _ = stack_size;
    }

    fn ret(env: *Env, state: *State) Error!void {
        // stop execution if returning from first function
        if (env.trace.top == 0) {
            try halt(env, state);
            return;
        }

        // actually return
        const entry = try env.trace.pop();
        state.pc = entry.ret_loc;
        env.stack.top = entry.base + 1; // + 1 for the return value
    }
};

/// subroutines for opcodes which are generic over width
fn GenericSubs(comptime W: Width) type {
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
    };
}
