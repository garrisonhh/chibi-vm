const std = @import("std");

pub const Opcode = enum(u6) {
    halt = 0,
    // set up a function. reads 2 bytes as u16 for the stack size
    enter,
    // drop the function stack and return
    ret,

    // read value bytes from code and push to stack
    constant,
    // remove a stack value
    drop,

    // binary twos-complement 64-bit math
    // some need differentiating between signed and unsigned, some don't
    add,
    sub,
    mulu,
    muli,
    divu,
    divi,
    mod,

    // unary math
    neg,

    // number type manipulation
    sign_extend, // i8/i16/i32 -> i64
    sign_narrow, // i64 -> i8/i16/i32

    // load,
    // store,
    // read,
    // write,
};

/// used to encode the width of basic operations
pub const Width = enum(u2) {
    byte,
    short,
    half,
    word,

    pub fn bytes(w: Width) u4 {
        return @as(u4, 1) << @intFromEnum(w);
    }

    pub fn fromBytes(nbytes: u4) Width {
        inline for (comptime std.enums.values(Width)) |w| {
            if (w.bytes() == nbytes) {
                return w;
            }
        } else {
            std.debug.panic("invalid number of bytes: {}\n", .{nbytes});
        }
    }
};

/// meta description of ops for verification
pub const StackEffect = struct {
    inputs: usize,
    outputs: usize,
};

/// high level op description (compiled into bytecode)
pub const Op = union(Opcode) {
    pub const Constant = union(Width) {
        byte: [1]u8,
        short: [2]u8,
        half: [4]u8,
        word: [8]u8,
    };

    halt,
    enter: usize,
    ret,
    constant: Constant,
    drop,
    add: Width,
    sub: Width,
    mulu: Width,
    muli: Width,
    divu: Width,
    divi: Width,
    mod: Width,
    neg: Width,
    sign_extend: Width,
    sign_narrow: Width,

    /// get the opcode meta description for code verification
    /// *works in comptime*
    pub fn stackEffect(o: Op) StackEffect {
        const StackEffectTuple = struct {
            @"0": usize,
            @"1": usize,
        };

        const tup: StackEffectTuple = switch (o) {
            .halt,
            .enter,
            .ret,
            => .{ 0, 0 },
            .constant => .{ 0, 1 },
            .drop => .{ 1, 0 },
            .neg,
            .sign_extend,
            .sign_narrow,
            => .{ 1, 1 },
            .add,
            .sub,
            .mulu,
            .muli,
            .divu,
            .divi,
            .mod,
            => .{ 2, 1 },
        };

        return StackEffect{
            .inputs = tup.@"0",
            .outputs = tup.@"1",
        };
    }
};

/// the bytecode form of ops
pub const ByteOp = packed struct(u8) {
    width: Width = .byte,
    opcode: Opcode,
};
