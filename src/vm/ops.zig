const std = @import("std");

pub const Opcode = enum(u6) {
    halt = 0,

    /// reads a u16 for the stack size, and adds it to the stack pointer
    enter,

    /// reads a u8 for the number of parameters to pop
    /// exits a function:
    /// 1. pops the return value on the operand stack
    /// 2. returns execution to the previous instruction and frame
    /// 3. removes the parameters provided
    /// 4. pushes the return value back to the operand stack
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

    // 1. reads offset as i16 from bytecode
    // 2. pushes `base_pointer + offset` to stack
    local,

    // 1. reads offset as u16 from bytecode
    // 2. pops pointer
    // 3. derefs `width` bytes at `pointer + offset * width` and pushes
    // (this arrived unintentionaly super close to the design of x86 mov!)
    load,
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

    pub fn fromBytesExact(nbytes: usize) ?Width {
        return switch (nbytes) {
            1 => .byte,
            2 => .short,
            4 => .half,
            8 => .word,
            else => null,
        };
    }

    pub fn fromBytesFit(nbytes: usize) ?Width {
        return switch (nbytes) {
            1 => .byte,
            2 => .short,
            3, 4 => .half,
            5, 6, 7, 8 => .word,
            else => null,
        };
    }
};

/// meta description of ops for verification
pub const Meta = struct {
    inputs: usize,
    outputs: usize,
    // additional bytes read from the bytecode
    // TODO reads: usize,
};

/// high level op description (compiled into bytecode)
pub const Op = union(Opcode) {
    pub const Constant = union(Width) {
        byte: [1]u8,
        short: [2]u8,
        half: [4]u8,
        word: [8]u8,
    };

    pub const Address = struct {
        width: Width,
        /// offset from the pointer
        offset: u16,
    };

    halt,
    /// data is frame size
    enter: u16,
    /// data is number of parameters
    ret: u8,
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
    /// data is offset from base pointer
    local: i16,
    load: Address,

    /// get the opcode meta description for code verification
    /// *works in comptime*
    pub fn meta(o: Op) Meta {
        const StackEffectTuple = struct {
            @"0": usize,
            @"1": usize,
        };

        const tup: StackEffectTuple = switch (o) {
            .halt,
            .enter,
            .ret,
            => .{ 0, 0 },
            .constant,
            .local,
            => .{ 0, 1 },
            .drop,
            => .{ 1, 0 },
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

        return Meta{
            .inputs = tup.@"0",
            .outputs = tup.@"1",
        };
    }
};

/// the bytecode form of ops
pub const ByteOp = packed struct(u8) {
    width: Width = .byte,
    opcode: Opcode,

    pub fn format(
        self: @This(),
        comptime _: []const u8,
        _: std.fmt.FormatOptions,
        writer: anytype,
    ) @TypeOf(writer).Error!void {
        try writer.print("{s} {s}", .{
            @tagName(self.opcode),
            @tagName(self.width),
        });
    }
};
