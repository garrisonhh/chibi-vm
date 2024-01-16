const std = @import("std");
const in_debug = @import("builtin").mode == .Debug;
const objects = @import("objects.zig");
const Label = objects.Builder.Label;

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

    /// read value bytes from code and push to stack
    constant,
    /// read code loc from code and push to stack
    label,
    /// remove a stack value
    drop,

    // twos-complement math
    // some need differentiating between signed and unsigned, some don't
    add,
    sub,
    mulu,
    muli,
    divu,
    divi,
    mod,
    neg,

    // logic
    @"or",
    @"and",
    not,

    // bitwise logic
    bitor,
    bitand,
    bitcom,
    bitxor,

    // conditional logic
    eq,
    ne,
    eqz,

    // number type manipulation
    extend, // u8/u16/u32 -> u64
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
    // 1. reads offset as u16 from bytecode
    // 2. pops data
    // 3. pops pointer
    // 4. writes `width` bytes at `pointer + offset * width`
    // (this arrived unintentionaly super close to the design of x86 mov!)
    store,

    /// reads u32 length, peeks `dst` ptr and writes zeroes to it
    zero,
    /// reads u32 length, pops `src` ptr, peeks `dst` ptr and copies
    copy,

    /// reads u32, and set program counter to the value
    jump,
    /// reads u32, pops sized value, and if it is zero then jump
    jz,
    /// reads u32, pops sized value, and if it's not zero then jump
    jnz,
    /// pops u32 and calls it
    call,

    pub const Meta = struct {
        /// popped values
        inputs: usize,
        /// pushed values
        outputs: usize,
        /// whether this op is generic over width
        sized: bool,
    };

    /// get metadata about opcode execution. this is useful for verification and
    /// other tasks involving processing raw bytecode.
    pub fn meta(opcode: Opcode) Meta {
        return switch (opcode) {
            .halt => .{ .inputs = 0, .outputs = 0, .sized = false },
            .enter => .{ .inputs = 0, .outputs = 0, .sized = false },
            .ret => .{ .inputs = 1, .outputs = 0, .sized = false },
            .constant => .{ .inputs = 0, .outputs = 1, .sized = true },
            .label => .{ .inputs = 0, .outputs = 1, .sized = false },
            .drop => .{ .inputs = 1, .outputs = 0, .sized = false },
            .local => .{ .inputs = 0, .outputs = 1, .sized = false },
            .load => .{ .inputs = 1, .outputs = 1, .sized = true },
            .store => .{ .inputs = 2, .outputs = 0, .sized = true },

            .add,
            .sub,
            .mulu,
            .muli,
            .divu,
            .divi,
            .mod,
            .bitand,
            .bitor,
            .bitxor,
            .eq,
            .ne,
            => .{ .inputs = 2, .outputs = 1, .sized = true },

            .@"and",
            .@"or",
            => .{ .inputs = 2, .outputs = 1, .sized = false },

            .eqz,
            .neg,
            .bitcom,
            .extend,
            .sign_extend,
            .sign_narrow,
            => .{ .inputs = 1, .outputs = 1, .sized = true },

            .not,
            => .{ .inputs = 1, .outputs = 1, .sized = false },

            .zero => .{ .inputs = 1, .outputs = 1, .sized = false },
            .copy => .{ .inputs = 2, .outputs = 1, .sized = false },

            .jump => .{ .inputs = 0, .outputs = 0, .sized = false },
            .jz, .jnz => .{ .inputs = 1, .outputs = 0, .sized = true },
            .call => .{ .inputs = 1, .outputs = 0, .sized = false },
        };
    }
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

    /// finds minimum width wide enough for the number of bytes rather than an
    /// exact match in byte count
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

/// the bytecode form of ops
pub const ByteOp = packed struct(u8) {
    /// this should always be `byte` for ops which are not sized
    width: Width = .byte,
    opcode: Opcode,

    /// how many extra bytes of bytecode this byteop will read
    /// *this is not included in `Opcode.meta` because some ops change their
    /// extra bytes based on their width*
    pub fn extraBytes(bo: ByteOp) usize {
        return switch (bo.opcode) {
            .halt,
            .drop,
            .add,
            .sub,
            .mulu,
            .muli,
            .divu,
            .divi,
            .mod,
            .neg,
            .@"and",
            .@"or",
            .not,
            .bitand,
            .bitor,
            .bitcom,
            .bitxor,
            .eq,
            .ne,
            .eqz,
            .extend,
            .sign_extend,
            .sign_narrow,
            .call,
            => 0,

            .ret => 1,
            .enter,
            .local,
            .load,
            .store,
            => 2,
            .label,
            .zero,
            .copy,
            .jump,
            .jz,
            .jnz,
            => 4,

            .constant => bo.width.bytes(),
        };
    }
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

    pub const CondJmp = struct {
        width: Width,
        dest: Label,
    };

    halt,
    /// data is frame size
    enter: u16,
    /// data is number of parameters
    ret: u8,
    constant: Constant,
    label: Label,
    drop,
    add: Width,
    sub: Width,
    mulu: Width,
    muli: Width,
    divu: Width,
    divi: Width,
    mod: Width,
    neg: Width,
    @"and": Width,
    @"or": Width,
    not: Width,
    bitand: Width,
    bitor: Width,
    bitcom: Width,
    bitxor: Width,
    eq: Width,
    ne: Width,
    eqz: Width,
    extend: Width,
    sign_extend: Width,
    sign_narrow: Width,
    /// data is offset from base pointer
    local: i16,
    load: Address,
    store: Address,
    /// data is number of bytes to zero
    zero: u32,
    /// data is number of bytes to copy
    copy: u32,
    jump: Label,
    jz: CondJmp,
    jnz: CondJmp,
    call,

    pub fn format(
        op: Op,
        comptime _: []const u8,
        _: std.fmt.FormatOptions,
        writer: anytype,
    ) @TypeOf(writer).Error!void {
        try writer.print("{s}", .{@tagName(op)});

        switch (op) {
            inline else => |data| {
                if (@TypeOf(data) != void) {
                    try writer.print(" ", .{});
                }

                switch (@TypeOf(data)) {
                    void => {},
                    u8, u16, i16, u32 => try writer.print("{d}", .{data}),
                    Width => try writer.print("{s}", .{@tagName(data)}),

                    Constant => {
                        const bytes: []const u8 = switch (data) {
                            inline else => |array| &array,
                        };

                        try writer.print("{s} 0x", .{@tagName(data)});
                        for (bytes, 0..) |byte, i| {
                            if (i > 0 and i % 2 == 0) {
                                try writer.print("_", .{});
                            }
                            try writer.print("{x:0>2}", .{byte});
                        }
                    },
                    Address => try writer.print(
                        "{s} {d}",
                        .{ @tagName(data.width), data.offset },
                    ),
                    Label => try writer.print("{}", .{data}),
                    CondJmp => try writer.print(
                        "{s} {}",
                        .{ @tagName(data.width), data.dest },
                    ),

                    else => unreachable,
                }
            },
        }
    }
};
