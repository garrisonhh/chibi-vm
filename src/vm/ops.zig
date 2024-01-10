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
        const MetaTuple = struct {
            @"0": usize,
            @"1": usize,
            @"2": bool,
        };

        const tup: MetaTuple = switch (opcode) {
            .halt => .{ 0, 0, false },
            .enter => .{ 0, 0, false },
            .ret => .{ 1, 0, false },
            .constant => .{ 0, 1, true },
            .drop => .{ 1, 0, false },
            .local => .{ 0, 1, false },
            .load => .{ 1, 1, true },
            .store => .{ 2, 0, true },

            .add,
            .sub,
            .mulu,
            .muli,
            .divu,
            .divi,
            .mod,
            => .{ 2, 1, true },
            .neg => .{ 1, 1, true },

            .sign_extend,
            .sign_narrow,
            => .{ 1, 1, true },

            .zero => .{ 1, 1, false },
            .copy => .{ 2, 1, false },
        };

        return Meta{
            .inputs = tup.@"0",
            .outputs = tup.@"1",
            .sized = tup.@"2",
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
    width: Width = .byte,
    opcode: Opcode,

    /// how many extra bytes of bytecode this byteop will read
    /// *this is not included in `Opcode.meta` because some ops change their
    /// extra bytes based on their width*
    pub fn extraBytes(bo: ByteOp) usize {
        return switch (bo.opcode) {
            .constant => bo.width.bytes(),

            .ret => 1,
            .enter,
            .local,
            .load,
            .store,
            => 2,
            .zero, .copy => 4,

            else => 0,
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
    store: Address,
    /// data is number of bytes to zero
    zero: u32,
    /// data is number of bytes to copy
    copy: u32,

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

                    else => unreachable,
                }
            },
        }
    }
};

// utilities ===================================================================

fn sliceTo(comptime T: type, slice: []const u8) T {
    std.debug.assert(slice.len == @sizeOf(T));
    const ptr: *const [@sizeOf(T)]u8 = @ptrCast(slice.ptr);
    return @bitCast(ptr.*);
}

/// reads the first op from the bytecode. if you provide an nbytes pointer, it
/// will write the length read to it.
fn firstOpAdvanced(code: []const u8, nbytes: ?*usize) Op {
    const byteop: ByteOp = @bitCast(code[0]);
    const extra_bytes = byteop.extraBytes();
    const extra = code[1..1 + extra_bytes];

    if (nbytes) |out| {
        out.* = 1 + extra_bytes;
    }

    return switch (byteop.opcode) {
        // void
        inline .halt, .drop => |tag| @unionInit(Op, @tagName(tag), {}),

        // width
        inline .add,
        .sub,
        .mulu,
        .muli,
        .divu,
        .divi,
        .mod,
        .neg,
        .sign_extend,
        .sign_narrow,
        => |tag| @unionInit(Op, @tagName(tag), byteop.width),

        // some int or raw value
        inline .enter, .ret, .local, .zero, .copy => |tag| raw: {
            const name = @tagName(tag);

            var op = @unionInit(Op, name, undefined);
            const T = @TypeOf(@field(op, name));
            @field(op, name) = sliceTo(T, extra);

            break :raw op;
        },

        // special
        .constant => Op{
            .constant = switch (byteop.width) {
                inline else => |w| @unionInit(
                    Op.Constant,
                    @tagName(w),
                    sliceTo([w.bytes()]u8, extra),
                ),
            },
        },
        inline .load, .store => |tag| @unionInit(Op, @tagName(tag), Op.Address{
            .width = byteop.width,
            .offset = sliceTo(u16, extra),
        }),
    };
}

pub fn firstOp(code: []const u8) Op {
    return firstOpAdvanced(code, null);
}

pub fn iterateBytecode(code: []const u8) BytecodeIterator {
    return .{ .code = code };
}

pub const BytecodeIterator = struct {
    code: []const u8,

    pub fn next(iter: *BytecodeIterator) ?Op {
        if (iter.code.len == 0) {
            return null;
        }

        var nbytes: usize = undefined;
        const op = firstOpAdvanced(iter.code, &nbytes);
        iter.code = iter.code[nbytes..];

        return op;
    }
};