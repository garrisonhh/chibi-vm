const std = @import("std");
const sources = @import("sources.zig");
const Loc = sources.Loc;

pub const Error = struct {
    pub const Kind = union(enum) {
        /// lexer encountered invalid byte(s)
        invalid_character,
    };

    loc: Loc,
    kind: Kind,

    pub fn init(loc: Loc, kind: Kind) Error {
        return .{ .loc = loc, .kind = kind };
    }

    fn displayMessage(kind: Kind, writer: anytype) @TypeOf(writer).Error!void {
        switch (kind) {
            inline else => |meta, tag| {
                if (@TypeOf(meta) != void) {
                    @compileError(@tagName(tag) ++ " needs an error message");
                }

                const msg: [@tagName(tag).len]u8 = comptime msg: {
                    var msg = @tagName(tag).*;
                    for (&msg) |*ch| {
                        ch.* = switch (ch.*) {
                            'a'...'z' => |a| a,
                            else => ' '
                        };
                    }

                    break :msg msg;
                };

                try writer.print("{s}", .{msg});
            },
        }
    }

    fn displayContext(loc: Loc, writer: anytype) @TypeOf(writer).Error!void {
        const src_text = sources.get(loc.source).text;
        var lines = std.mem.split(u8, src_text, "\n");
        for (0..loc.line_index) |_| {
            _ = lines.next();
        }

        const line = lines.next().?; // if this fails, line index is invalid

        try writer.print("{s}\n", .{line});
        try writer.writeByteNTimes(' ', loc.char_index);
        try writer.print("^\n", .{});
    }

    pub fn display(err: Error, writer: anytype) @TypeOf(writer).Error!void {
        try writer.print("[{}] error: ", .{err.loc});
        try displayMessage(err.kind, writer);
        try writer.print("\n", .{});
        try displayContext(err.loc, writer);
    }
};