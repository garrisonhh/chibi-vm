const std = @import("std");
const Allocator = std.mem.Allocator;
const sources = @import("sources.zig");
const Loc = sources.Loc;

pub const Error = struct {
    pub const Kind = union(enum) {
        unexpected_expression,

        // lex/preprocess errors
        invalid_character,
        unfinished_string,
        unfinished_include,
        unsupported_preprocessor_directive,
        expected_include_path,
        included_self,
        included_file_not_found,
        error_directive,

        // parse errors
        expected_rparen,
        expected_semicolon,
        expected_block_or_value,
        expected_declspec,
        extra_basic_type,
        extra_type_qualifier,
        extra_signedness,
        expected_declarator,
    };

    loc: Loc,
    kind: Kind,

    pub fn init(loc: Loc, kind: Kind) Error {
        return Error{
            .loc = loc,
            .kind = kind,
        };
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
                            else => ' ',
                        };
                    }

                    break :msg msg;
                };

                try writer.print("{s}", .{msg});
            },
        }
    }

    fn displayContext(loc: Loc, writer: anytype) @TypeOf(writer).Error!void {
        const src_text = loc.source.get().text;
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

pub const ErrorBuffer = struct {
    const Self = @This();

    errors: std.ArrayList(Error),

    pub fn init(ally: Allocator) Self {
        return Self{
            .errors = std.ArrayList(Error).init(ally),
        };
    }

    pub fn deinit(self: Self) void {
        self.errors.deinit();
    }

    pub fn add(self: *Self, loc: Loc, kind: Error.Kind) Allocator.Error!void {
        try self.errors.append(Error.init(loc, kind));
    }

    pub fn hasErrors(self: Self) bool {
        return self.errors.items.len > 0;
    }

    pub fn clear(self: *Self) void {
        self.errors.resize(0) catch unreachable;
    }

    pub fn display(self: Self, writer: anytype) @TypeOf(writer).Error!void {
        for (self.errors.items) |err| {
            try err.display(writer);
        }
    }
};
