const std = @import("std");
const sources = @import("sources.zig");
const Source = sources.Source;
const Loc = sources.Loc;

const Symbol = struct {
    str: []const u8,
    tag: Token.Tag,
};

const symbols = [_]Symbol{
    .{ .str = "&", .tag = .ampersand },
    .{ .str = "{", .tag = .lcurly },
    .{ .str = "}", .tag = .rcurly },
    .{ .str = "(", .tag = .lparen },
    .{ .str = ")", .tag = .rparen },
    .{ .str = "=", .tag = .equals },
    .{ .str = "->", .tag = .arrow },
    .{ .str = ":", .tag = .colon },
    .{ .str = ";", .tag = .semicolon },
    .{ .str = ".", .tag = .dot },
    .{ .str = ",", .tag = .comma },
    .{ .str = "-", .tag = .minus },
    .{ .str = "+", .tag = .plus },
    .{ .str = "*", .tag = .star },
    .{ .str = "/", .tag = .slash },
    .{ .str = "%", .tag = .percent },
    .{ .str = "==", .tag = .eq },
};

const keywords = [_]Token.Tag{
    .char,
    .short,
    .int,
    .long,
    .float,
    .double,
    .@"return",
    .@"if",
    .@"else",
};

pub const Error = error{InvalidInput};

pub const Token = struct {
    const Self = @This();

    pub const Tag = enum {
        ident,
        int_lit,
        float_lit,
        true,
        false,

        char,
        short,
        int,
        long,
        float,
        double,
        @"return",
        @"if",
        @"else",

        lparen,
        rparen,
        lcurly,
        rcurly,

        ampersand,
        equals,
        arrow,
        colon,
        semicolon,
        dot,
        comma,
        minus,
        plus,
        star,
        slash,
        percent,
        eq,
    };

    /// starting location
    loc: Loc,
    tag: Tag,
    start: usize,
    stop: usize,

    pub fn slice(self: Self) []const u8 {
        return sources.get(self.loc.source).text[self.start..self.stop];
    }

    pub fn format(
        self: Self,
        comptime _: []const u8,
        _: std.fmt.FormatOptions,
        writer: anytype,
    ) @TypeOf(writer).Error!void {
        try writer.print("<{} {s}>", .{ self.loc, @tagName(self.tag) });
    }
};

const Lexer = @This();

text: []const u8,
index: usize,
/// the location of the peeked/next token
loc: Loc,

pub fn init(source: Source) Lexer {
    const text = sources.get(source).text;
    return .{
        .text = text,
        .index = 0,
        .loc = .{
            .source = source,
            .line_index = 0,
            .char_index = 0,
        },
    };
}

// wrapped codepoint iterator ==================================================

fn peek(self: *Lexer) ?u8 {
    if (self.index >= self.text.len) return null;
    return self.text[self.index];
}

fn peekSlice(self: *Lexer, len: usize) ?[]const u8 {
    if (self.index + len > self.text.len) return null;
    return self.text[self.index..self.index + len];
}

fn advance(self: *Lexer) void {
    std.debug.assert(self.index < self.text.len);
    const ch = self.text[self.index];
    self.index += 1;

    if (ch == '\n') {
        self.loc.line_index += 1;
        self.loc.char_index = 0;
    } else {
        self.loc.char_index += 1;
    }
}

fn advanceTimes(self: *Lexer, times: usize) void {
    // TODO this could be more efficient lol
    for (times) |_| self.advance();
}

// classification ==============================================================

fn isSpace(c: u8) bool {
    return switch (c) {
        ' ', '\r', '\n', '\t' => true,
        else => false,
    };
}

fn isWord(c: u8) bool {
    return switch (c) {
        'a'...'z',
        'A'...'Z',
        '0'...'9',
        '_',
        '$',
        => true,
        else => false,
    };
}

fn isDigit(c: u8) bool {
    return switch (c) {
        '0'...'9' => true,
        else => false,
    };
}

fn isIdentStart(c: u8) bool {
    return isIdentTail(c) and !isDigit(c);
}

fn isIdentTail(c: u8) bool {
    return isWord(c);
}

// tokenization ================================================================

fn skipSpaces(self: *Lexer) Error!void {
    while (self.peek()) |pk| {
        if (!isSpace(pk)) return;
        self.advance();
    }
}

/// iterate to find the next token
pub fn next(self: *Lexer) Error!?Token {
    try self.skipSpaces();

    const start_loc = self.loc;
    const start_index = self.index;
    const start_ch = self.peek() orelse return null;

    const tag: Token.Tag = for (symbols) |sym| {
        // symbols
        const got = self.peekSlice(sym.str.len) orelse continue;

        if (std.mem.eql(u8, sym.str, got)) {
            self.advanceTimes(got.len);
            break sym.tag;
        }
    } else if (isIdentStart(start_ch)) tok: {
        // identifiers
        self.advance();
        while (self.peek()) |inner_ch| {
            if (!isIdentTail(inner_ch)) break;
            self.advance();
        }

        const text = self.text[start_index..self.index];
        for (keywords) |kw| {
            if (std.mem.eql(u8, @tagName(kw), text)) {
                break :tok kw;
            }
        }

        break :tok .ident;
    } else if (isDigit(start_ch)) tok: {
        self.advance();

        // integral
        while (self.peek()) |inner_ch| {
            if (!isDigit(inner_ch)) break;
            self.advance();
        }

        const dot_ch = self.peek() orelse {
            break :tok .int_lit;
        };
        if (dot_ch != '.') break :tok .int;
        self.advance();

        // fractional
        while (self.peek()) |inner_ch| {
            if (!isDigit(inner_ch)) break;
            self.advance();
        }

        break :tok .float_lit;
    } else {
        std.debug.print("invalid input: `{c}`\n", .{start_ch});
        return Error.InvalidInput;
    };

    return Token{
        .loc = start_loc,
        .tag = tag,
        .start = start_index,
        .stop = self.index,
    };
}
