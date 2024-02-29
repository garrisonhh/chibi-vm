const std = @import("std");
const sources = @import("sources.zig");
const Source = sources.Source;
const Loc = sources.Loc;

const Symbol = struct {
    str: []const u8,
    tag: Token.Tag,
};

const c_symbols = [_]Symbol{
    .{ .str = "==", .tag = .eq },
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
};

const pp_symbols = [_]Symbol{
    .{ .str = "##", .tag = .hash_hash },
    .{ .str = "#", .tag = .hash },
};

const c_keywords = [_]Token.Tag{
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

const pp_keywords = [_]Token.Tag{
    .include,
    .define,
    .undef,
};

pub const Error = error{
    InvalidInput,
    UnfinishedString,
    UnfinishedInclude,
};

pub const Token = struct {
    const Self = @This();

    pub const Tag = enum {
        ident,
        int_lit,
        float_lit,
        string_lit,
        /// include paths like `<stdio.h>`
        include_lit,

        // keywords
        char,
        short,
        int,
        long,
        float,
        double,
        @"return",
        @"if",
        @"else",
        include,
        define,
        undef,

        // symbols
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
        hash,
        hash_hash,
    };

    /// starting location
    loc: Loc,
    tag: Tag,
    start: usize,
    stop: usize,

    pub fn slice(self: Self) []const u8 {
        return sources.get(self.loc.source).text[self.start..self.stop];
    }

    /// location of the end of the token
    pub fn end(self: Self) Loc {
        var loc = self.loc;
        loc.char_index += @intCast(self.stop - self.start);
        return loc;
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

const State = union(enum) {
    /// next iteration must check for preprocessor vs. c
    newline,
    /// parse line of c
    c,
    /// parse line of preprocessor c
    pp,
};

text: []const u8,
index: usize,
/// the location of the peeked/next token
loc: Loc,
state: State = .newline,

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
    return self.text[self.index .. self.index + len];
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

fn lexSymbol(self: *Lexer, symbols: []const Symbol) ?Token.Tag {
    for (symbols) |sym| {
        const got = self.peekSlice(sym.str.len) orelse continue;
        if (std.mem.eql(u8, sym.str, got)) {
            self.advanceTimes(got.len);
            return sym.tag;
        }
    }

    return null;
}

fn lexKeyword(self: *Lexer, keywords: []const Token.Tag) ?Token.Tag {
    const start_index = self.index;

    const start_ch = self.peek() orelse return null;
    if (!isIdentStart(start_ch)) return null;

    self.advance();
    while (self.peek()) |inner_ch| {
        if (!isIdentTail(inner_ch)) break;
        self.advance();
    }

    const text = self.text[start_index..self.index];
    for (keywords) |kw| {
        if (std.mem.eql(u8, @tagName(kw), text)) {
            return kw;
        }
    }

    return .ident;
}

fn lexNumber(self: *Lexer) ?Token.Tag {
    const start_ch = self.peek() orelse return null;
    if (!isDigit(start_ch)) return null;
    self.advance();

    // integral
    while (self.peek()) |inner_ch| {
        if (!isDigit(inner_ch)) break;
        self.advance();
    }

    const dot_ch = self.peek() orelse return .int_lit;
    if (dot_ch != '.') return .int_lit;
    self.advance();

    // fractional
    while (self.peek()) |inner_ch| {
        if (!isDigit(inner_ch)) break;
        self.advance();
    }

    return .float_lit;
}

fn lexString(self: *Lexer) Error!?Token.Tag {
    const start_ch = self.peek() orelse return null;
    if (start_ch != '"') return null;
    self.advance();

    var esc = false;
    while (true) {
        const inner_ch = self.peek() orelse {
            return Error.UnfinishedString;
        };
        self.advance();

        if (inner_ch == '\\') {
            esc = true;
            continue;
        }

        if (inner_ch == '"' and !esc) {
            break;
        }

        esc = false;
    }

    return .string_lit;
}

fn lexIncludeString(self: *Lexer) Error!?Token.Tag {
    const start_ch = self.peek() orelse return null;
    if (start_ch != '<') return null;
    self.advance();

    while (true) {
        const inner_ch = self.peek() orelse {
            return Error.UnfinishedInclude;
        };
        self.advance();

        if (inner_ch == '>') break;
    }

    return .include_lit;
}

fn lexC(self: *Lexer) Error!Token.Tag {
    return self.lexSymbol(&c_symbols) orelse
        self.lexKeyword(&c_keywords) orelse
        self.lexNumber() orelse
        try self.lexString() orelse
        Error.InvalidInput;
}

fn lexPp(self: *Lexer) Error!Token.Tag {
    return self.lexSymbol(&(pp_symbols ++ c_symbols)) orelse
        self.lexKeyword(&(pp_keywords ++ c_keywords)) orelse
        self.lexNumber() orelse
        try self.lexString() orelse
        try self.lexIncludeString() orelse
        Error.InvalidInput;
}

fn skipNonNewlineSpaces(self: *Lexer) void {
    while (self.peek()) |pk| {
        if (!isSpace(pk)) return;
        self.advance();
    }
}

/// iterate to find the next token
pub fn next(self: *Lexer) Error!?Token {
    // skip spaces
    self.skipNonNewlineSpaces();
    while (self.peek() == @as(u8, '\n')) {
        self.state = .newline;
        self.skipNonNewlineSpaces();
    }

    // check eof
    const pk = self.peek() orelse return null;

    // check mode if required
    if (self.state == .newline) {
        self.state = if (pk == '#') .pp else .c;
    }

    const start_loc = self.loc;
    const start_index = self.index;
    const tag = switch (self.state) {
        .newline => unreachable,
        .c => try self.lexC(),
        .pp => try self.lexPp(),
    };

    return Token{
        .loc = start_loc,
        .tag = tag,
        .start = start_index,
        .stop = self.index,
    };
}
