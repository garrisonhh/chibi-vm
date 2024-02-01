const std = @import("std");

pub const Token = struct {
    pub const Kind = enum {
        lparen,
        rparen,
        word,
    };

    start: usize,
    len: usize,
    kind: Kind,

    pub fn span(tok: Token, text: []const u8) []const u8 {
        return text[tok.start .. tok.start + tok.len];
    }
};

const Lexer = @This();

text: []const u8,
index: usize = 0,

fn isSpace(c: u8) bool {
    return switch (c) {
        ' ', '\t', '\r', '\n' => true,
        else => false,
    };
}

fn isWord(c: u8) bool {
    return switch (c) {
        'a'...'z',
        'A'...'Z',
        '0'...'9',
        '.',
        '-',
        '_',
        => true,
        else => false,
    };
}

fn isValid(c: u8) bool {
    return isWord(c) or isSpace(c) or switch (c) {
        '(', ')' => true,
        else => false,
    };
}

pub fn init(text: []const u8) Lexer {
    return .{ .text = text };
}

pub const Error = error{
    InvalidCharacter,
};

fn peek(lexer: *Lexer) Error!?u8 {
    if (lexer.index == lexer.text.len) {
        return null;
    }

    const ch = lexer.text[lexer.index];
    if (!isValid(ch)) return Error.InvalidCharacter;

    return ch;
}

fn advance(lexer: *Lexer) void {
    std.debug.assert(lexer.index < lexer.text.len);
    lexer.index += 1;
}

fn take(lexer: *Lexer) Error!?u8 {
    const ch = lexer.peek() orelse {
        return null;
    };
    lexer.advance();
    return ch;
}

fn token(lexer: Lexer, kind: Token.Kind, len: usize) Token {
    return Token{
        .kind = kind,
        .start = lexer.index - len,
        .len = len,
    };
}

pub fn next(lexer: *Lexer) Error!?Token {
    // skip spaces
    while (true) {
        const ch = try lexer.peek() orelse break;
        if (!isSpace(ch)) break;
        lexer.advance();
    }

    // get first non-space token
    const fst = try lexer.peek() orelse {
        return null;
    };

    // parens
    switch (fst) {
        inline '(', ')' => |found| {
            lexer.advance();

            const kind: Token.Kind = comptime switch (found) {
                '(' => .lparen,
                ')' => .rparen,
                else => unreachable,
            };

            return lexer.token(kind, 1);
        },
        else => {},
    }

    // words
    var word_len: usize = 0;
    while (true) {
        const ch = try lexer.peek() orelse break;
        if (!isWord(ch)) break;
        lexer.advance();
        word_len += 1;
    }

    return lexer.token(.word, word_len);
}
