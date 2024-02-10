const std = @import("std");
const Allocator = std.mem.Allocator;
const Lexer = @import("Lexer.zig");
const mini = @import("mini.zig");
const Name = mini.Name;

pub const SExpr = struct {
    const Self = @This();
    pub const Kind = std.meta.Tag(Data);

    pub const Syntax = enum {
        true,
        false,
        def,
        lambda,
        @"if",
        @"->",
        @"+",
        @"-",
        @"*",
    };

    pub const Data = union(enum) {
        syntax: Syntax,
        ident: mini.String,
        int: []const u8,
        float: []const u8,
        list: []const SExpr,
    };

    span_start: usize,
    span_len: usize,
    data: Data,

    pub fn span(self: Self, text: []const u8) []const u8 {
        return text[self.span_start .. self.span_start + self.span_len];
    }

    pub fn isSyntax(self: Self, syntax: Syntax) bool {
        return self.data == .syntax and self.data.syntax == syntax;
    }

    /// if this sexpr is an application of syntax
    pub fn isSyntaxApp(self: Self, syntax: Syntax) bool {
        return self.data == .list and
            self.data.list.len > 0 and
            self.data.list[0].isSyntax(syntax);
    }

    pub fn format(
        self: Self,
        comptime _: []const u8,
        _: std.fmt.FormatOptions,
        writer: anytype,
    ) @TypeOf(writer).Error!void {
        switch (self.data) {
            .syntax => |syntax| {
                try writer.print("{s}", .{@tagName(syntax)});
            },
            .ident => |str| {
                try writer.print("<{s}>{}", .{ @tagName(self.data), str });
            },
            .int, .float => |str| {
                try writer.print("<{s}>{s}", .{ @tagName(self.data), str });
            },
            .list => |children| {
                try writer.print("(", .{});
                for (children, 0..) |child, i| {
                    if (i > 0) try writer.print(" ", .{});
                    try writer.print("{}", .{child});
                }
                try writer.print(")", .{});
            },
        }
    }
};

/// ast represents the syntax for a single file
pub const Ast = struct {
    const Self = @This();

    pub const SyntaxError = struct {
        pub const Kind = enum {
            unexpected_eof,
            invalid_character,
            invalid_word,
            unmatched_rparen,
            unfinished_list,
        };

        kind: Kind,
        start: usize,
        len: usize,
    };

    ally: Allocator,
    /// used for building exprs
    arena: std.heap.ArenaAllocator,

    // this is set when a syntax error is produced
    err: ?SyntaxError = null,

    /// string filename of source
    filename: []const u8,
    /// content of source
    text: []const u8,
    /// mini name of source
    name: Name,
    /// top level program
    toplevel: std.ArrayListUnmanaged(SExpr) = .{},

    fn init(ally: Allocator, filename: []const u8, text: []const u8) Self {
        return Self{
            .ally = ally,
            .arena = std.heap.ArenaAllocator.init(ally),
            .filename = filename,
            .text = text,
            .name = nameOfFilename(filename),
        };
    }

    pub fn deinit(self: *Self) void {
        self.toplevel.deinit(self.ally);
        self.arena.deinit();
    }

    fn nameOfFilename(filename: []const u8) Name {
        const basename = std.fs.path.basename(filename);
        const ext_index = std.mem.indexOf(u8, basename, ".") orelse basename.len;
        const base = basename[0..ext_index];
        return mini.name(null, mini.string(base));
    }

    fn syntaxError(
        self: *Self,
        kind: SyntaxError.Kind,
        start: usize,
        len: usize,
    ) void {
        std.debug.assert(self.err == null);
        self.err = SyntaxError{
            .kind = kind,
            .start = start,
            .len = len,
        };
    }
};

// recursive descent ===========================================================

pub const Error = Allocator.Error;

fn isDigit(ch: u8) bool {
    return ch >= '0' and ch <= '9';
}

fn isNumeric(ch: u8) bool {
    return isDigit(ch) or ch == '_';
}

fn isIdentStart(ch: u8) bool {
    return switch (ch) {
        'a'...'z',
        'A'...'Z',
        => true,
        else => Lexer.isSymbol(ch),
    };
}

fn isIdentInner(ch: u8) bool {
    return isIdentStart(ch) or isDigit(ch);
}

fn isIdentWord(word: []const u8) bool {
    if (!isIdentStart(word[0])) return false;

    for (word[1..]) |inner| {
        if (!isIdentInner(inner)) return false;
    }

    return true;
}

fn isIntWord(word: []const u8) bool {
    // skip negative sign
    const integral = if (word[0] == '-') word[1..] else word;

    if (!isDigit(integral[0])) return false;

    for (word) |ch| {
        if (!isNumeric(ch)) return false;
    }

    return true;
}

fn isFloatWord(word: []const u8) bool {
    const dot = std.mem.indexOf(u8, word, ".") orelse {
        return false;
    };
    const integral = word[0..dot];
    const fractional = word[dot + 1 ..];

    if (!isIntWord(integral)) return false;

    for (fractional) |ch| {
        if (!isNumeric(ch)) return false;
    }

    return true;
}

/// classifies a word token, returns null if it's unidentifiable
fn classify(word: []const u8) ?SExpr.Data {
    std.debug.assert(word.len > 0);
    if (isIdentWord(word)) {
        for (std.enums.values(SExpr.Syntax)) |syntax| {
            if (std.mem.eql(u8, word, @tagName(syntax))) {
                return .{ .syntax = syntax };
            }
        }

        return .{ .ident = mini.string(word) };
    } else if (isIntWord(word)) {
        return .{ .int = word };
    } else if (isFloatWord(word)) {
        return .{ .float = word };
    }

    return null;
}

/// returns null on syntax error
fn parseExpr(lexer: *Lexer, ast: *Ast, initial: Lexer.Token) Error!?SExpr {
    const expr: SExpr = switch (initial.kind) {
        .lparen => list: {
            var children = std.ArrayList(SExpr).init(ast.ally);
            defer children.deinit();

            while (true) {
                const tok = lexer.next() catch |e| {
                    const err_kind: Ast.SyntaxError.Kind = switch (e) {
                        Lexer.Error.InvalidCharacter => .invalid_character,
                    };

                    ast.syntaxError(err_kind, lexer.index - 1, 1);
                    return null;
                } orelse {
                    ast.syntaxError(
                        .unfinished_list,
                        initial.start,
                        lexer.index - initial.start,
                    );
                    return null;
                };
                if (tok.kind == .rparen) break;

                const child = try parseExpr(lexer, ast, tok) orelse {
                    return null;
                };
                try children.append(child);
            }

            const arena_ally = ast.arena.allocator();
            const arena_children = try arena_ally.dupe(SExpr, children.items);
            break :list SExpr{
                .span_start = initial.start,
                .span_len = lexer.index - initial.start,
                .data = .{ .list = arena_children },
            };
        },
        .rparen => {
            ast.syntaxError(.unmatched_rparen, initial.start, initial.len);
            return null;
        },
        .word => word: {
            const word = initial.span(lexer.text);
            const data = classify(word) orelse {
                ast.syntaxError(.invalid_word, initial.start, initial.len);
                return null;
            };

            break :word SExpr{
                .span_start = initial.start,
                .span_len = initial.len,
                .data = data,
            };
        },
    };

    return expr;
}

pub fn parse(ally: Allocator, name: []const u8, text: []const u8) Error!Ast {
    var lexer = Lexer.init(text);
    var ast = Ast.init(ally, name, text);

    while (true) {
        const tok = lexer.next() catch |e| {
            switch (e) {
                Lexer.Error.InvalidCharacter => {
                    ast.syntaxError(.invalid_character, lexer.index, 1);
                },
            }

            break;
        } orelse break;

        const expr = try parseExpr(&lexer, &ast, tok) orelse break;
        try ast.toplevel.append(ast.ally, expr);
    }

    return ast;
}
