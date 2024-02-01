const std = @import("std");
const Allocator = std.mem.Allocator;
const Lexer = @import("Lexer.zig");

pub const Expr = struct {
    const Self = @This();
    pub const Kind = std.meta.Tag(Data);

    pub const Data = union(enum) {
        ident: []const u8,
        int: []const u8,
        float: []const u8,
        list: []const Expr,
    };

    span_start: usize,
    span_len: usize,
    data: Data,

    pub fn span(self: Self, text: []const u8) []const u8 {
        return text[self.span_start .. self.span_start + self.span_len];
    }

    pub fn format(
        self: Self,
        comptime _: []const u8,
        _: std.fmt.FormatOptions,
        writer: anytype,
    ) @TypeOf(writer).Error!void {
        switch (self.data) {
            .ident, .int, .float => |str| {
                try writer.print("<{s}>{s}", .{@tagName(self.data), str});
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
pub const Ast = struct{
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

    /// name of source
    name: []const u8,
    /// content of source
    text: []const u8,
    /// top level program
    toplevel: std.ArrayListUnmanaged(Expr) = .{},

    fn init(ally: Allocator, name: []const u8, text: []const u8) Self {
        return Self{
            .ally = ally,
            .arena = std.heap.ArenaAllocator.init(ally),
            .name = name,
            .text = text,
        };
    }

    pub fn deinit(self: *Self) void {
        self.toplevel.deinit(self.ally);
        self.arena.deinit();
    }

    fn syntaxError(
        self: *Self,
        kind: SyntaxError.Kind,
        start: usize,
        len: usize,
    ) void {
        self.err = SyntaxError{
            .kind = kind,
            .start = start,
            .len = len,
        };
    }
};

// recursive descent ===========================================================

pub const Error = Allocator.Error;

/// classifies a word token, returns null if it's unidentifiable
fn classify(word: []const u8) ?Expr.Data {
    // TODO
    return .{ .ident = word };
}

/// returns null on syntax error
fn parseExpr(lexer: *Lexer, ast: *Ast, initial: Lexer.Token) Error!?Expr {
    const expr: Expr = switch (initial.kind) {
        .lparen => list: {
            var children = std.ArrayList(Expr).init(ast.ally);
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
            const arena_children = try arena_ally.dupe(Expr, children.items);
            break :list Expr{
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

            break :word Expr{
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
                    ast.syntaxError(.invalid_character, lexer.index - 1, 1);
                },
            }

            break;
        } orelse break;

        const expr = try parseExpr(&lexer, &ast, tok) orelse break;
        try ast.toplevel.append(ast.ally, expr);
    }

    return ast;
}