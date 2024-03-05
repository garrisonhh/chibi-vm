//! recursive descent parser for c
//!
//! basic info to understand this code:
//! - the parser is intended to be run on individual toplevel statements, split
//!   by an iterator. it is designed this way in order to:
//!     1. take advantage of C's restrictions to elegantly handle the type
//!        system
//!     2. allow a much larger amount of the translation unit to be analyzed
//!        before failure, producing more intelligent error output
//! - if possible, errors are added to a tree as ast nodes
//!     - recoverable errors can also be added to the error buffer if this is
//!       inconvenient
//! - on unrecoverable error, parse functions should reset to their initial
//!   token index and return null

const std = @import("std");
const Allocator = std.mem.Allocator;
const sources = @import("sources.zig");
const Loc = sources.Loc;
const errors = @import("errors.zig");
const ErrorBuffer = errors.ErrorBuffer;
const Token = @import("Lexer.zig").Token;

pub const Id = enum(u32) { _ };

pub const Expr = union(enum) {
    const Self = @This();

    pub const Signedness = enum {
        signed,
        unsigned,
    };

    pub const Type = enum {
        char,
        int,
        long,
        long_long,
    };

    pub const DeclSpec = struct {
        is_const: bool,
        signedness: ?Signedness,
        /// underlying type
        type: Type,
    };

    pub const Pointer = struct {
        is_const: bool,
        child: ?Id,
    };

    pub const Declaration = struct {
        declspec: Id,
        declarator: Id,
    };

    err: errors.Error,
    ident: []const u8,
    comma: [2]Id,
    declspec: DeclSpec,
    pointer: Pointer,
    declaration: Declaration,

    /// if this expr is an error, get the error
    pub fn check(self: Self) ?errors.Error {
        return if (self == .err) self.err else null;
    }
};

/// represents a syntax tree for a single top level statement
pub const Tree = struct {
    const Self = @This();

    const Entry = struct {
        loc: Loc,
        expr: Expr,
    };

    arena: std.heap.ArenaAllocator,
    exprs: std.MultiArrayList(Entry) = .{},
    root: ?Id = null,

    fn init(ally: Allocator) Self {
        return Self{
            .arena = std.heap.ArenaAllocator.init(ally),
        };
    }

    pub fn deinit(self: *Self) void {
        self.arena.deinit();
        self.exprs.deinit(self.arena.child_allocator);
    }

    fn allocator(self: *Self) Allocator {
        return self.arena.allocator();
    }

    fn add(self: *Self, loc: Loc, expr: Expr) Allocator.Error!Id {
        const id: Id = @enumFromInt(@as(u32, @intCast(self.exprs.len)));
        try self.exprs.append(self.arena.child_allocator, Entry{
            .loc = loc,
            .expr = expr,
        });

        return id;
    }

    /// sugar for constructing an error expr
    fn err(self: *Self, loc: Loc, kind: errors.Error.Kind) Allocator.Error!Id {
        return try self.add(loc, .{ .err = errors.Error.init(loc, kind) });
    }

    fn get(self: Self, id: Id) Expr {
        return self.exprs.items(.expr)[@intFromEnum(id)];
    }

    fn getLoc(self: Self, id: Id) Loc {
        return self.exprs.items(.loc)[@intFromEnum(id)];
    }

    fn Display(comptime Writer: type) type {
        return struct {
            tree: Tree,
            writer: Writer,
            level: u32 = 0,

            fn enter(d: *@This()) void {
                d.level += 1;
            }

            fn exit(d: *@This()) void {
                d.level -= 1;
            }

            fn print(d: *@This(), comptime fmt: []const u8, args: anytype) Writer.Error!void {
                var buf: [512]u8 = undefined;
                const slice = std.fmt.bufPrint(&buf, fmt, args) catch "<buffer overflow>";

                var last: usize = 0;
                for (slice, 0..) |ch, i| {
                    if (ch == '\n') {
                        try d.writer.writeByteNTimes(' ', d.level * 2);
                        try d.writer.writeAll(slice[last..i]);
                        try d.writer.writeByte('\n');

                        last = i + 1;
                    }
                }

                if (last != slice.len) {
                    try d.writer.writeByteNTimes(' ', d.level * 2);
                    try d.writer.writeAll(slice[last..]);
                }
            }

            fn display(d: *@This(), id: Id) Writer.Error!void {
                const expr = d.tree.get(id);
                try d.print("{s}:\n", .{@tagName(expr)});

                d.enter();
                defer d.exit();

                switch (expr) {
                    .err => |e| try d.print("{s}\n", .{@tagName(e.kind)}),
                    .ident => |str| try d.print("{s}\n", .{str}),
                    .pointer => |p| {
                        if (p.is_const) try d.print("const\n", .{});
                        if (p.child) |child| {
                            try d.display(child);
                        }
                    },
                    .declspec => |ds| {
                        if (ds.is_const) try d.print("const ", .{});
                        if (ds.signedness) |sign| {
                            try d.print("{s} ", .{@tagName(sign)});
                        }

                        try d.print("{s}\n", .{@tagName(ds.type)});
                    },
                    .declaration => |decl| {
                        try d.display(decl.declspec);
                        try d.display(decl.declarator);
                    },
                    .comma => |bin| {
                        try d.display(bin[0]);
                        try d.display(bin[1]);
                    },
                }
            }
        };
    }

    /// dump a tree to a writer
    pub fn display(self: Self, id: Id, writer: anytype) @TypeOf(writer).Error!void {
        var d = Display(@TypeOf(writer)){
            .tree = self,
            .writer = writer,
        };
        try d.display(id);
    }
};

// parsing =====================================================================

/// context for parsing a toplevel statement
const Parser = struct {
    arena: std.heap.ArenaAllocator,
    eb: *ErrorBuffer,
    tokens: []const Token,
    index: usize = 0,

    fn init(ally: Allocator, eb: *ErrorBuffer, tokens: []const Token) Parser {
        std.debug.assert(tokens.len > 0);
        return Parser{
            .arena = std.heap.ArenaAllocator.init(ally),
            .eb = eb,
            .tokens = tokens,
        };
    }

    fn deinit(p: *Parser) void {
        p.arena.deinit();
    }

    fn allocator(p: *Parser) Allocator {
        return p.arena.allocator();
    }

    /// returns a location located approximately before the current token position
    fn lastLoc(p: Parser) Loc {
        if (p.index == 0) {
            return p.tokens[0].loc;
        }
        return p.tokens[p.index - 1].end();
    }

    /// returns a location located approximately after the current token position
    fn nextLoc(p: Parser) Loc {
        if (p.index == p.tokens.len) {
            return p.tokens[p.index - 1].end();
        }
        return p.tokens[p.index].loc;
    }

    fn peek(p: Parser) ?Token {
        return if (p.index < p.tokens.len) p.tokens[p.index] else null;
    }

    fn advance(p: *Parser) void {
        std.debug.assert(p.index < p.tokens.len);
        p.index += 1;
    }

    fn next(p: *Parser) ?Token {
        const pk = p.peek();
        if (pk != null) p.advance();
        return pk;
    }

    fn done(p: Parser) bool {
        return p.index == p.tokens.len;
    }
};

pub const Error = Allocator.Error;

fn tagTo(comptime Into: type, tag: Token.Tag) Into {
    return std.meta.stringToEnum(Into, @tagName(tag)).?;
}

fn parseDeclSpec(p: *Parser, tree: *Tree) Error!?Id {
    var is_const: bool = false;
    var signedness: ?Expr.Signedness = null;
    var basic: ?Expr.Type = null;

    const loc = p.nextLoc();

    while (p.peek()) |token| {
        switch (token.tag) {
            .@"const" => {
                if (is_const) {
                    try p.eb.add(token.loc, .extra_type_qualifier);
                }
                is_const = true;
            },
            .unsigned, .signed => {
                const token_sign = tagTo(Expr.Signedness, token.tag);
                if (signedness != null) {
                    try p.eb.add(token.loc, .extra_signedness);
                } else {
                    signedness = token_sign;
                }
            },
            .char, .int, .long => {
                const token_ty = tagTo(Expr.Type, token.tag);

                if (basic == null) {
                    basic = token_ty;
                } else if (basic.? == .long and token.tag == .long) {
                    basic = .long_long;
                } else {
                    try p.eb.add(token.loc, .extra_basic_type);
                }
            },
            else => break,
        }

        p.advance();
    }

    const final_ty = basic orelse {
        return try tree.err(p.lastLoc(), .expected_type_specifier);
    };

    return try tree.add(loc, .{ .declspec = .{
        .is_const = is_const,
        .signedness = signedness,
        .type = final_ty,
    } });
}

fn parseDeclarator(p: *Parser, tree: *Tree) Error!?Id {
    const tok = p.next() orelse {
        return try tree.err(p.lastLoc(), .expected_declarator);
    };
    return switch (tok.tag) {
        .ident => try tree.add(tok.loc, .{ .ident = tok.slice() }),
        .star => ptr: {
            var is_const = false;
            while (p.peek()) |pk| {
                switch (pk.tag) {
                    .@"const" => {
                        if (is_const) {
                            try p.eb.add(pk.loc, .extra_type_qualifier);
                        }
                        is_const = true;
                    },
                    else => break,
                }
            }

            const child = try parseDeclarator(p, tree) orelse return null;
            break :ptr try tree.add(tok.loc, .{ .pointer = .{
                .is_const = is_const,
                .child = child,
            } });
        },
        .lparen => parens: {
            const child = try parseDeclarator(p, tree) orelse return null;
            const next = p.next() orelse {
                try p.eb.add(p.lastLoc(), .expected_rparen);
                return null;
            };
            if (next.tag != .rparen) {
                try p.eb.add(next.loc, .expected_rparen);
                return null;
            }

            break :parens child;
        },
        else => try tree.err(tok.loc, .expected_declarator),
    };
}

fn parseDeclaration(p: *Parser, tree: *Tree) Error!?Id {
    const declspec = try parseDeclSpec(p, tree) orelse return null;
    const declarator = try parseDeclarator(p, tree) orelse return null;

    var declarators = declarator;
    while (true) {
        const pk = p.peek() orelse break;
        if (pk.tag != .comma) break;
        p.advance();

        const rhs = try parseDeclarator(p, tree) orelse return null;
        declarators = try tree.add(pk.loc, .{ .comma = .{ declarators, rhs } });
    }

    return try tree.add(tree.getLoc(declspec), .{ .declaration = .{
        .declspec = declspec,
        .declarator = declarators,
    } });
}

/// parse a toplevel declaration
pub fn parse(
    ally: Allocator,
    eb: *ErrorBuffer,
    tokens: []const Token,
) Error!Tree {
    std.debug.assert(tokens.len > 0);

    var tree = Tree.init(ally);
    errdefer tree.deinit();

    var p = Parser.init(ally, eb, tokens);
    tree.root = try parseDeclaration(&p, &tree);

    // TODO collect errors from tree

    return tree;
}

// token processing ============================================================

pub const SplitTopLevelIterator = struct {
    tokens: []const Token,
    index: usize = 0,

    pub fn next(iter: *@This()) ?[]const Token {
        if (iter.index == iter.tokens.len) return null;

        const start_index = iter.index;
        var level: usize = 0;
        while (iter.index < iter.tokens.len) {
            const token = iter.tokens[iter.index];
            iter.index += 1;

            switch (token.tag) {
                .lcurly => level += 1,
                .rcurly => {
                    if (level == 0) break;
                    level -= 1;
                    if (level == 0) break;
                },
                .semicolon => {
                    if (level == 0) break;
                },
                else => {},
            }
        }

        return iter.tokens[start_index..iter.index];
    }
};

/// iterate over toplevel decls
pub fn splitToplevel(tokens: []const Token) SplitTopLevelIterator {
    return .{ .tokens = tokens };
}
