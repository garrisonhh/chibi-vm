//! recursive descent parser for c, heavily based off of katahiromz's grammar
//!
//! basic info to understand this code:
//! - the parser is intended to be run on individual toplevel statements, split
//!   by an iterator
//!   - after parsing, the resulting tree should be inspected for errors
//!   - it is designed this way in order to:
//!     1. take advantage of C's restrictions to elegantly handle the type
//!        system
//!     2. allow a much larger amount of the translation unit to be analyzed
//!        before failure, producing more intelligent error output
//! - this parser is written with error handling held heavily in mind
//!   - all errors should be created as exprs, allowing parsing to gracefully
//!     ignore or handle errors as necessary

const std = @import("std");
const Allocator = std.mem.Allocator;
const sources = @import("sources.zig");
const Loc = sources.Loc;
const errors = @import("errors.zig");
const ErrorBuffer = errors.ErrorBuffer;
const Token = @import("Lexer.zig").Token;

pub const Id = enum(u31) { _ };

pub const Expr = union(enum) {
    const Self = @This();
    pub const Kind = std.meta.Tag(Self);

    err: errors.Error.Kind,
    ident: []const u8,

    @"const",
    void,
    unsigned,
    signed,
    char,
    short,
    int,
    long,
    float,
    double,
    star,

    pointers,
    pointer,
    block,
    params,
    parens,
    declspecs,
    declarator,
    function_definition,

    pub fn format(
        self: Self,
        comptime _: []const u8,
        _: std.fmt.FormatOptions,
        writer: anytype,
    ) @TypeOf(writer).Error!void {
        try writer.print("{s} ", .{@tagName(self)});
        switch (self) {
            .err => |tag| {
                try writer.print("{s}", .{@tagName(tag)});
            },
            .ident => |ident| {
                try writer.print("{s}", .{ident});
            },
            inline else => |data, tag| {
                if (comptime @TypeOf(data) != void) {
                    @compileError("plz handle " ++ @tagName(tag));
                }
            },
        }
    }
};

/// represents a syntax tree for a single top level statement
pub const Tree = struct {
    const Self = @This();

    const Entry = struct {
        loc: Loc,
        expr: Expr,
        first_child: ?Id = null,
        last_child: ?Id = null,
        next: ?Id = null,
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
    fn addErr(self: *Self, loc: Loc, kind: errors.Error.Kind) Allocator.Error!Id {
        return try self.add(loc, .{ .err = kind });
    }

    fn isErr(self: Self, id: Id) bool {
        return self.exprs.items(.expr)[@intFromEnum(id)] == .err;
    }

    fn get(self: Self, id: Id) Expr {
        return self.exprs.items(.expr)[@intFromEnum(id)];
    }

    fn getLoc(self: Self, id: Id) Loc {
        return self.exprs.items(.loc)[@intFromEnum(id)];
    }

    // append a node to a detached (parentless) node
    fn appendDetached(self: Self, node: Id, other: Id) void {
        std.debug.assert(self.exprs.items(.next)[@intFromEnum(node)] == null);
        self.exprs.items(.next)[@intFromEnum(node)] = other;
    }

    fn appendChild(self: Self, node: Id, child: Id) void {
        const node_index = @intFromEnum(node);
        if (self.exprs.items(.last_child)[node_index]) |prev_last_child| {
            self.exprs.items(.next)[@intFromEnum(prev_last_child)] = child;
        } else {
            self.exprs.items(.first_child)[node_index] = child;
        }

        var last_child = child;
        while (self.exprs.items(.next)[@intFromEnum(last_child)]) |next_child| {
            last_child = next_child;
        }

        self.exprs.items(.last_child)[node_index] = last_child;
    }

    const ChildIterator = struct {
        child_nexts: []const ?Id,
        trav: ?Id,

        fn next(iter: *@This()) ?Id {
            const node = iter.trav orelse return null;
            iter.trav = iter.child_nexts[@intFromEnum(node)];
            return node;
        }
    };

    fn children(self: Self, node: Id) ChildIterator {
        return .{
            .child_nexts = self.exprs.items(.next),
            .trav = self.exprs.items(.first_child)[@intFromEnum(node)],
        };
    }

    fn collectErrorsRecursive(
        self: Self,
        eb: *ErrorBuffer,
        id: Id,
    ) Allocator.Error!void {
        if (self.isErr(id)) {
            try eb.add(self.getLoc(id), self.get(id).err);
        }

        var child_iter = self.children(id);
        while (child_iter.next()) |child| {
            try self.collectErrorsRecursive(eb, child);
        }
    }

    pub fn collectErrors(self: Self, eb: *ErrorBuffer) Allocator.Error!void {
        if (self.root) |root| {
            try self.collectErrorsRecursive(eb, root);
        }
    }

    fn displayExpr(
        self: Self,
        id: Id,
        level: usize,
        writer: anytype,
    ) @TypeOf(writer).Error!void {
        try writer.writeByteNTimes(' ', 2 * level);
        try writer.print("{}\n", .{self.get(id)});

        var child_iter = self.children(id);
        while (child_iter.next()) |child| {
            try self.displayExpr(child, level + 1, writer);
        }
    }

    /// print out tree (mostly for debugging)
    pub fn display(self: Self, writer: anytype) @TypeOf(writer).Error!void {
        if (self.root) |root| {
            try self.displayExpr(root, 0, writer);
        } else {
            try writer.print("(empty tree)", .{});
        }

        try writer.print("\n", .{});
    }
};

// parsing =====================================================================

/// context for parsing a toplevel statement
const Parser = struct {
    tokens: []const Token,
    index: usize = 0,

    fn init(tokens: []const Token) Parser {
        return .{ .tokens = tokens };
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

fn parseToken(p: *Parser, tag: Token.Tag) ?Token {
    const tok = p.peek() orelse return null;
    if (tok.tag != tag) return null;
    p.advance();
    return tok;
}

pub const Error = Allocator.Error;
const ParseFn = fn (p: *Parser, tree: *Tree) Error!Id;

fn parseIdent(p: *Parser, tree: *Tree) Error!Id {
    const tok = parseToken(p, .ident) orelse {
        return try tree.addErr(p.nextLoc(), .expected_identifier);
    };
    const ident = try tree.allocator().dupe(u8, tok.slice());
    return try tree.add(tok.loc, .{ .ident = ident });
}

/// parse a token tag into an expr with no attached data
fn parseSymbol(
    comptime tag: Token.Tag,
    comptime into: Expr.Kind,
    comptime fail_err: errors.Error.Kind,
) ParseFn {
    return struct {
        fn f(p: *Parser, tree: *Tree) Error!Id {
            const tok = parseToken(p, tag) orelse {
                return try tree.addErr(p.nextLoc(), fail_err);
            };
            return try tree.add(tok.loc, @unionInit(Expr, @tagName(into), {}));
        }
    }.f;
}

/// attempt to parse each func in order until one works
fn parseOneOf(
    comptime fail_err: errors.Error.Kind,
    comptime funcs: anytype,
) ParseFn {
    return struct {
        fn f(p: *Parser, tree: *Tree) Error!Id {
            inline for (funcs) |func| {
                const res = try func(p, tree);
                if (!tree.isErr(res)) {
                    return res;
                }
            }

            return try tree.addErr(p.nextLoc(), fail_err);
        }
    }.f;
}

fn parseMany(comptime container: Expr.Kind, comptime func: ParseFn) ParseFn {
    return struct {
        fn f(p: *Parser, tree: *Tree) Error!Id {
            const node = try tree.add(
                p.nextLoc(),
                @unionInit(Expr, @tagName(container), {}),
            );
            while (true) {
                const res = try func(p, tree);
                if (tree.isErr(res)) break;
                tree.appendChild(node, res);
            }

            return node;
        }
    }.f;
}

/// parse a series of parsers into one expr
fn parseContainer(
    comptime container: Expr.Kind,
    comptime fail_err: errors.Error.Kind,
    comptime funcs: anytype,
) ParseFn {
    return struct {
        fn f(p: *Parser, tree: *Tree) Error!Id {
            const start = p.index;
            const node = try tree.add(
                p.nextLoc(),
                @unionInit(Expr, @tagName(container), {}),
            );
            inline for (funcs) |func| {
                const res = try func(p, tree);
                tree.appendChild(node, res);

                if (tree.isErr(res)) {
                    if (p.index == start) {
                        // recoverable error
                        return try tree.addErr(p.nextLoc(), fail_err);
                    } else {
                        // unrecoverable error
                        break;
                    }
                }
            }

            return node;
        }
    }.f;
}

fn parseOptional(comptime container: Expr.Kind, comptime func: ParseFn) ParseFn {
    return struct {
        fn f(p: *Parser, tree: *Tree) Error!Id {
            const node = try tree.add(
                p.nextLoc(),
                @unionInit(Expr, @tagName(container), {}),
            );
            const res = try func(p, tree);
            if (!tree.isErr(res)) {
                tree.appendChild(node, res);
            }
            return node;
        }
    }.f;
}

const parseDeclSpecs = parseMany(.declspecs, parseOneOf(.unexpected, .{
    parseSymbol(.@"const", .@"const", .unexpected),
    parseSymbol(.int, .int, .unexpected),
}));

fn parsePointers(p: *Parser, tree: *Tree) Error!Id {
    const pointers = try tree.add(p.nextLoc(), .pointers);
    var trav = pointers;
    while (parseToken(p, .star)) |star| {
        const node = try tree.add(star.loc, .pointer);
        tree.appendChild(trav, node);
        trav = node;
    }

    return pointers;
}

fn parseDirectDeclarator(p: *Parser, tree: *Tree) Error!Id {
    const start = p.index;
    _ = start;

    // parse initial
    var dd: Id = initial: {
        const ident_res = try parseIdent(p, tree);
        if (!tree.isErr(ident_res)) {
            break :initial ident_res;
        }

        if (parseToken(p, .lparen)) |tok| {
            return try tree.addErr(tok.loc, .unimplemented_expression);
        }

        return try tree.addErr(p.nextLoc(), .expected_direct_declarator);
    };

    while (true) {
        if (parseToken(p, .lparen)) |lparen| {
            const params = try tree.add(tree.getLoc(dd), .params);
            const parens = try tree.add(lparen.loc, .parens);
            tree.appendChild(params, dd);
            tree.appendChild(params, parens);
            dd = params;

            if (parseToken(p, .rparen)) |_| {
                break;
            }

            return try tree.addErr(p.nextLoc(), .unimplemented_expression);
        } // TODO else if parse lbracket
        else break;
    }

    return dd;
}

fn parseCompoundStatement(p: *Parser, tree: *Tree) Error!Id {
    const start = p.index;

    const lcurly = parseToken(p, .lcurly) orelse {
        return try tree.addErr(p.nextLoc(), .expected_compound_statement);
    };
    const container = try tree.add(lcurly.loc, .block);

    // TODO parse declarations & statements

    _ = parseToken(p, .rcurly) orelse {
        p.index = start;
        return try tree.addErr(lcurly.loc, .unfinished_block);
    };

    return container;
}

const parseDeclarator = parseContainer(.declarator, .unexpected, .{
    parsePointers,
    parseDirectDeclarator,
});

const parseFunctionDefinition = parseContainer(.function_definition, .unexpected, .{
    parseDeclSpecs,
    parseDeclarator,
    parseCompoundStatement,
});

fn parseDeclaration(p: *Parser, tree: *Tree) Error!Id {
    return try tree.addErr(p.nextLoc(), .unimplemented_expression);
}

const parseExternalDeclaration = parseOneOf(.expected_toplevel_statement, .{
    parseFunctionDefinition,
    parseDeclaration,
});

/// parse a toplevel declaration
pub fn parse(
    ally: Allocator,
    tokens: []const Token,
) Error!Tree {
    std.debug.assert(tokens.len > 0);

    var tree = Tree.init(ally);
    errdefer tree.deinit();

    var p = Parser.init(tokens);
    tree.root = try parseExternalDeclaration(&p, &tree);

    if (!p.done()) {
        const extra_tokens_err = try tree.addErr(p.nextLoc(), .unexpected_expression);
        tree.appendChild(tree.root.?, extra_tokens_err);
    }

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
