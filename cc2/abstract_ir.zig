const std = @import("std");
const Allocator = std.mem.Allocator;
const sources = @import("sources.zig");
const Loc = sources.Loc;
const errors = @import("errors.zig");
const ErrorBuffer = errors.ErrorBuffer;
const Token = @import("Lexer.zig").Token;
const concrete = @import("concrete_ir.zig");
const Cst = concrete.Cst;
const Cid = concrete.Id;

fn CstIterator(comptime T: type) type {
    return struct {
        const Self = @This();

        loc: Loc,
        items: []const T,
        index: usize = 0,

        fn init(loc: Loc, items: []const T) Self {
            return .{ .loc = loc, .items = items };
        }

        fn peek(self: Self) ?T {
            if (self.index >= self.items.len) return null;
            return self.items[self.index];
        }

        fn advance(self: *Self) void {
            self.index = @min(self.index + 1, self.items.len);
        }

        fn next(self: *Self) ?T {
            const item = self.peek() orelse {
                return null;
            };
            self.advance();
            return item;
        }

        /// finds a reasonable location for the end of the consumed tokens
        fn lastLoc(self: TokenIterator) Loc {
            if (self.items.len == 0) {
                return self.loc.end();
            } else if (self.index < self.items.len) {
                return self.items[self.index - 1].endLoc();
            }

            return self.items[self.items.len - 1].endLoc();
        }

        /// finds a reasonable location for the next token pointed to by the iterator
        fn nextLoc(self: TokenIterator) Loc {
            if (self.items.len == 0) {
                return self.loc.start();
            } else if (self.index < self.items.len) {
                return self.items[self.index].startLoc();
            }

            return self.items[self.items.len - 1].endLoc();
        }

        /// if this token iterator isn't done, generates an unexpected expression
        fn ensureComplete(self: TokenIterator, eb: *ErrorBuffer) Allocator.Error!void {
            if (self.index < self.items.len) {
                try eb.add(self.items[self.index].startLoc(), .unexpected_expression);
            }
        }
    };
}

const TokenIterator = CstIterator(Token);
const NodeIterator = CstIterator(concrete.Id);

pub const Id = enum(u32) { _ };

pub const Expr = union(enum) {
    const Self = @This();
    pub const Tag = std.meta.Tag(Self);

    const Symbol = enum {
        comma,
        assignment,
        add,
        subtract,
    };

    const StorageClassSpec = enum {
        typedef,
        @"extern",
        static,
        _Thread_local,
        auto,
        register,
    };

    const BasicTypeSpec = enum {
        void,
        char,
        short,
        int,
        long,
        float,
        double,
        signed,
        unsigned,
        _Bool,
        _Complex,
        _Imaginary,
    };

    const TypeSpec = union(enum) {
        basic: BasicTypeSpec,
    };

    const Declarator = union(enum) {
        const Function = struct {
            lhs: Id,
            params: []const Id,
        };

        pointer: Id,
        function: Function,
    };

    const FunctionDefinition = struct {
        decl_specs: []const Id,
        declarator: Id,
        // knr_decls: ?Id,
        compound_stmt: []const Id,
    };

    const Declaration = struct {
        decl_specs: []const Id,
        init_declarators: Id,
    };

    const Binary = struct {
        symbol: Id,
        lhs: Id,
        rhs: Id,
    };

    translation_unit: []const Id,
    function_definition: FunctionDefinition,
    storage_class_spec: StorageClassSpec,
    type_spec: TypeSpec,
    declarator: Declarator,
    identifier: []const u8,
    statement: Id,
    return_stmt: ?Id,
    integer_constant: []const u8,
    string_constant: []const u8,
    parens: Id,
    symbol: Symbol,
    binary: Binary,
    empty_decl,
    declaration: Declaration,

    /// type of the field associated with this tag
    fn Data(comptime tag: Tag) type {
        return std.meta.FieldType(Self, tag);
    }
};

pub const Ast = struct {
    const Self = @This();

    const ExprEntry = struct {
        loc: Loc,
        expr: Expr,
    };

    arena: std.heap.ArenaAllocator,
    root: ?Id = null,
    exprs: std.MultiArrayList(ExprEntry) = .{},

    fn init(allocator: Allocator) Self {
        return .{
            .arena = std.heap.ArenaAllocator.init(allocator),
        };
    }

    pub fn deinit(self: *Self) void {
        self.exprs.deinit(self.arena.child_allocator);
        self.arena.deinit();
    }

    fn ally(self: *Self) Allocator {
        return self.arena.allocator();
    }

    fn add(self: *Self, loc: Loc, expr: Expr) Allocator.Error!Id {
        const id: Id = @enumFromInt(self.exprs.len);
        try self.exprs.append(self.arena.child_allocator, .{
            .loc = loc,
            .expr = expr,
        });
        return id;
    }

    pub fn get(self: Self, id: Id) Expr {
        return self.exprs.items(.expr)[@intFromEnum(id)];
    }

    pub fn getLoc(self: Self, id: Id) Loc {
        return self.exprs.items(.loc)[@intFromEnum(id)];
    }

    fn displayData(
        self: Self,
        comptime T: type,
        data: T,
        child_depth: usize,
        writer: anytype,
    ) @TypeOf(writer).Error!void {
        switch (@typeInfo(T)) {
            .void => {},
            .optional => {
                const wrapped = data orelse return;
                try self.displayData(@TypeOf(wrapped), wrapped, child_depth, writer);
            },
            .pointer => |ptr| {
                std.debug.assert(ptr.size == .slice);

                switch (ptr.child) {
                    Id => {
                        for (data) |child| {
                            try self.displayInner(child, child_depth, writer);
                        }
                    },
                    u8 => {
                        try writer.writeByteNTimes(' ', 2 * child_depth);
                        try writer.print("{s}\n", .{data});
                    },
                    else => @compileError(@typeName(ptr.child)),
                }
            },
            .@"enum" => {
                if (T == Id) {
                    try self.displayInner(data, child_depth, writer);
                } else {
                    try writer.writeByteNTimes(' ', 2 * child_depth);
                    try writer.print("{s}\n", .{@tagName(data)});
                }
            },
            .@"struct" => |st| {
                inline for (st.fields) |field| {
                    try writer.writeByteNTimes(' ', 2 * child_depth);
                    try writer.print("({s})\n", .{field.name});
                    try self.displayData(
                        field.type,
                        @field(data, field.name),
                        child_depth + 1,
                        writer,
                    );
                }
            },
            .@"union" => {
                switch (data) {
                    inline else => |child_data, child_tag| {
                        try writer.writeByteNTimes(' ', 2 * child_depth);
                        try writer.print("({s})\n", .{@tagName(child_tag)});
                        try self.displayData(
                            @TypeOf(child_data),
                            child_data,
                            child_depth + 1,
                            writer,
                        );
                    },
                }
            },
            else => @compileError(@typeName(T)),
        }
    }

    fn displayInner(
        self: Self,
        id: Id,
        depth: usize,
        writer: anytype,
    ) @TypeOf(writer).Error!void {
        try writer.writeByteNTimes(' ', 2 * depth);

        const node = self.get(id);
        const loc = self.getLoc(id);
        try writer.print("{s} {}\n", .{ @tagName(node), loc });

        switch (self.get(id)) {
            inline else => |data| try self.displayData(@TypeOf(data), data, depth + 1, writer),
        }
    }

    pub fn displayExpr(self: Self, id: Id, writer: anytype) @TypeOf(writer).Error!void {
        try self.displayInner(id, 0, writer);
    }

    pub fn display(self: Self, writer: anytype) @TypeOf(writer).Error!void {
        if (self.root) |root| {
            try self.displayInner(root, 0, writer);
        } else {
            try writer.print("<empty ast>", .{});
        }
    }
};

const Error = Allocator.Error;
const ParseFunction = fn (*ErrorBuffer, *Ast, *TokenIterator) Error!?Id;

fn ParseEnumResult(comptime E: type) type {
    return struct { loc: Loc, tag: E };
}

fn parseEnum(comptime E: type) fn (*TokenIterator) ?ParseEnumResult(E) {
    return struct {
        fn f(tokens: *TokenIterator) ?ParseEnumResult(E) {
            const pk = tokens.peek() orelse return null;
            const tag = std.meta.stringToEnum(E, @tagName(pk.tag)) orelse {
                return null;
            };
            tokens.advance();

            return .{ .loc = pk.loc(), .tag = tag };
        }
    }.f;
}

fn parseEnumToExpr(comptime E: type, comptime into: Expr.Tag) ParseFunction {
    return struct {
        fn f(_: *ErrorBuffer, ast: *Ast, tokens: *TokenIterator) Error!?Id {
            const pk = tokens.peek() orelse return null;
            const value = std.meta.stringToEnum(E, @tagName(pk.tag)) orelse {
                return null;
            };
            tokens.advance();

            return try ast.add(pk.loc(), @unionInit(Expr, @tagName(into), value));
        }
    }.f;
}

fn parseOneOf(comptime funcs: anytype) ParseFunction {
    return struct {
        fn f(eb: *ErrorBuffer, ast: *Ast, tokens: *TokenIterator) Error!?Id {
            return inline for (funcs) |func| {
                if (try func(eb, ast, tokens)) |success| {
                    return success;
                }
            } else null;
        }
    }.f;
}

fn SeriesSpec(comptime tag: Expr.Tag) type {
    comptime {
        const info = @typeInfo(Expr.Data(tag));
        if (info != .Struct) {
            @compileError("parseSeries expects to parse into a struct");
        }
        for (info.Struct.fields) |field| {
            if (field.type != Id) {
                @compileError(
                    "parseSeries expects fields of " ++ @typeName(Expr.Data(tag)) ++
                        " to be Ids",
                );
            }
        }
    }

    return struct {
        parser: ParseFunction,
        to_field: ?std.meta.FieldEnum(Expr.Data(tag)),
        on_fail: ?errors.Error.Kind,
    };
}

fn parseSeries(
    comptime tag: Expr.Tag,
    comptime specs: []const SeriesSpec(tag),
) ParseFunction {
    comptime {
        if (specs.len == 0) {
            @compileError("must have at least one parser");
        }
    }

    return struct {
        fn f(eb: *ErrorBuffer, ast: *Ast, tokens: *TokenIterator) Error!?Id {
            const first_loc = tokens.nextLoc();

            var result: Expr.Data(tag) = undefined;
            inline for (specs) |spec| {
                const data = try spec.parser(eb, ast, tokens) orelse {
                    if (spec.on_fail) |err| {
                        try eb.add(tokens.nextLoc(), err);
                    }
                    return null;
                };
                if (spec.to_field) |field| {
                    @field(result, @tagName(field)) = data;
                }
            }

            const loc = first_loc.span(tokens.lastLoc());
            const expr = @unionInit(Expr, @tagName(tag), result);
            return try ast.add(loc, expr);
        }
    }.f;
}

fn parseTag(tokens: *TokenIterator, tag: Token.Tag) ?Token {
    const tok = tokens.peek() orelse return null;
    if (tok.tag != tag) return null;
    tokens.advance();
    return tok;
}

fn parseSymbol(comptime tag: Token.Tag, comptime symbol: Expr.Symbol) ParseFunction {
    return struct {
        fn f(_: *ErrorBuffer, ast: *Ast, tokens: *TokenIterator) Error!?Id {
            const token = parseTag(tokens, tag) orelse return null;
            return try ast.add(token.loc(), .{ .symbol = symbol });
        }
    }.f;
}

const SymbolSpec = struct { Token.Tag, Expr.Symbol };

fn parseOneOfSymbols(comptime specs: []const SymbolSpec) ParseFunction {
    return struct {
        fn f(_: *ErrorBuffer, ast: *Ast, tokens: *TokenIterator) Error!?Id {
            const tok = tokens.peek() orelse return null;
            const symbol = for (specs) |spec| {
                const tag, const symbol = spec;
                if (tok.tag == tag) break symbol;
            } else {
                return null;
            };
            tokens.advance();
            return try ast.add(tok.loc(), .{ .symbol = symbol });
        }
    }.f;
}

fn parseBinaryRightAssociative(
    comptime symbols: []const SymbolSpec,
    comptime inner_parser: ParseFunction,
) ParseFunction {
    return struct {
        fn f(eb: *ErrorBuffer, ast: *Ast, tokens: *TokenIterator) Error!?Id {
            const lhs = try inner_parser(eb, ast, tokens) orelse return null;
            const symbol = try parseOneOfSymbols(symbols)(eb, ast, tokens) orelse {
                return lhs;
            };
            const rhs = try f(eb, ast, tokens) orelse {
                try eb.add(ast.getLoc(lhs).span(tokens.nextLoc()), .expected_expression);
                return null;
            };

            const loc = ast.getLoc(lhs).span(ast.getLoc(rhs));
            return try ast.add(loc, .{ .binary = .{
                .symbol = symbol,
                .lhs = lhs,
                .rhs = rhs,
            } });
        }
    }.f;
}

fn parseIdentifier(_: *ErrorBuffer, ast: *Ast, tokens: *TokenIterator) Error!?Id {
    const tok = parseTag(tokens, .ident) orelse return null;
    const str = try ast.ally().dupe(u8, tok.slice());
    return try ast.add(tok.loc(), .{ .identifier = str });
}

/// expect a semicolon and tokens to finish
fn expectEndOfStatement(eb: *ErrorBuffer, tokens: *TokenIterator) Error!void {
    if (parseTag(tokens, .semicolon) == null) {
        try eb.add(tokens.nextLoc(), .{ .expected_token = .semicolon });
        return;
    }

    try tokens.ensureComplete(eb);
}

const parseStorageClassSpec = parseEnumToExpr(Expr.StorageClassSpec, .storage_class_spec);

fn parseTypeSpec(_: *ErrorBuffer, ast: *Ast, tokens: *TokenIterator) Error!?Id {
    if (parseEnum(Expr.BasicTypeSpec)(tokens)) |basic| {
        return try ast.add(basic.loc, .{ .type_spec = .{ .basic = basic.tag } });
    }

    return null;
}

fn parseDeclSpec(eb: *ErrorBuffer, ast: *Ast, tokens: *TokenIterator) Error!?Id {
    return parseOneOf([_]ParseFunction{
        parseStorageClassSpec,
        parseTypeSpec,
    })(eb, ast, tokens);
}

fn parseDeclSpecs(eb: *ErrorBuffer, ast: *Ast, tokens: *TokenIterator) ![]const Id {
    var decl_specs = std.ArrayList(Id).init(ast.ally());
    defer decl_specs.deinit();

    while (try parseDeclSpec(eb, ast, tokens)) |decl_spec| {
        try decl_specs.append(decl_spec);
    }

    return decl_specs.toOwnedSlice();
}

fn parseDirectDeclarator(eb: *ErrorBuffer, ast: *Ast, tokens: *TokenIterator) !?Id {
    var direct_decl = initial: {
        if (try parseIdentifier(eb, ast, tokens)) |identifier| {
            break :initial identifier;
        } else if (parseTag(tokens, .lparen)) |lparen| {
            const inner = try parseDeclarator(eb, ast, tokens) orelse {
                try eb.add(lparen.endLoc(), .expected_declarator);
                return null;
            };
            if (parseTag(tokens, .rparen) == null) {
                try eb.add(ast.getLoc(inner).end(), .{ .expected_token = .rparen });
            }

            break :initial inner;
        }

        return null;
    };

    while (true) {
        if (parseTag(tokens, .lparen)) |lparen| {
            var params = std.ArrayList(Id).init(ast.ally());
            defer params.deinit();

            if (parseTag(tokens, .rparen)) |rparen| {
                // TODO merge locs
                _ = rparen;
                direct_decl = try ast.add(lparen.startLoc(), .{ .declarator = .{
                    .function = .{
                        .lhs = direct_decl,
                        .params = try params.toOwnedSlice(),
                    },
                } });
            }
        } else break;

        // TODO handle lbracket
    }

    return direct_decl;
}

fn parseDeclarator(eb: *ErrorBuffer, ast: *Ast, tokens: *TokenIterator) Error!?Id {
    const pointer_res = parseTag(tokens, .star);

    var declarator = try parseDirectDeclarator(eb, ast, tokens) orelse {
        if (pointer_res) |res| {
            try eb.add(res.loc(), .expected_declarator);
        }
        return null;
    };

    if (pointer_res) |res| {
        // TODO merge pointer and declarator locs
        declarator = try ast.add(res.loc(), .{ .declarator = .{
            .pointer = declarator,
        } });
    }

    return declarator;
}

fn parseInitDeclarator(eb: *ErrorBuffer, ast: *Ast, tokens: *TokenIterator) Error!?Id {
    const declarator = try parseDeclarator(eb, ast, tokens) orelse return null;
    if (parseTag(tokens, .equals)) |tok| {
        _ = tok;
        @panic("TODO declarator initializers");
    }

    return declarator;
}

fn parseDeclaration(eb: *ErrorBuffer, ast: *Ast, tokens: *TokenIterator) Error!?Id {
    const first = tokens.peek() orelse return null;
    if (first.tag == .semicolon) {
        tokens.advance();
        return try ast.add(first.loc(), .empty_decl);
    }

    const decl_specs = try parseDeclSpecs(eb, ast, tokens);
    if (decl_specs.len == 0) {
        try eb.add(tokens.nextLoc(), .expected_declaration);
        return null;
    }

    const init_declarators = try parseBinaryRightAssociative(
        &.{.{ .comma, .comma }},
        parseInitDeclarator,
    )(eb, ast, tokens) orelse {
        try eb.add(tokens.nextLoc(), .expected_declarator);
        return null;
    };

    const loc = first.loc().span(ast.getLoc(init_declarators));
    return try ast.add(loc, .{ .declaration = .{
        .decl_specs = decl_specs,
        .init_declarators = init_declarators,
    } });
}

fn parsePrimaryExpression(eb: *ErrorBuffer, ast: *Ast, tokens: *TokenIterator) Error!?Id {
    const tok = tokens.peek() orelse return null;
    return switch (tok.tag) {
        .ident => ident: {
            tokens.advance();
            const str = try ast.ally().dupe(u8, tok.slice());
            break :ident try ast.add(tok.loc(), .{ .identifier = str });
        },
        .int_lit => int: {
            tokens.advance();
            const str = try ast.ally().dupe(u8, tok.slice());
            break :int try ast.add(tok.loc(), .{ .integer_constant = str });
        },
        .string_lit => str: {
            tokens.advance();
            const str = try ast.ally().dupe(u8, tok.slice());
            break :str try ast.add(tok.loc(), .{ .string_constant = str });
        },
        .lparen => parens: {
            tokens.advance();
            const inner = try parseExpression(eb, ast, tokens) orelse {
                try eb.add(tokens.nextLoc(), .expected_expression);
                return null;
            };
            const rparen = parseTag(tokens, .rparen) orelse {
                try eb.add(tokens.nextLoc(), .{ .expected_token = .rparen });

                const combined_loc = tok.loc().span(tokens.nextLoc());
                break :parens try ast.add(combined_loc, .{ .parens = inner });
            };

            const combined_loc = tok.loc().span(rparen.loc());
            break :parens try ast.add(combined_loc, .{ .parens = inner });
        },
        else => null,
    };
}

const parseAddSubExpr = parseBinaryRightAssociative(
    &.{ .{ .plus, .add }, .{ .minus, .subtract } },
    parsePrimaryExpression,
);

const parseAssignmentExpr = parseBinaryRightAssociative(
    &.{.{ .equals, .assignment }},
    parseAddSubExpr,
);

const parseCommaExpr = parseBinaryRightAssociative(&.{.{ .comma, .comma }}, parseAssignmentExpr);

const parseExpression = parseCommaExpr;

fn parseStatement(eb: *ErrorBuffer, cst: Cst, ast: *Ast, cid: Cid) Error!?Id {
    const node = cst.get(cid);
    const loc = cst.getLoc(cid);

    switch (node) {
        .root => unreachable,
        .stmt => |stmt_tokens| {
            var tokens = TokenIterator.init(loc, stmt_tokens);
            const first = tokens.peek().?;
            switch (first.tag) {
                .@"return" => {
                    tokens.advance();
                    const value = try parseExpression(eb, ast, &tokens);
                    try expectEndOfStatement(eb, &tokens);
                    return try ast.add(loc, .{ .return_stmt = value });
                },
                else => {
                    const value = try parseOneOf(.{
                        parseExpression,
                        parseDeclaration,
                    })(eb, ast, &tokens);
                    try expectEndOfStatement(eb, &tokens);
                    return value;
                },
            }
        },
        .block => |block| {
            _ = block;
            @panic("TODO");
        },
    }
}

fn expectCompoundStatement(
    eb: *ErrorBuffer,
    cst: Cst,
    ast: *Ast,
    compound: concrete.Block,
) Error![]const Id {
    var statements = std.ArrayList(Id).init(ast.ally());
    defer statements.deinit();

    var nodes = NodeIterator.init(compound.loc.start(), compound.block);
    while (nodes.next()) |cid| {
        switch (cst.get(cid)) {
            .root => unreachable,
            .stmt => |stmt| stmt: {
                if (try parseStatement(eb, cst, ast, cid)) |res| {
                    try statements.append(res);
                    break :stmt;
                }

                var tokens = TokenIterator.init(cst.getLoc(cid), stmt);
                const res = try parseDeclaration(eb, ast, &tokens) orelse {
                    try eb.add(cst.getLoc(cid).start(), .expected_statement);
                    break :stmt;
                };
                try tokens.ensureComplete(eb);
                try statements.append(res);
            },
            .block => |block| block: {
                const res = try parseStatement(eb, cst, ast, cid) orelse {
                    try eb.add(block.loc.start(), .expected_statement);
                    break :block;
                };

                try statements.append(res);
            },
        }
    }

    return try statements.toOwnedSlice();
}

fn expectExternalDeclaration(eb: *ErrorBuffer, cst: Cst, ast: *Ast, cid: Cid) !?Id {
    const loc = cst.getLoc(cid);
    switch (cst.get(cid)) {
        .root => unreachable,
        .stmt => |stmt| {
            var tokens = TokenIterator.init(loc, stmt);
            const res = try parseDeclaration(eb, ast, &tokens) orelse {
                try eb.add(cst.getLoc(cid).start(), .expected_declaration);
                return null;
            };
            try tokens.ensureComplete(eb);
            return res;
        },
        .block => |block| {
            var tokens = TokenIterator.init(block.loc, block.head);
            const decl_specs = try parseDeclSpecs(eb, ast, &tokens);
            const declarator = try parseDeclarator(eb, ast, &tokens) orelse {
                const expected_loc = loc: {
                    if (tokens.peek()) |pk| {
                        break :loc pk.startLoc();
                    } else if (block.head.len > 0) {
                        break :loc block.head[block.head.len - 1].endLoc();
                    }
                    break :loc block.loc.start();
                };

                try eb.add(expected_loc, .expected_declarator);
                return null;
            };

            // TODO support k&r declaration-list here

            try tokens.ensureComplete(eb);

            const compound_stmt = try expectCompoundStatement(eb, cst, ast, block);

            return try ast.add(loc, .{ .function_definition = .{
                .decl_specs = decl_specs,
                .declarator = declarator,
                .compound_stmt = compound_stmt,
            } });
        },
    }
}

fn expectTranslationUnit(eb: *ErrorBuffer, cst: Cst, ast: *Ast) !Id {
    var ext_decls = std.ArrayList(Id).init(ast.ally());
    defer ext_decls.deinit();

    const root_cid = cst.root.?;
    for (cst.get(root_cid).root) |ext_decl_cid| {
        const ext_decl = try expectExternalDeclaration(eb, cst, ast, ext_decl_cid) orelse {
            continue;
        };
        try ext_decls.append(ext_decl);
    }

    return try ast.add(cst.getLoc(root_cid), .{
        .translation_unit = try ext_decls.toOwnedSlice(),
    });
}

/// parses a translation unit
pub fn parse(ally: Allocator, eb: *ErrorBuffer, cst: Cst) !Ast {
    var ast = Ast.init(ally);
    errdefer ast.deinit();

    ast.root = try expectTranslationUnit(eb, cst, &ast);

    return ast;
}
