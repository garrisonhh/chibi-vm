//! the preprocessor wraps the lexer

const std = @import("std");
const Allocator = std.mem.Allocator;
const errors = @import("errors.zig");
const ErrorBuffer = errors.ErrorBuffer;
const sources = @import("sources.zig");
const Source = sources.Source;
const Loc = sources.Loc;
const Lexer = @import("Lexer.zig");
const Token = Lexer.Token;

pub const Error = Allocator.Error;

const Context = struct {
    ally: Allocator,
    output: std.ArrayListUnmanaged(Token) = .{},

    fn init(ally: Allocator) Context {
        return .{ .ally = ally };
    }

    fn deinit(ctx: *Context) void {
        ctx.output.deinit(ctx.ally);
    }

    fn append(ctx: *Context, token: Token) Allocator.Error!void {
        try ctx.output.append(ctx.ally, token);
    }

    fn appendSlice(ctx: *Context, tokens: []const Token) Allocator.Error!void {
        try ctx.output.appendSlice(ctx.ally, tokens);
    }

    fn toOwnedSlice(ctx: *Context) Allocator.Error![]const Token {
        return try ctx.output.toOwnedSlice(ctx.ally);
    }
};

const TokenSliceIterator = struct {
    const Self = @This();

    slice: []const Token,
    index: usize = 0,

    fn peek(self: Self) ?Token {
        return if (self.index < self.slice.len) self.slice[self.index] else null;
    }

    fn advance(self: *Self) void {
        std.debug.assert(self.index < self.slice.len);
        self.index += 1;
    }

    fn next(self: *Self) ?Token {
        const pk = self.peek();
        if (pk != null) self.advance();
        return pk;
    }

    fn prev(self: Self) Token {
        std.debug.assert(self.index > 0);
        return self.slice[self.index - 1];
    }
};

const Directive = union(enum) {
    include: Token,
};

fn parseDirective(eb: *ErrorBuffer, line: []const Token) Allocator.Error!?Directive {
    var iter = TokenSliceIterator{ .slice = line };

    const hash = iter.next() orelse unreachable;
    std.debug.assert(hash.tag == .hash);

    const dir_tok = iter.next() orelse return null;
    const directive: Directive = switch (dir_tok.tag) {
        .include => include: {
            const path_tok = iter.next() orelse {
                try eb.add(dir_tok.end(), .expected_include_path);
                return null;
            };

            break :include switch (path_tok.tag) {
                .string_lit, .include_lit => .{ .include = path_tok },
                else => {
                    try eb.add(path_tok.loc, .expected_include_path);
                    return null;
                },
            };
        },
        .@"error" => {
            try eb.add(dir_tok.loc, .error_directive);
            return null;
        },
        else => {
            try eb.add(dir_tok.loc, .unsupported_preprocessor_directive);
            return null;
        },
    };

    if (iter.next()) |tok| {
        try eb.add(tok.loc, .unexpected_expression);
        return null;
    }

    return directive;
}

/// attempt to load an included file
fn loadInclude(ctx: *Context, eb: *ErrorBuffer, tok: Token) Allocator.Error!?Source {
    const inc_path_slice = tok.slice();
    const inc_filename = inc_path_slice[1 .. inc_path_slice.len - 1];

    switch (tok.tag) {
        .string_lit => {
            const src_path = tok.loc.source.get().filename;
            const wd = std.fs.path.dirname(src_path) orelse "";

            const inc_path = try std.fs.path.join(ctx.ally, &.{ wd, inc_filename });
            defer ctx.ally.free(inc_path);

            // TODO search system include dirs

            const src = sources.addPath(inc_path) catch {
                try eb.add(tok.loc, .included_file_not_found);
                return null;
            };
            if (src == tok.loc.source) {
                try eb.add(tok.loc, .included_self);
                return null;
            }

            return src;
        },
        .include_lit => {
            @panic("TODO");
        },
        else => unreachable,
    }
}

fn execDirective(ctx: *Context, eb: *ErrorBuffer, directive: Directive) Allocator.Error!void {
    switch (directive) {
        .include => |path_tok| {
            const inc_src = try loadInclude(ctx, eb, path_tok) orelse return;
            try preprocessInner(ctx, eb, inc_src);
        },
    }
}

fn preprocessLine(ctx: *Context, eb: *ErrorBuffer, line: []const Token) Allocator.Error!void {
    if (line.len == 0) {
        return;
    } else if (line[0].tag == .hash) {
        const directive = try parseDirective(eb, line) orelse return;
        try execDirective(ctx, eb, directive);
        return;
    }

    // TODO macro expansion

    try ctx.appendSlice(line);
}

/// create compiler error from lexer error
fn diagnoseLexer(eb: *ErrorBuffer, lexer: Lexer, e: Lexer.Error) Allocator.Error!void {
    const kind: errors.Error.Kind = switch (e) {
        Lexer.Error.InvalidInput => .invalid_character,
        Lexer.Error.UnfinishedString => .unfinished_string,
        Lexer.Error.UnfinishedInclude => .unfinished_include,
    };
    try eb.add(lexer.loc, kind);
}

fn preprocessInner(ctx: *Context, eb: *ErrorBuffer, src: Source) Allocator.Error!void {
    var line_index: usize = 0;
    var line_buf = std.ArrayListUnmanaged(Token){};
    defer line_buf.deinit(ctx.ally);

    var lexer = Lexer.init(src);
    while (lexer.next() catch |e| {
        try diagnoseLexer(eb, lexer, e);
        return;
    }) |token| {
        if (token.loc.line_index != line_index) {
            line_index = token.loc.line_index;
            if (line_buf.items.len > 0) {
                try preprocessLine(ctx, eb, line_buf.items);
                line_buf.resize(ctx.ally, 0) catch unreachable;
            }
        }

        try line_buf.append(ctx.ally, token);
    }

    if (line_buf.items.len > 0) {
        try preprocessLine(ctx, eb, line_buf.items);
    }
}

/// lexes and preprocesses a source. returns null when errors are created and
/// the caller needs to halt and wave a finger at the programmer.
pub fn preprocess(ally: Allocator, eb: *ErrorBuffer, src: Source) Allocator.Error!?[]const Token {
    var ctx = Context.init(ally);
    defer ctx.deinit();

    try preprocessInner(&ctx, eb, src);

    if (eb.hasErrors()) return null;
    return try ctx.toOwnedSlice();
}
