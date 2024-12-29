//! concrete syntax parsing. this step allows for the abstract syntax output to
//! be super friendly by splitting up syntax into bite-sized chunks (blocks and
//! statements)

const std = @import("std");
const Allocator = std.mem.Allocator;
const sources = @import("sources.zig");
const Loc = sources.Loc;
const errors = @import("errors.zig");
const ErrorBuffer = errors.ErrorBuffer;
const Token = @import("Lexer.zig").Token;

const Id = enum(u32) { _ };

const Node = union(enum) {
    const Block = struct {
        head: []const Token,
        block: []const Id,
    };

    root: []const Id,
    stmt: []const Token,
    block: Block,
};

/// concrete syntax tree
pub const Cst = struct {
    const Self = @This();

    arena: std.heap.ArenaAllocator,
    nodes: std.MultiArrayList(Node) = .{},
    root: ?Id = null,

    fn init(allocator: Allocator) Self {
        return Cst{ .arena = std.heap.ArenaAllocator.init(allocator) };
    }

    pub fn deinit(self: *Self) void {
        self.arena.deinit();
    }

    fn ally(self: *Self) Allocator {
        return self.arena.allocator();
    }

    fn add(self: *Self, node: Node) Allocator.Error!Id {
        const id: Id = @enumFromInt(self.nodes.len);
        try self.nodes.append(self.ally(), node);
        return id;
    }

    pub fn get(self: *const Self, id: Id) Node {
        return self.nodes.get(@intFromEnum(id));
    }

    fn displayInner(
        self: Self,
        id: Id,
        depth: usize,
        writer: anytype,
    ) @TypeOf(writer).Error!void {
        try writer.writeByteNTimes(' ', 4 * depth);

        const node = self.get(id);
        try writer.print("{s}", .{@tagName(node)});

        switch (node) {
            .root => |children| {
                try writer.print("\n", .{});
                for (children) |child| {
                    try self.displayInner(child, depth + 1, writer);
                }
            },
            .stmt => |tokens| {
                const loc = Token.rangeLoc(tokens);
                try writer.print(" {}", .{loc});
                for (tokens) |tok| {
                    try writer.print(" {s}", .{tok.slice()});
                }
                try writer.print("\n", .{});
            },
            .block => |block| {
                const loc = Token.rangeLoc(block.head);
                try writer.print(" {}:", .{loc});
                for (block.head) |tok| {
                    try writer.print(" {s}", .{tok.slice()});
                }
                try writer.print("\n", .{});
                for (block.block) |child| {
                    try self.displayInner(child, depth + 1, writer);
                }
            },
        }
    }

    /// dump cst to writer in human-debuggable format
    pub fn display(self: Self, writer: anytype) @TypeOf(writer).Error!void {
        if (self.root) |root| {
            try self.displayInner(root, 0, writer);
        } else {
            try writer.print("<empty cst>", .{});
        }
    }
};

const TokenIterator = struct {
    const Self = @This();

    tokens: []const Token,
    index: usize = 0,

    fn peek(self: Self) ?Token {
        if (self.index >= self.tokens.len) return null;
        return self.tokens[self.index];
    }

    fn advance(self: *Self) void {
        self.index = @min(self.index + 1, self.tokens.len);
    }

    fn next(self: *Self) ?Token {
        const token = self.peek() orelse {
            return null;
        };
        self.advance();
        return token;
    }

    fn since(self: Self, start: usize) []const Token {
        std.debug.assert(start <= self.index);
        return self.tokens[start..self.index];
    }

    fn nextLoc(self: Self) Loc {
        std.debug.assert(self.tokens.len > 0);
        if (self.index == self.tokens.len) {
            return self.tokens[self.tokens.len - 1].end();
        }
        return self.tokens[self.index].loc();
    }
};

fn parseNode(cst: *Cst, eb: *ErrorBuffer, tokens: *TokenIterator) Allocator.Error!?Id {
    var start_index = tokens.index;
    while (tokens.next()) |tok| {
        switch (tok.tag) {
            .rcurly => {
                try eb.add(tok.loc(), .unexpected_right_curly);
                start_index = tokens.index;
                return null;
            },
            .semicolon => {
                return try cst.add(.{ .stmt = tokens.since(start_index) });
            },
            .lcurly => {
                const since = tokens.since(start_index);
                const head = since[0 .. since.len - 1];

                var block = std.ArrayList(Id).init(cst.ally());
                defer block.deinit();

                const ended_properly = while (tokens.peek()) |pk| {
                    if (pk.tag == .rcurly) {
                        tokens.advance();
                        break true;
                    }

                    const node = try parseNode(cst, eb, tokens) orelse {
                        break false;
                    };
                    try block.append(node);
                } else false;

                if (!ended_properly) {
                    try eb.add(tok.loc(), .unmatched_left_curly);
                    try eb.add(tokens.nextLoc(), .expected_end_of_block);
                }

                return try cst.add(.{ .block = .{
                    .head = head,
                    .block = try block.toOwnedSlice(),
                } });
            },
            else => {},
        }
    }

    // end of input
    const extra = tokens.since(start_index);
    if (extra.len > 0) {
        try eb.add(Token.rangeLoc(extra), .unexpected_expression);
    }

    return null;
}

/// cst must outlive tokens
pub fn parse(
    ally: Allocator,
    eb: *ErrorBuffer,
    tokens: []const Token,
) Allocator.Error!Cst {
    var cst = Cst.init(ally);
    errdefer cst.deinit();

    var iter = TokenIterator{ .tokens = tokens };
    var toplevels = std.ArrayList(Id).init(cst.ally());
    defer toplevels.deinit();
    while (try parseNode(&cst, eb, &iter)) |id| {
        try toplevels.append(id);
    }

    cst.root = try cst.add(.{ .root = try toplevels.toOwnedSlice() });

    std.debug.assert(iter.index == tokens.len);

    return cst;
}
