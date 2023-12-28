const std = @import("std");
const Allocator = std.mem.Allocator;
const chibi = @import("chibi.zig");

const Source = struct {
    const Self = @This();

    name: [:0]u8,
    contents: [:0]u8,

    pub fn init(
        ally: Allocator,
        name: []const u8,
        contents: []const u8,
    ) Allocator.Error!Self {
        return Self{
            .name = try ally.dupeZ(u8, name),
            .contents = try ally.dupeZ(u8, contents),
        };
    }

    pub fn deinit(self: Self, ally: Allocator) void {
        ally.free(self.name);
        ally.free(self.contents);
    }
};

fn dumpTokens(tokens: ?*chibi.Token) void {
    var trav = tokens;
    while (trav) |token| : (trav = token.next) {
        const slice = token.loc[0..@intCast(token.len)];
        std.debug.print(
            "{[filename]s}:{[lineno]d} `{[str]s}`\n",
            .{
                .filename = token.filename,
                .lineno = token.line_no,
                .str = slice,
            }
        );
    }
}

const InterpretError = error {
};

fn interpret(
    ally: Allocator,
    sources: []const Source,
) (Allocator.Error || InterpretError)!void {
    // load sources as chibi files
    var files = std.ArrayList(*chibi.File).init(ally);
    defer {
        for (files.items) |file| std.c.free(file);
        files.deinit();
    }

    for (sources, 0..) |source, i| {
        const file = chibi.new_file(
            source.name.ptr,
            @intCast(i),
            source.contents.ptr,
        );

        try files.append(file);

        const tokens = chibi.tokenize(file);
        dumpTokens(tokens);
    }
}

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const ally = gpa.allocator();

    var sources = std.ArrayList(Source).init(ally);
    defer {
        for (sources.items) |source| source.deinit(ally);
        sources.deinit();
    }

    try sources.append(try Source.init(
        ally,
        "test_file",
        \\int main(void) {
        \\  return 0;
        \\}
        \\
    ));

    try interpret(ally, sources.items);
}
