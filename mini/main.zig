const std = @import("std");
const parser = @import("parser.zig");
const mini = @import("mini.zig");

/// useful for displaying errors nicely
const SourceLoc = struct {
    name: mini.Name,
    line_index: usize,
    char_index: usize,
    len: usize,
    text: []const u8,

    fn init(
        name: mini.Name,
        source: []const u8,
        start: usize,
        len: usize,
    ) SourceLoc {
        const text_start = std.mem.lastIndexOf(u8, source[0..start], "\n") orelse 0;
        const stop_offset = std.mem.indexOf(u8, source[start..], "\n");
        const text_stop = if (stop_offset) |offset| start + offset else source.len;
        const text = source[text_start..text_stop];

        const line_index = std.mem.count(u8, source[0..start], "\n");
        const char_index = start - text_start;

        return SourceLoc{
            .name = name,
            .line_index = line_index,
            .char_index = char_index,
            .len = len,
            .text = text,
        };
    }

    fn displaySingleLineContext(
        loc: SourceLoc,
        writer: anytype,
    ) @TypeOf(writer).Error!void {
        const lineno = loc.line_index + 1;

        var buf: [64]u8 = undefined;
        const meta = std.fmt.bufPrint(&buf, "{d} | ", .{lineno}) catch {
            unreachable;
        };

        try writer.print("{s}{s}\n", .{meta, loc.text});

        const hl_offset = meta.len + loc.char_index;
        try writer.writeByteNTimes(' ', hl_offset);

        if (loc.len == 0) {
            try writer.writeByte('^');
        } else {
            try writer.writeByteNTimes('~', loc.len);
        }
        try writer.writeByte('\n');
    }

    fn displayMultiLineContext(
        loc: SourceLoc,
        writer: anytype,
    ) @TypeOf(writer).Error!void {
        _ = loc;
        @panic("TODO display multiline context");
    }

    fn display(loc: SourceLoc, writer: anytype) @TypeOf(writer).Error!void {
        const lineno = loc.line_index + 1;
        const charno = loc.char_index + 1;
        try writer.print("[{}:{}:{}]\n", .{loc.name, lineno, charno});

        const multiline = std.mem.count(u8, loc.text, "\n") > 0;
        if (multiline) {
            try loc.displayMultiLineContext(writer);
        } else {
            try loc.displaySingleLineContext(writer);
        }
    }
};

fn miniError(
    loc: SourceLoc,
    comptime fmt: []const u8,
    args: anytype,
) !void {
    const stderr = std.io.getStdErr().writer();
    var bw = std.io.bufferedWriter(stderr);
    const writer = bw.writer();

    try writer.print("error: " ++ fmt ++ "\n", args);
    try loc.display(writer);

    try bw.flush();
}

/// if there is a syntax error, display it to stderr and exit
fn checkSyntaxError(ast: parser.Ast) !void {
    const err = ast.err orelse return;

    const desc: []const u8 = switch (err.kind) {
        .unexpected_eof => "unexpected end of file",
        .invalid_character => "invalid character",
        .invalid_word => "invalid word",
        .unmatched_rparen => "unmatched ')'",
        .unfinished_list => "unfinished list",
    };
    const loc = SourceLoc.init(ast.name, ast.text, err.start, err.len);

    try miniError(loc, "{s}", .{desc});

    std.process.exit(1);
}

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const ally = gpa.allocator();

    mini.init();
    defer mini.deinit();

    var ast = try parser.parse(ally, "test",
        \\(as i32 4))
        \\
    );
    defer ast.deinit();

    try checkSyntaxError(ast);

    for (ast.toplevel.items) |expr| {
        std.debug.print("{}\n", .{expr});
    }
}

// testing =====================================================================

comptime {
    std.testing.refAllDeclsRecursive(@This());
}

// TODO end-to-end testing
