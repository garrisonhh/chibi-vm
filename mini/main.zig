const std = @import("std");
const vm = @import("vm");
const mini = @import("mini.zig");
const parser = @import("parser.zig");
const sema = @import("sema.zig");
const codegen = @import("codegen.zig");

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
        const stop_offset = std.mem.indexOf(u8, source[start + len ..], "\n");
        const text_stop = if (stop_offset) |offset| start + len + offset else source.len;
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

        // display line
        var buf: [64]u8 = undefined;
        const meta = std.fmt.bufPrint(&buf, "{d} | ", .{lineno}) catch {
            unreachable;
        };
        try writer.print("{s}{s}\n", .{ meta, loc.text });

        // highlight
        try writer.writeByteNTimes(' ', meta.len + loc.char_index);
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
        const line_count = 1 + std.mem.count(u8, loc.text, "\n");
        std.debug.assert(line_count > 1);
        const last_newline_offset = std.mem.lastIndexOf(u8, loc.text, "\n").?;
        const following_char_index = loc.text.len - last_newline_offset - 1;

        // calculate offset for meta
        const final_lineno = loc.line_index + line_count;
        const meta_fmt = "{d} | ";
        const max_meta_len = std.fmt.count(meta_fmt, .{final_lineno});

        // leading arrow
        try writer.writeByteNTimes(' ', max_meta_len + loc.char_index);
        try writer.writeAll("v\n");

        // display lines
        var i: usize = 0;
        var lines = std.mem.split(u8, loc.text, "\n");
        while (lines.next()) |line| : (i += 1) {
            // elide massive multiline errors
            if (line_count > 7 and (i >= 3 and i < line_count - 3)) {
                if (i == 3) {
                    try writer.writeByteNTimes(' ', max_meta_len);
                    try writer.writeAll("...\n");
                }
                continue;
            }

            const lineno = loc.line_index + i + 1;

            var buf: [64]u8 = undefined;
            const meta = std.fmt.bufPrint(&buf, meta_fmt, .{lineno}) catch {
                unreachable;
            };
            try writer.writeByteNTimes(' ', meta.len - max_meta_len);
            try writer.print("{s}{s}\n", .{ meta, line });
        }

        // following arrow
        try writer.writeByteNTimes(' ', max_meta_len + following_char_index);
        try writer.writeAll("^\n");
    }

    fn display(loc: SourceLoc, writer: anytype) @TypeOf(writer).Error!void {
        const lineno = loc.line_index + 1;
        const charno = loc.char_index + 1;
        try writer.print("[{}:{}:{}]\n", .{ loc.name, lineno, charno });

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

/// if there is a syntax error, display it to stderr and exit
fn checkSemanticError(ast: parser.Ast, tir: sema.Tir) !void {
    const err = tir.err orelse return;

    var buf: [1024]u8 = undefined;
    const bufPrint = std.fmt.bufPrint;

    const desc: []const u8 = switch (err.meta) {
        .expected_syntax => |s| try bufPrint(&buf, "expected {s}", .{@tagName(s)}),
        .invalid_syntax => |s| try bufPrint(&buf, "invalid {s}", .{@tagName(s)}),
        .expected => |ty| try bufPrint(&buf, "expected a {}", .{ty}),
        .unknown_ident => |ident| try bufPrint(&buf, "unknown identifier `{}`", .{ident}),
        .redefinition => |name| try bufPrint(&buf, "`{}` is already defined", .{name}),
        .invalid_operator => |ty| try bufPrint(&buf, "invalid operator for {}", .{ty}),
        .wrong_argument_count => "wrong argument count",
        .undeducable_type => "impossible to deduce the type of this expression",
        .expected_function => "expected a function",
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

    // parse
    var ast = try parser.parse(ally, "test",
        \\(def one i32 1)
        \\
        \\(def f (-> i32 bool) (lambda (x)
        \\  (g x)))
        \\
        \\(def g (-> i32 bool) (lambda (x)
        \\  (> x one)))
        \\
    );
    defer ast.deinit();

    try checkSyntaxError(ast);

    std.debug.print("[parsed exprs]\n", .{});
    for (ast.toplevel.items) |expr| {
        std.debug.print("{}\n", .{expr});
    }
    std.debug.print("\n", .{});

    // analyze
    var tir = sema.Tir.init(ally);
    defer tir.deinit(ally);

    try sema.sema(ally, &tir, ast);

    try checkSemanticError(ast, tir);

    // generate code
    var units = std.ArrayList(vm.Unit).init(ally);
    defer {
        for (units.items) |unit| unit.deinit(ally);
        units.deinit();
    }

    var decls = tir.decls.iterator();
    while (decls.next()) |entry| {
        const unit = try codegen.codegen(ally, entry.key_ptr.*, entry.value_ptr.*);
        try units.append(unit);
    }

    const module = try vm.link(ally, units.items);
    defer module.deinit(ally);

    std.debug.print("DATA {}\n", .{std.fmt.fmtSliceHexLower(module.data)});
    std.debug.print("CODE {}\n", .{std.fmt.fmtSliceHexLower(module.code)});

    std.debug.print("[compiled]\n", .{});
    var code_iter = vm.iterate(module.code);
    while (code_iter.next()) |eop| {
        std.debug.print("{d: >6} | {s}", .{ eop.offset, @tagName(eop.byteop.opcode) });

        if (eop.byteop.opcode.meta().sized) {
            std.debug.print(" {s}", .{@tagName(eop.byteop.width)});
        }

        if (eop.extra.len > 0) {
            if (eop.byteop.opcode == .local) {
                std.debug.print(" {d}", .{std.mem.readIntSliceNative(i16, eop.extra)});
            } else {
                std.debug.print(" 0x{}", .{std.fmt.fmtSliceHexLower(eop.extra)});
            }
        }

        std.debug.print("\n", .{});
    }
    std.debug.print("\n", .{});

    // execute
    var env = try vm.Env.init(ally, .{});
    defer env.deinit(ally);

    var state = try vm.Env.load(ally, module);
    defer state.deinit(ally);

    const func = "test.f";

    for ([_]i32{ 1, 2, 3 }) |in| {
        try env.push(i32, in);
        try env.exec(&state, func);
        const res = try env.pop(bool);
        std.debug.print("({s} {}) = {}\n", .{ func, in, res });
    }
}

// testing =====================================================================

comptime {
    std.testing.refAllDeclsRecursive(@This());
}

// TODO end-to-end testing
