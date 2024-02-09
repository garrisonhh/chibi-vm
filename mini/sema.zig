const std = @import("std");
const Allocator = std.mem.Allocator;
const mini = @import("mini.zig");
const Name = mini.Name;
const Type = mini.Type;
const parser = @import("parser.zig");
const SExpr = parser.SExpr;
const Ast = parser.Ast;

const FirstPass = struct {
    const Self = @This();

    const Decl = struct {
        const Self = @This();

        sexpr: SExpr,
        name: Name,
        type: Type,
        body: *const SExpr,
    };

    arena: std.heap.ArenaAllocator,
    decls: std.AutoHashMapUnmanaged(Name, Decl) = .{},

    root: Name,

    fn init(backing_ally: Allocator, root: Name) Self {
        return Self{
            .arena = std.heap.ArenaAllocator.init(backing_ally),
            .root = root,
        };
    }

    fn deinit(self: *Self, ally: Allocator) void {
        self.decls.deinit(ally);
        self.arena.deinit();
    }
};

pub const TExpr = struct {
    const Self = @This();
    pub const Kind = std.meta.Tag(Data);

    // TODO with some kind of function type polymorphism a lot of these could
    // be represented as normal functions, at least during semantic analysis
    pub const Builtin = enum {
        add,
        sub,
        mul,
    };

    pub const BuiltinApp = struct {
        builtin: Builtin,
        args: []const TExpr,
    };

    pub const Data = union(enum) {
        unit,
        int: i64,
        builtin_app: BuiltinApp,
    };

    type: Type,
    data: Data,

    pub fn format(
        self: Self,
        comptime _: []const u8,
        _: std.fmt.FormatOptions,
        writer: anytype,
    ) @TypeOf(writer).Error!void {
        try writer.print("<{}>", .{self.type});
        switch (self.data) {
            .unit => try writer.writeAll("()"),
            .int => |n| try writer.print("{}", .{n}),
            .builtin_app => |bapp| {
                try writer.print("({s}", .{@tagName(bapp.builtin)});
                for (bapp.args) |child| {
                    try writer.print(" {}", .{child});
                }
                try writer.writeAll(")");
            },
        }
    }
};

pub const Error = Allocator.Error || error{};

/// context for typed intermediate representation
pub const Tir = struct {
    const Self = @This();

    pub const SemanticError = struct {
        pub const Meta = union(enum) {
            // TODO move syntax errors back to their homeland
            expected_syntax: SExpr.Syntax,
            invalid_syntax: SExpr.Syntax,
            expected: Type,
            unknown_ident: mini.String,
            redefinition: Name,
        };

        meta: Meta,
        start: usize,
        len: usize,
    };

    arena: std.heap.ArenaAllocator,
    decls: std.AutoHashMapUnmanaged(Name, TExpr) = .{},

    /// this is set when a semantic error occurs
    err: ?SemanticError = null,

    pub fn init(backing_ally: Allocator) Self {
        return Self{
            .arena = std.heap.ArenaAllocator.init(backing_ally),
        };
    }

    pub fn deinit(self: *Self, ally: Allocator) void {
        self.decls.deinit(ally);
        self.arena.deinit();
    }

    fn semanticError(self: *Self, sexpr: SExpr, meta: SemanticError.Meta) void {
        std.debug.assert(self.err == null);
        self.err = SemanticError{
            .meta = meta,
            .start = sexpr.span_start,
            .len = sexpr.span_len,
        };
    }
};

// common between passes =======================================================

/// get a mini type from a type expression. returns null on semantic error
fn evalType(tir: *Tir, ns: Name, sexpr: SExpr) Error!?Type {
    const arena_ally = tir.arena.allocator();
    switch (sexpr.data) {
        .syntax, .int, .float => {
            tir.semanticError(sexpr, .{ .expected = mini.types.type() });
            return null;
        },
        .ident => |ident| {
            const meta = mini.lookup(ns, ident) orelse {
                tir.semanticError(sexpr, .{ .unknown_ident = ident });
                return null;
            };
            if (!meta.type.eql(mini.types.type())) {
                tir.semanticError(sexpr, .{ .expected = mini.types.type() });
                return null;
            }

            return meta.global.type;
        },
        .list => |list| {
            if (list.len == 0) {
                return mini.types.unit();
            } else if (list[0].data != .syntax) {
                tir.semanticError(sexpr, .{ .expected = mini.types.type() });
                return null;
            }

            switch (list[0].data.syntax) {
                .def, .lambda => {
                    tir.semanticError(sexpr, .{ .expected = mini.types.type() });
                    return null;
                },
                .@"->" => {
                    if (list.len == 1) {
                        tir.semanticError(sexpr, .{ .invalid_syntax = .@"->" });
                        return null;
                    }

                    const params = try arena_ally.alloc(Type, list.len - 2);
                    for (list[1 .. list.len - 1], params) |param_sexpr, *slot| {
                        slot.* = try evalType(tir, ns, param_sexpr) orelse {
                            return null;
                        };
                    }

                    const return_sexpr = list[list.len - 1];
                    const returns = try evalType(tir, ns, return_sexpr) orelse {
                        return null;
                    };

                    return mini.types.function(params, returns);
                },
            }
        },
    }
}

// first pass ==================================================================

// (def <name> <type> <value>)
fn analyzeFirstPassDecl(
    ally: Allocator,
    tir: *Tir,
    fp: *FirstPass,
    sexpr: SExpr,
) Error!void {
    // verify form of def
    if (!sexpr.isSyntaxApp(.def)) {
        tir.semanticError(sexpr, .{ .expected_syntax = .def });
        return;
    }

    const defapp = sexpr.data.list;
    const is_valid_defcall = defapp.len == 4 and defapp[1].data == .ident;

    if (!is_valid_defcall) {
        tir.semanticError(sexpr, .{ .invalid_syntax = .def });
        return;
    }

    // create def
    const name = mini.name(fp.root, defapp[1].data.ident);
    const @"type" = try evalType(tir, fp.root, defapp[2]) orelse {
        return;
    };
    const body = &defapp[3];

    try fp.decls.put(ally, name, FirstPass.Decl{
        .sexpr = sexpr,
        .name = name,
        .type = @"type",
        .body = body,
    });
}

fn firstPass(ally: Allocator, tir: *Tir, fp: *FirstPass, ast: Ast) Error!void {
    for (ast.toplevel.items) |sexpr| {
        try analyzeFirstPassDecl(ally, tir, fp, sexpr);
        if (tir.err != null) break;
    }
}

// second pass =================================================================

fn analyzeLambda(
    ally: Allocator,
    tir: *Tir,
    fp: *const FirstPass,
    sexpr: SExpr,
    expected: Type,
) Error!?TExpr {
    _ = ally;
    _ = tir;
    _ = fp;
    _ = sexpr;
    _ = expected;
    @panic("TODO");
}

fn analyzeExpr(
    ally: Allocator,
    tir: *Tir,
    fp: *const FirstPass,
    sexpr: SExpr,
    expected: Type,
) Error!?TExpr {
    _ = ally;
    _ = fp;

    const arena_ally = tir.arena.allocator();
    _ = arena_ally;
    const meta = mini.types.get(expected);
    const data: TExpr.Data = switch (sexpr.data) {
        .int => |literal| int: {
            if (meta != .int) {
                tir.semanticError(sexpr, .{ .expected = expected });
                return null;
            }

            // parse int and verify that it fits into the expected bytes
            const n: i64 = switch (meta.int) {
                inline 1, 2, 4, 8 => |nbytes| parse: {
                    const I = std.meta.Int(.signed, nbytes * 8);
                    const n = std.fmt.parseInt(I, literal, 0) catch {
                        tir.semanticError(sexpr, .{ .expected = expected });
                        return null;
                    };

                    break :parse n;
                },
                else => unreachable,
            };

            break :int .{ .int = n };
        },
        .list => |list| list: {
            if (list.len == 0) {
                // unit
                if (meta != .unit) {
                    tir.semanticError(sexpr, .{ .expected = expected });
                    return null;
                }

                break :list .unit;
            } else if (list[0].data == .syntax) {
                // syntactic form
                @panic("TODO syntax");
            } else {
                // must be an actual list
                @panic("TODO list types + analyzing them");
            }
        },
        inline else => |_, tag| @panic("TODO analyze " ++ @tagName(tag)),
    };

    return TExpr{
        .type = expected,
        .data = data,
    };
}

fn secondPass(
    ally: Allocator,
    tir: *Tir,
    fp: *const FirstPass,
) Error!void {
    var decls = fp.decls.valueIterator();
    while (decls.next()) |decl| {
        const texpr = try analyzeExpr(ally, tir, fp, decl.body.*, decl.type) orelse {
            return;
        };

        try tir.decls.put(ally, decl.name, texpr);
    }
}

// =============================================================================

/// analyze all of the functions in an ast and add them to the tir
pub fn sema(ally: Allocator, tir: *Tir, ast: Ast) Error!void {
    var fp = FirstPass.init(ally, ast.name);
    defer fp.deinit(ally);

    try firstPass(ally, tir, &fp, ast);
    if (tir.err != null) return;

    std.debug.print("[first pass of {}]\n", .{ast.name});
    var decls = fp.decls.valueIterator();
    while (decls.next()) |decl| {
        std.debug.print("{}: {} = {}\n", .{ decl.name, decl.type, decl.body });
    }
    std.debug.print("\n", .{});

    try secondPass(ally, tir, &fp);
    if (tir.err != null) return;

    std.debug.print("[second pass of {}]\n", .{ast.name});
    var typed_decls = tir.decls.iterator();
    while (typed_decls.next()) |entry| {
        const name = entry.key_ptr.*;
        const texpr = entry.value_ptr.*;

        std.debug.print("{} = {}\n", .{ name, texpr });
    }
    std.debug.print("\n", .{});
}
