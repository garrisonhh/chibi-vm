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

        name: Name,
        type: Type,
        body: *const SExpr,
    };

    arena: std.heap.ArenaAllocator,
    decls: std.ArrayListUnmanaged(Decl) = .{},

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

    pub const Data = union(enum) {
        call: []const TExpr,
    };

    type: Type,
    data: Data,
};

pub const Function = struct {
    const Self = @This();

    name: Name,
    type: Type,
    body: TExpr,
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
            expected_type,
            unknown_ident: mini.String,
        };

        meta: Meta,
        start: usize,
        len: usize,
    };

    /// owns functions
    arena: std.heap.ArenaAllocator,
    functions: std.ArrayListUnmanaged(Function) = .{},

    /// this is set when a semantic error occurs
    err: ?SemanticError = null,

    pub fn init(backing_ally: Allocator) Self {
        return Self{
            .arena = std.heap.ArenaAllocator.init(backing_ally),
        };
    }

    pub fn deinit(self: *Self) void {
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
fn parseType(tir: *Tir, ns: Name, sexpr: SExpr) Error!?Type {
    const arena_ally = tir.arena.allocator();
    switch (sexpr.data) {
        .syntax, .int, .float => {
            tir.semanticError(sexpr, .expected_type);
            return null;
        },
        .ident => |ident| {
            const meta = mini.lookup(ns, ident) orelse {
                tir.semanticError(sexpr, .{ .unknown_ident = ident });
                return null;
            };
            if (!meta.type.eql(mini.types.type())) {
                tir.semanticError(sexpr, .expected_type);
                return null;
            }

            return meta.global.type;
        },
        .list => |list| {
            if (list.len == 0) {
                return mini.types.unit();
            } else if (list[0].data != .syntax) {
                tir.semanticError(sexpr, .expected_type);
                return null;
            }

            switch (list[0].data.syntax) {
                .def, .lambda => {
                    tir.semanticError(sexpr, .expected_type);
                    return null;
                },
                .@"->" => {
                    if (list.len == 1) {
                        tir.semanticError(sexpr, .{ .invalid_syntax = .@"->" });
                        return null;
                    }

                    const params = try arena_ally.alloc(Type, list.len - 2);
                    for (list[1 .. list.len - 1], params) |param_sexpr, *slot| {
                        slot.* = try parseType(tir, ns, param_sexpr) orelse {
                            return null;
                        };
                    }

                    const return_sexpr = list[list.len - 1];
                    const returns = try parseType(tir, ns, return_sexpr) orelse {
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
fn parseFirstPassDecl(
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
    const @"type" = try parseType(tir, fp.root, defapp[2]) orelse {
        return;
    };
    const body = &defapp[3];

    try fp.decls.append(ally, FirstPass.Decl{
        .name = name,
        .type = @"type",
        .body = body,
    });
}

fn firstPass(ally: Allocator, tir: *Tir, fp: *FirstPass, ast: Ast) Error!void {
    for (ast.toplevel.items) |sexpr| {
        try parseFirstPassDecl(ally, tir, fp, sexpr);
        if (tir.err != null) break;
    }
}

// second pass =================================================================

// =============================================================================

/// analyze all of the functions in an ast and add them to the tir
pub fn sema(ally: Allocator, tir: *Tir, ast: Ast) Error!void {
    var fp = FirstPass.init(ally, ast.name);
    defer fp.deinit(ally);

    try firstPass(ally, tir, &fp, ast);
    if (tir.err != null) return;

    std.debug.print("[first pass of {}]\n", .{ast.name});
    for (fp.decls.items) |decl| {
        std.debug.print("{}: {} = {}\n", .{ decl.name, decl.type, decl.body });
    }
}
