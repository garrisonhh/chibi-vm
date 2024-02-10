const std = @import("std");
const Allocator = std.mem.Allocator;
const mini = @import("mini.zig");
const String = mini.String;
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

    pub const BuiltinApp = struct {
        // TODO with some kind of function type polymorphism a these could
        // be represented as normal functions, at least during semantic analysis
        pub const Kind = enum {
            add,
            sub,
            mul,
        };

        kind: BuiltinApp.Kind,
        args: []const TExpr,
    };

    pub const Lambda = struct {
        /// a reference to a parameter, function temporary or stack value
        pub const Variable = union(enum) {
            param: u32,
            value: u32,
        };

        params: []const String,
        body: *const TExpr,
        /// values potentially referenced by variables within this lambda
        values: []const Type,
    };

    pub const If = struct {
        cond: *const TExpr,
        when_true: *const TExpr,
        when_false: *const TExpr,
    };

    pub const Data = union(enum) {
        unit,
        bool: bool,
        int: i64,
        builtin_app: BuiltinApp,
        reference: Name,
        variable: Lambda.Variable,
        lambda: Lambda,
        @"if": If,
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
            .bool => |b| try writer.print("{}", .{b}),
            .int => |n| try writer.print("{}", .{n}),
            .builtin_app => |bapp| {
                try writer.print("({s}", .{@tagName(bapp.kind)});
                for (bapp.args) |child| {
                    try writer.print(" {}", .{child});
                }
                try writer.writeAll(")");
            },
            .lambda => |lambda| {
                try writer.writeAll("(lambda (");
                for (lambda.params, 0..) |param, i| {
                    if (i > 0) try writer.writeAll(" ");
                    try writer.print("{}", .{param});
                }
                try writer.print(") {})", .{lambda.body});
            },
            .@"if" => |meta| {
                try writer.print("(if {} {} {})", .{
                    meta.cond,
                    meta.when_true,
                    meta.when_false,
                });
            },
            .variable => |variable| {
                switch (variable) {
                    .param, .value => |index| {
                        try writer.print("{s}-{d}", .{ @tagName(variable), index });
                    },
                }
            },
            .reference => |name| {
                try writer.print("{}", .{name});
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
            not_enough_args,
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

    fn alloc(self: *Self, comptime T: type, count: usize) Allocator.Error![]T {
        const arena_ally = self.arena.allocator();
        return arena_ally.alloc(T, count);
    }

    fn create(self: *Self, comptime T: type) Allocator.Error!*T {
        const arena_ally = self.arena.allocator();
        return arena_ally.create(T);
    }

    /// create a ptr and write to it
    fn box(self: *Self, comptime T: type, value: T) Allocator.Error!*T {
        const ptr = try self.create(T);
        ptr.* = value;
        return ptr;
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
                else => {
                    tir.semanticError(sexpr, .{ .expected = mini.types.type() });
                    return null;
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

/// symbol table for the current function. mini does not support closures so
/// this is all of the symbols sema needs
const FunctionSymbols = struct {
    const Self = @This();

    vars: std.ArrayListUnmanaged(Type) = .{},
    params: []const Type,

    table: std.AutoArrayHashMapUnmanaged(String, TExpr.Lambda.Variable) = .{},
    /// indices of currently accessible scopes within the table
    scopes: std.ArrayListUnmanaged(usize) = .{},

    fn init(
        ally: Allocator,
        param_names: []const String,
        param_types: []const Type,
    ) Allocator.Error!Self {
        var self = Self{
            .params = try ally.dupe(Type, param_types),
        };

        for (param_names, 0..) |name, i| {
            try self.table.put(ally, name, .{ .param = @intCast(i) });
        }

        return self;
    }

    fn deinit(self: *Self, ally: Allocator) void {
        self.scopes.deinit(ally);
        self.table.deinit(ally);
        self.vars.deinit(ally);
        ally.free(self.params);
    }

    const PutError = Allocator.Error || error{NameConflict};

    // TODO enter and exit for let blocks

    /// create a variable in the function
    fn put(
        self: Self,
        ally: Allocator,
        ident: String,
        var_type: Type,
    ) PutError!TExpr.Lambda.Variable {
        const res = try self.table.getOrPut(ally, ident);
        if (res.found_existing) {
            return PutError.NameConflict;
        }

        const variable = TExpr.Lambda.Variable{
            .value = @intCast(self.vars.items.len),
        };
        try self.vars.append(ally, var_type);

        res.value_ptr.* = variable;

        return variable;
    }

    /// retrieve the name for an identifer
    fn get(self: Self, ident: String) ?TExpr.Lambda.Variable {
        return self.table.get(ident);
    }
};

const Context = struct {
    ally: Allocator,
    /// contains module-specific names
    fp: *const FirstPass,
    st: ?FunctionSymbols = null,

    fn init(ally: Allocator, fp: *const FirstPass) Context {
        return Context{
            .ally = ally,
            .fp = fp,
        };
    }

    fn deinit(ctx: *Context) void {
        std.debug.assert(ctx.st == null);
    }

    fn enterFunction(
        ctx: *Context,
        param_names: []const String,
        param_types: []const Type,
    ) Allocator.Error!void {
        std.debug.assert(ctx.st == null);
        ctx.st = try FunctionSymbols.init(ctx.ally, param_names, param_types);
    }

    fn exitFunction(ctx: *Context) void {
        std.debug.assert(ctx.st != null);
        ctx.st.?.deinit(ctx.ally);
        ctx.st = null;
    }

    /// copy variable values from the function symbol table for building a
    /// TExpr.Lambda
    fn getFunctionValues(
        ctx: Context,
        ally: Allocator,
    ) Allocator.Error![]const Type {
        std.debug.assert(ctx.st != null);
        return ally.dupe(Type, ctx.st.?.vars.items);
    }

    const Lookup = union(enum) {
        name: Name,
        variable: TExpr.Lambda.Variable,
    };

    /// attempt to find an identifier in all of the relevant scopes
    fn lookup(ctx: Context, ident: String) ?Lookup {
        if (ctx.st) |st| {
            if (st.get(ident)) |variable| {
                return .{ .variable = variable };
            }
        }

        if (ctx.fp.decls.get(mini.name(ctx.fp.root, ident))) |fp_decl| {
            return .{ .name = fp_decl.name };
        } else if (mini.lookup(ctx.fp.root, ident)) |global| {
            return .{ .name = global.name };
        }

        return null;
    }
};

fn analyzeBuiltinApp(
    ally: Allocator,
    tir: *Tir,
    ctx: *Context,
    sexpr: SExpr,
    expected: mini.Type,
) Error!?TExpr.BuiltinApp {
    std.debug.assert(sexpr.data == .list);
    std.debug.assert(sexpr.data.list[0].data == .syntax);

    const meta = mini.types.get(expected);
    const list = sexpr.data.list;

    const kind: TExpr.BuiltinApp.Kind = switch (list[0].data.syntax) {
        .@"+" => .add,
        .@"-" => .sub,
        .@"*" => .mul,
        else => unreachable,
    };

    const args: []const TExpr = switch (kind) {
        // binary reductions on integer or float
        .add, .sub, .mul => args: {
            if (meta != .int and meta != .float) {
                tir.semanticError(sexpr, .{ .expected = expected });
                return null;
            } else if (list.len < 3) {
                tir.semanticError(sexpr, .not_enough_args);
                return null;
            }

            const args = try tir.alloc(TExpr, list.len - 1);
            for (list[1..], args) |arg_sexpr, *arg| {
                arg.* = try analyzeExpr(ally, tir, ctx, arg_sexpr, expected) orelse {
                    return null;
                };
            }

            break :args args;
        },
    };

    return TExpr.BuiltinApp{
        .kind = kind,
        .args = args,
    };
}

fn analyzeLambda(
    ally: Allocator,
    tir: *Tir,
    ctx: *Context,
    sexpr: SExpr,
    meta: mini.TypeInfo,
) Error!?TExpr.Lambda {
    std.debug.assert(sexpr.isSyntaxApp(.lambda));
    std.debug.assert(meta == .function);

    const list = sexpr.data.list;
    if (list.len != 3) {
        tir.semanticError(sexpr, .{ .invalid_syntax = .lambda });
        return null;
    }

    // params
    const params_sexpr = list[1];
    if (params_sexpr.data != .list) {
        tir.semanticError(sexpr, .{ .invalid_syntax = .lambda });
        return null;
    }

    const params = try tir.alloc(String, params_sexpr.data.list.len);
    for (params_sexpr.data.list, params) |param_sexpr, *param| {
        if (param_sexpr.data != .ident) {
            tir.semanticError(sexpr, .{ .invalid_syntax = .lambda });
            return null;
        }

        param.* = param_sexpr.data.ident;
    }

    // body with funciton scope
    try ctx.enterFunction(params, meta.function.params);
    defer ctx.exitFunction();

    const return_type = meta.function.returns;
    const body = try analyzeExpr(ally, tir, ctx, list[2], return_type) orelse {
        return null;
    };

    const values = try ctx.getFunctionValues(tir.arena.allocator());

    return TExpr.Lambda{
        .params = params,
        .body = try tir.box(TExpr, body),
        .values = values,
    };
}

fn analyzeIf(
    ally: Allocator,
    tir: *Tir,
    ctx: *Context,
    sexpr: SExpr,
    expected: Type,
) Error!?TExpr.If {
    std.debug.assert(sexpr.isSyntaxApp(.@"if"));

    const list = sexpr.data.list;
    if (list.len != 4) {
        tir.semanticError(sexpr, .{ .invalid_syntax = .@"if" });
        return null;
    }

    const cond = try analyzeExpr(ally, tir, ctx, list[1], mini.types.@"bool"()) orelse {
        return null;
    };
    const when_true = try analyzeExpr(ally, tir, ctx, list[2], expected) orelse {
        return null;
    };
    const when_false = try analyzeExpr(ally, tir, ctx, list[3], expected) orelse {
        return null;
    };

    return TExpr.If{
        .cond = try tir.box(TExpr, cond),
        .when_true = try tir.box(TExpr, when_true),
        .when_false = try tir.box(TExpr, when_false),
    };
}

fn analyzeExpr(
    ally: Allocator,
    tir: *Tir,
    ctx: *Context,
    sexpr: SExpr,
    expected: Type,
) Error!?TExpr {
    const meta = mini.types.get(expected);
    const data: TExpr.Data = switch (sexpr.data) {
        .ident => |ident| ident: {
            const lookup = ctx.lookup(ident) orelse {
                tir.semanticError(sexpr, .{ .unknown_ident = ident });
                return null;
            };

            break :ident switch (lookup) {
                .name => |name| .{ .reference = name },
                .variable => |variable| .{ .variable = variable },
            };
        },
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
        .syntax => |syntax| switch (syntax) {
            // values that are syntax
            .true, .false => bools: {
                if (meta != .bool) {
                    tir.semanticError(sexpr, .{ .expected = expected });
                    return null;
                }

                break :bools .{ .bool = syntax == .true };
            },

            // everything else should be an application
            else => {
                tir.semanticError(sexpr, .{ .expected = expected });
                return null;
            },
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
                switch (list[0].data.syntax) {
                    .def, .@"->", .true, .false => {
                        tir.semanticError(sexpr, .{ .expected = expected });
                        return null;
                    },
                    .lambda => {
                        const lambda = try analyzeLambda(ally, tir, ctx, sexpr, meta) orelse {
                            return null;
                        };
                        break :list .{ .lambda = lambda };
                    },
                    .@"if" => {
                        const @"if" = try analyzeIf(ally, tir, ctx, sexpr, expected) orelse {
                            return null;
                        };
                        break :list .{ .@"if" = @"if" };
                    },
                    .@"+", .@"-", .@"*" => {
                        const bapp = try analyzeBuiltinApp(ally, tir, ctx, sexpr, expected) orelse {
                            return null;
                        };
                        break :list .{ .builtin_app = bapp };
                    },
                }
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
    var ctx = Context.init(ally, fp);
    defer ctx.deinit();

    var decls = fp.decls.valueIterator();
    while (decls.next()) |decl| {
        const texpr = try analyzeExpr(ally, tir, &ctx, decl.body.*, decl.type) orelse {
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
        std.debug.print("{}: {}\n", .{ decl.name, decl.type });
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
