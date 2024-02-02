const std = @import("std");
const mini = @import("mini.zig");
const Name = mini.Name;
const Type = mini.Type;
const parser = @import("parser.zig");
const Expr = parser.Expr;
const Ast = parser.Ast;

pub const Decl = struct {
    const Self = @This();

    name: Name,
    type: mini.Type,
};