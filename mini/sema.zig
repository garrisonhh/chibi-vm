const std = @import("std");
const mini = @import("mini.zig");
const Name = mini.Name;
const Type = mini.Type;
const parser = @import("parser.zig");
const SExpr = parser.SExpr;
const Ast = parser.Ast;

pub const TExpr = struct {
    const Self = @This();
    pub const Kind = std.meta.Tag(Data);

    pub const Data = union(enum) {
    };

    type: Type,
    data: Data,
};

/// typed intermediate representation
pub const Tir = struct {

};