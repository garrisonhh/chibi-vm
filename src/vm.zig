//! namespace forwards

pub usingnamespace @import("vm/ops.zig");
pub usingnamespace @import("vm/objects.zig");
pub const Env = @import("vm/Env.zig");

// tests =======================================================================

const std = @import("std");
const vm = @This();

test "add" {
    const ally = std.testing.allocator;
    var builder = vm.Builder.init(ally);

    try builder.global("add");
    try builder.op(.{ .add = .word });
    try builder.op(.ret);

    const obj = try builder.build();
    defer obj.deinit(ally);

    var so = try vm.link(ally, &.{obj});
    defer so.deinit(ally);

    var env = try vm.Env.init(ally, .{});
    defer env.deinit(ally);

    try env.push(i64, 24);
    try env.push(i64, 18);
    try env.exec(&so, "add");
    const res = try env.pop(i64);

    try std.testing.expectEqual(@as(i64, 42), res);
}

test "fibonacci" {
    return error.TodoImplementIfElse;
}
