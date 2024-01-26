//! tests for translation units, modules, and linking

const std = @import("std");
const vm = @import("vm");

const ally = std.testing.allocator;

/// builds the classic `fn add(i32, i32) i32` example
fn buildAddFunction() !vm.Unit {
    var b = vm.Builder.init(ally);
    defer b.deinit();

    _ = try b.define("add", .exported, .code);
    try b.param(0);
    try b.op(.{ .load = .{
        .width = .half,
        .offset = 0,
    } });
    try b.param(1);
    try b.op(.{ .load = .{
        .width = .half,
        .offset = 0,
    } });
    try b.op(.{ .add = .half });
    try b.op(.{ .ret = 2 });

    return try b.build(ally);
}

test "singular translation unit" {
    var env = try vm.Env.init(ally, .{});
    defer env.deinit(ally);

    const unit = try buildAddFunction();
    defer unit.deinit(ally);

    const mod = try vm.link(ally, &.{unit});
    defer mod.deinit(ally);

    try env.push(i32, 22);
    try env.push(i32, 34);
    try env.exec(ally, mod, "add");
    const res = try env.pop(i32);

    try std.testing.expectEqual(@as(i32, 56), res);
}

test "basic linking" {
    var env = try vm.Env.init(ally, .{});
    defer env.deinit(ally);

    const add_unit = try buildAddFunction();
    defer add_unit.deinit(ally);

    var b = vm.Builder.init(ally);
    defer b.deinit();

    const add_label = try b.symbol("add");

    _ = try b.define("call_add", .exported, .code);
    try b.param(0);
    try b.op(.{ .load = .{
        .width = .half,
        .offset = 0,
    } });
    try b.param(1);
    try b.op(.{ .load = .{
        .width = .half,
        .offset = 0,
    } });
    try b.op(.{ .label = add_label });
    try b.op(.call);
    try b.op(.{ .ret = 2 });

    const call_add_unit = try b.build(ally);
    defer call_add_unit.deinit(ally);

    const mod = try vm.link(ally, &.{add_unit, call_add_unit});
    defer mod.deinit(ally);

    try env.push(i32, 22);
    try env.push(i32, 34);
    try env.exec(ally, mod, "call_add");
    const res = try env.pop(i32);

    try std.testing.expectEqual(@as(i32, 56), res);
}