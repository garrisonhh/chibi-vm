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
    try env.loadExec(ally, mod, "add");
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

    try b.define("call_add", .exported, .code);
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
    try env.loadExec(ally, mod, "call_add");
    const res = try env.pop(i32);

    try std.testing.expectEqual(@as(i32, 56), res);
}

test "massive amount of units" {
    const count = 32 * 1024;

    var arena = std.heap.ArenaAllocator.init(ally);
    defer arena.deinit();
    const arena_ally = arena.allocator();

    var prng = std.rand.DefaultPrng.init(0);

    var values = std.StringHashMap(u64).init(ally);
    defer values.deinit();

    var units = std.ArrayList(vm.Unit).init(ally);
    defer {
        for (units.items) |unit| unit.deinit(ally);
        units.deinit();
    }

    for (0..count) |i| {
        var b = vm.Builder.init(ally);
        defer b.deinit();

        const name = try std.fmt.allocPrint(arena_ally, "f{d}", .{i});
        const value = prng.random().uintAtMost(u64, std.math.maxInt(u64));

        try values.put(name, value);

        try b.define(name, .exported, .code);
        try b.constant(u64, value);
        try b.op(.{ .ret = 0 });

        const unit = try b.build(ally);
        try units.append(unit);
    }

    const mod = try vm.link(ally, units.items);
    defer mod.deinit(ally);

    var env = try vm.Env.init(ally, .{});
    defer env.deinit(ally);

    var state = try vm.Env.load(ally, mod);
    defer state.deinit(ally);

    var entries = values.iterator();
    while (entries.next()) |entry| {
        const name = entry.key_ptr.*;
        const expected = entry.value_ptr.*;

        try env.exec(&state, name);

        const actual = try env.pop(u64);
        try std.testing.expectEqual(expected, actual);
    }
}