//! namespace forwards

pub usingnamespace @import("vm/ops.zig");
pub usingnamespace @import("vm/objects.zig");
pub const Env = @import("vm/Env.zig");

const std = @import("std");
const tests = @import("vm/tests.zig");

comptime {
    std.testing.refAllDecls(tests);
}