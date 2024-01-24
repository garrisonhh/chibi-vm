//! namespace forwards

pub usingnamespace @import("ops.zig");
pub usingnamespace @import("objects.zig");
pub const Env = @import("Env.zig");

const std = @import("std");
const tests = @import("tests.zig");

comptime {
    std.testing.refAllDecls(tests);
}
