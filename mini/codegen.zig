const std = @import("std");
const Allocator = std.mem.Allocator;
const vm = @import("vm");
const Builder = vm.Builder;
const Label = vm.Builder.Label;
const mini = @import("mini.zig");
const Name = mini.Name;
const Type = mini.Type;
const sema = @import("sema.zig");
const TExpr = sema.TExpr;

pub const Error = Allocator.Error || vm.Builder.SymbolError;

/// context for current function being lowered
const Context = struct {
    stack_size: usize,
    // map value index -> stack offset
    stack_offsets: []const usize,

    fn init(ally: Allocator, values: []const Type) Allocator.Error!Context {
        // generate stack offsets and size TODO sort values first
        var stack_size: usize = 0;
        const stack_offsets = try ally.alloc(usize, values.len);
        for (values, stack_offsets) |ty, *offset| {
            const size = mini.types.sizeOf(ty);
            const aln = mini.types.alignOf(ty);

            stack_size = std.mem.alignForward(usize, stack_size, aln);
            offset.* = stack_size;
            stack_size += size;
        }

        return Context{
            .stack_size = stack_size,
            .stack_offsets = stack_offsets,
        };
    }

    fn deinit(ctx: *Context, ally: Allocator) void {
        ally.free(ctx.stack_offsets);
    }

    /// local i16 offset of a stack variable index
    fn localOffsetOf(ctx: Context, index: usize) i16 {
        return @intCast(ctx.stack_offsets[index]);
    }
};

fn bytesFromExpr(buf: []u8, expr: TExpr) void {
    switch (expr.data) {
        .unit => {},
        .int => |n| {
            const meta = mini.types.get(expr.type);
            switch (meta.int) {
                inline 1, 2, 4, 8 => |nbytes| {
                    const I = std.meta.Int(.signed, nbytes * 8);
                    const bytes = std.mem.asBytes(&@as(I, @intCast(n)));
                    @memset(buf, 0);
                    @memcpy(buf[0..bytes.len], bytes);
                },
                else => unreachable,
            }
        },
        else => @panic("TODO"),
    }
}

fn lowerAddr(b: *Builder, ctx: *Context, expr: TExpr) Error!void {
    switch (expr.data) {
        .unit => {
            const label = try b.data(&.{});
            try b.op(.{ .data = label });
        },
        .reference => |name| {
            const label = try b.symbol(mini.namestring(name));
            const meta = mini.types.get(expr.type);

            if (meta == .function) {
                try b.op(.{ .label = label });
            } else {
                try b.op(.{ .data = label });
            }
        },
        .variable => |variable| switch (variable) {
            .param => |idx| try b.param(idx),
            .value => |idx| try b.op(.{ .local = ctx.localOffsetOf(idx) }),
        },

        else => std.debug.panic("TODO lower addr of {s}", .{@tagName(expr.data)}),
    }
}

fn lowerValue(b: *Builder, ctx: *Context, expr: TExpr) Error!void {
    switch (expr.data) {
        .unit => {
            try b.constant(u64, undefined);
        },
        .bool => |val| {
            try b.constant(bool, val);
        },
        .int => |n| switch (mini.types.get(expr.type).int) {
            inline 1, 2, 4, 8 => |nbytes| {
                const I = std.meta.Int(.signed, nbytes * 8);
                try b.constant(I, @as(I, @intCast(n)));
            },
            else => unreachable,
        },
        .variable, .reference => {
            // TODO wide types? I think sema will guard against them being
            // generated and assignment will copy by address, but there are
            // weird cases like function calls with structs passed by value? idk
            // maybe I'm overthinking it, maybe it needs more thought
            const size = mini.types.sizeOf(expr.type);
            const width = vm.Width.fromBytesFit(size).?;

            try lowerAddr(b, ctx, expr);
            try b.op(.{ .load = .{
                .width = width,
                .offset = 0,
            } });
        },
        .builtin_app => |bapp| switch (bapp.kind) {
            // binary math reductions
            .add, .sub, .mul => |kind| {
                for (bapp.args) |arg| {
                    try lowerValue(b, ctx, arg);
                }

                const meta = mini.types.get(expr.type);
                const width = vm.Width.fromBytesExact(mini.types.sizeOf(expr.type)).?;
                const op: vm.Op = switch (meta) {
                    .int => switch (kind) {
                        .add => .{ .add = width },
                        .sub => .{ .sub = width },
                        .mul => .{ .muli = width },
                        else => unreachable,
                    },
                    .float => switch (kind) {
                        .add => .{ .addf = width },
                        .sub => .{ .subf = width },
                        .mul => .{ .mulf = width },
                        else => unreachable,
                    },
                    else => unreachable,
                };

                for (0..bapp.args.len - 1) |_| {
                    try b.op(op);
                }
            },

            .eq => {
                std.debug.assert(bapp.args.len == 2);
                try lowerValue(b, ctx, bapp.args[0]);
                try lowerValue(b, ctx, bapp.args[1]);

                const size = mini.types.sizeOf(bapp.args[0].type);
                const width = vm.Width.fromBytesExact(size).?;
                try b.op(.{ .eq = width });
            },
            .gt, .lt => |kind| {
                std.debug.assert(bapp.args.len == 2);
                try lowerValue(b, ctx, bapp.args[0]);
                try lowerValue(b, ctx, bapp.args[1]);

                const size = mini.types.sizeOf(bapp.args[0].type);
                const width = vm.Width.fromBytesExact(size).?;
                const signed = mini.types.get(bapp.args[0].type) == .int;

                const op: vm.Op = if (signed) switch (kind) {
                    .gt => .{ .gti = width },
                    .lt => .{ .gti = width },
                    else => unreachable,
                } else switch (kind) {
                    .gt => .{ .gtu = width },
                    .lt => .{ .gtu = width },
                    else => unreachable,
                };

                try b.op(op);
            },
        },
        .@"if" => |meta| {
            const when_false = try b.backref();
            const end = try b.backref();

            // condition
            try lowerValue(b, ctx, meta.cond.*);
            try b.op(.{ .jz = .{
                .width = .byte,
                .dest = when_false,
            } });

            // when true
            try lowerValue(b, ctx, meta.when_true.*);
            try b.op(.{ .jump = end });

            // when false
            b.resolve(when_false);
            try lowerValue(b, ctx, meta.when_false.*);

            b.resolve(end);
        },

        else => std.debug.panic("TODO lower {s}", .{@tagName(expr.data)}),
    }
}

fn lowerLambda(b: *Builder, lambda: TExpr.Lambda) Error!Label {
    var ctx = try Context.init(b.ally, lambda.values);
    defer ctx.deinit(b.ally);

    const label = try b.label();
    try b.op(.{ .enter = @intCast(ctx.stack_size) });

    try lowerValue(b, &ctx, lambda.body.*);

    try b.op(.{ .ret = @intCast(lambda.params.len) });

    return label;
}

pub fn codegen(ally: Allocator, name: Name, expr: TExpr) Error!vm.Unit {
    var b = vm.Builder.init(ally);
    defer b.deinit();

    switch (expr.data) {
        .lambda => |lambda| {
            // function
            try b.define(mini.namestring(name), .exported, .code);
            _ = try lowerLambda(&b, lambda);
        },
        else => {
            // constant
            try b.define(mini.namestring(name), .exported, .data);

            const bytes = try b.ally.alloc(u8, mini.types.sizeOf(expr.type));
            defer b.ally.free(bytes);

            bytesFromExpr(bytes, expr);

            _ = try b.data(bytes);
        },
    }

    return try b.build(ally);
}
