//! command line argument parser.

const std = @import("std");
const Allocator = std.mem.Allocator;

pub fn Interface(comptime ArgId: type) type {
    return struct {
        const Self = @This();

        pub const ArgSpec = struct {
            pub const Required = enum { required, optional };

            pub const TakesInput = union(enum) {
                /// string will be used as metavar
                input: []const u8,
                no_input,
            };

            pub const Flag = struct {
                short: ?u8,
                long: ?[]const u8,
                required: Required = .required,
                takes_input: TakesInput = .no_input,
            };

            pub const PositionalCount = enum {
                one,
                many,
            };

            pub const Positional = struct {
                metavar: []const u8,
                count: PositionalCount = .one,
            };

            pub const Data = union(enum) {
                flag: Flag,
                positional: Positional,
            };

            id: ArgId,
            desc: []const u8,
            data: ArgSpec.Data,

            pub fn format(
                spec: ArgSpec,
                comptime fmt: []const u8,
                _: std.fmt.FormatOptions,
                writer: anytype,
            ) @TypeOf(writer).Error!void {
                if (comptime std.mem.eql(u8, fmt, "simple")) {
                    switch (spec.data) {
                        .positional => {},
                        .flag => |flag| {
                            if (flag.short) |short| {
                                try writer.print("-{c}", .{short});
                            } else if (flag.long) |long| {
                                try writer.print("--{s}", .{long});
                            }
                        },
                    }
                } else {
                    comptime std.debug.assert(fmt.len == 0);
                    switch (spec.data) {
                        .positional => {},
                        .flag => |flag| {
                            if (flag.short) |short| {
                                try writer.print("-{c}", .{short});
                                if (flag.long != null) {
                                    try writer.print(",", .{});
                                }
                            }
                            if (flag.long) |long| {
                                try writer.print("--{s}", .{long});
                            }
                        },
                    }
                }

                switch (spec.data) {
                    .positional => |pos| switch (pos.count) {
                        .one => try writer.print("<{s}>", .{pos.metavar}),
                        .many => try writer.print("[{s}...]", .{pos.metavar}),
                    },
                    .flag => |flag| switch (flag.takes_input) {
                        .no_input => {},
                        .input => |metavar| {
                            try writer.print(" <{s}>", .{metavar});
                        },
                    },
                }
            }
        };

        pub const Args = struct {
            id: ArgId,
            args: []const ArgSpec,
        };

        pub const Data = union(enum) {
            subcommands: []const Self,
            args: Args,
        };

        name: []const u8,
        desc: []const u8,
        data: Data,

        /// specify an argument for a command.
        /// argument will be returned with its id.
        /// provide short or long option, or neither for positional args.
        pub fn arg(id: ArgId, desc: []const u8, data: ArgSpec.Data) ArgSpec {
            return ArgSpec{
                .id = id,
                .desc = desc,
                .data = data,
            };
        }

        /// construct a command.
        pub fn command(name: []const u8, desc: []const u8, data: Data) Self {
            return Self{
                .name = name,
                .desc = desc,
                .data = data,
            };
        }

        const Context = struct {
            cmd: Self,
            input: []const []const u8,
            path_len: usize,

            fn path(ctx: Context) []const []const u8 {
                return ctx.input[0..ctx.path_len];
            }

            fn args(ctx: Context) []const []const u8 {
                return ctx.input[ctx.path_len..];
            }
        };

        fn shiftContext(ctx: Context) ?Context {
            if (ctx.cmd.data != .subcommands or ctx.path_len == ctx.input.len) {
                return null;
            }

            for (ctx.cmd.data.subcommands) |subcommand| {
                if (std.mem.eql(u8, subcommand.name, ctx.input[ctx.path_len])) {
                    return Context{
                        .cmd = subcommand,
                        .input = ctx.input,
                        .path_len = ctx.path_len + 1,
                    };
                }
            }

            return null;
        }

        /// splits out the path and args components of the input, and follows
        /// subcommand tree to identify the relevant command for parsing or
        /// usage
        fn genContext(self: Self, input: []const []const u8) Context {
            std.debug.assert(input.len > 0);

            var ctx = Context{
                .cmd = self,
                .input = input,
                .path_len = 1,
            };

            if (self.data == .subcommands) {
                while (shiftContext(ctx)) |shifted| {
                    ctx = shifted;
                }
            }

            return ctx;
        }

        /// writes usage to a writer
        pub fn usage(
            self: Self,
            raw_input: []const []const u8,
            writer: anytype,
        ) @TypeOf(writer).Error!void {
            const input = if (raw_input.len == 0) &.{self.name} else raw_input;
            const ctx = self.genContext(input);
            const path = ctx.path();

            // usage example and desc
            try writer.print("usage:", .{});
            for (path) |seg| {
                try writer.print(" {s}", .{seg});
            }

            switch (ctx.cmd.data) {
                .subcommands => try writer.print(" <subcommand>", .{}),
                .args => |args| {
                    for (args.args) |spec| {
                        if (spec.data == .flag and
                            spec.data.flag.required == .optional)
                        {
                            continue;
                        }

                        try writer.print(" {simple}", .{spec});
                    }
                },
            }

            // TODO print desc with wrap
            try writer.print("\n\n{s}\n\n", .{ctx.cmd.desc});

            switch (ctx.cmd.data) {
                .subcommands => |cmds| {
                    try writer.print("commands:\n", .{});
                    for (cmds) |cmd| {
                        try writer.print("  {s} - {s}\n", .{ cmd.name, cmd.desc });
                    }
                },
                .args => |args| {
                    try writer.print("arguments:\n", .{});
                    for (args.args) |spec| {
                        try writer.print("  {} - {s}\n", .{ spec, spec.desc });
                    }
                },
            }

            try writer.print("\n", .{});
        }

        pub const Arg = struct {
            id: ArgId,
            data: ?[]const u8,
        };

        pub const ParseResult = struct {
            id: ArgId,
            args: []const Arg,

            pub fn deinit(res: ParseResult, ally: Allocator) void {
                ally.free(res.args);
            }
        };

        /// returns owned slice of args, or null on invalid args.
        ///
        /// this allows for nice patterns like this:
        /// ```zig
        /// const args = cmd.parse(ally, input) orelse {
        ///     try cmd.usage(stderr);
        ///     std.process.exit(1);
        /// };
        /// ```
        pub fn parse(
            self: Self,
            ally: Allocator,
            raw_input: []const []const u8,
        ) Allocator.Error!?ParseResult {
            const input = if (raw_input.len == 0) &.{self.name} else raw_input;
            const ctx = self.genContext(input);

            if (ctx.cmd.data == .subcommands) return null;

            // build metadata for parsing
            var shorts = std.AutoHashMapUnmanaged(u8, ArgSpec){};
            defer shorts.deinit(ally);
            var longs = std.StringHashMapUnmanaged(ArgSpec){};
            defer longs.deinit(ally);
            var positionals = std.ArrayListUnmanaged(ArgSpec){};
            defer positionals.deinit(ally);

            const subcom = ctx.cmd.data.args;
            for (subcom.args) |spec| {
                switch (spec.data) {
                    .flag => |flag| {
                        if (flag.short) |short| {
                            try shorts.put(ally, short, spec);
                        }
                        if (flag.long) |long| {
                            try longs.put(ally, long, spec);
                        }
                    },
                    .positional => {
                        try positionals.append(ally, spec);
                    },
                }
            }

            // parse args
            var args = std.ArrayListUnmanaged(Arg){};
            defer args.deinit(ally);

            var positional_index: usize = 0;

            const raw_args = ctx.args();
            var index: usize = 0;
            while (index < raw_args.len) : (index += 1) {
                const raw_arg = raw_args[index];
                if (std.mem.startsWith(u8, raw_arg, "--")) {
                    // long option
                    const eq_index = std.mem.indexOf(u8, raw_arg, "=");
                    const long_opt = raw_arg[2 .. eq_index orelse raw_arg.len];

                    const spec = longs.get(long_opt) orelse {
                        return null;
                    };

                    const arg_data: ?[]const u8 = switch (spec.data.flag.takes_input) {
                        .no_input => no_input: {
                            if (eq_index != null) return null;
                            break :no_input null;
                        },
                        .input => input: {
                            // passed as `--long=<data>`
                            if (eq_index) |eq| {
                                break :input raw_arg[eq + 1 ..];
                            }

                            // passed as `--long <data>`
                            if (index == raw_args.len - 1) return null;

                            index += 1;
                            break :input raw_args[index];
                        },
                    };

                    try args.append(ally, Arg{
                        .id = spec.id,
                        .data = arg_data,
                    });
                } else if (std.mem.startsWith(u8, raw_arg, "-")) {
                    // short option(s)
                    const short_opts = raw_arg[1..];
                    for (short_opts, 0..) |ch, i| {
                        if (ch == '=') return null;

                        const spec = shorts.get(ch) orelse {
                            return null;
                        };
                        const arg_data: ?[]const u8 = switch (spec.data.flag.takes_input) {
                            .no_input => null,
                            .input => input: {
                                if (i != short_opts.len - 1 or index == raw_args.len - 1) {
                                    return null;
                                }

                                index += 1;
                                break :input raw_args[index];
                            },
                        };

                        try args.append(ally, Arg{
                            .id = spec.id,
                            .data = arg_data,
                        });
                    }
                } else {
                    // positional
                    if (positional_index == positionals.items.len) return null;

                    const spec = positionals.items[positional_index];
                    if (spec.data.positional.count == .one) {
                        positional_index += 1;
                    }

                    try args.append(ally, Arg{
                        .id = spec.id,
                        .data = raw_arg,
                    });
                }
            }

            // ensure required args are supplied
            var reqs = std.AutoHashMapUnmanaged(ArgId, bool){};
            defer reqs.deinit(ally);

            for (subcom.args) |spec| {
                if (spec.data == .positional or spec.data.flag.required == .required) {
                    try reqs.put(ally, spec.id, false);
                }
            }

            for (args.items) |found| {
                const slot = reqs.getPtr(found.id) orelse continue;
                slot.* = true;
            }

            var entries = reqs.iterator();
            while (entries.next()) |entry| {
                if (entry.value_ptr.* == false) {
                    // missing requirement
                    return null;
                }
            }

            return ParseResult{
                .id = subcom.id,
                .args = try args.toOwnedSlice(ally),
            };
        }
    };
}
