const std = @import("std");
const Allocator = std.mem.Allocator;
const chibi = @import("chibi.zig");

fn cStrSlice(c_str: [*:0]const u8) [:0]const u8 {
    const len = std.mem.indexOfSentinel(u8, 0, c_str);
    return c_str[0..len:0];
}

/// chibi objects tend to use a next pointer for lists
fn countChibi(comptime T: type, list: ?*T) usize {
    var count: usize = 0;
    var trav = list;
    while (trav) |item| : (trav = item.next) {
        count += 1;
    }

    return count;
}

fn ChibiIterator(comptime T: type) type {
    return struct {
        trav: ?*T,

        pub fn next(self: *@This()) ?*T {
            if (self.trav) |item| {
                self.trav = item.next;
                return item;
            }

            return null;
        }
    };
}

fn iterateChibi(comptime T: type, list: ?*T) ChibiIterator(T) {
    return .{ .trav = list };
}

const Source = struct {
    name: [:0]const u8,
    contents: [:0]const u8,
};

/// a more easy to work with chibi type
///
/// chibi types are each individually heap-allocated trees, so this mirrors
/// that design
const Type = struct {
    const Self = @This();

    const Int = struct {
        signedness: std.builtin.Signedness,
    };

    const Ptr = struct {
        child: *Self,
    };

    const Func = struct {
        params: []const *Self,
        returns: *Self,
    };

    const Data = union(chibi.TypeKind) {
        void,
        bool,
        char: Int,
        short: Int,
        int: Int,
        long: Int,
        float,
        double,
        ldouble,
        @"enum",
        ptr: Ptr,
        func: Func,
        array,
        vla,
        @"struct",
        @"union",
    };

    name: ?[]const u8,
    data: Data,

    fn fromChibi(ally: Allocator, ty: *chibi.Type) Allocator.Error!Self {
        const name: ?[]const u8 = if (ty.name) |name| name: {
            break :name name.loc[0..@intCast(name.len)];
        } else null;

        const data = switch (ty.kind) {
            inline .void,
            .bool,
            => |kind| @unionInit(Data, @tagName(kind), {}),

            // integers
            inline .char,
            .short,
            .int,
            .long,
            => |kind| @unionInit(Data, @tagName(kind), Int{
                .signedness = if (ty.is_unsigned) .unsigned else .signed,
            }),

            // floats
            inline .float,
            .double,
            .ldouble,
            => |kind| @unionInit(Data, @tagName(kind), {}),

            .ptr => Data{ .ptr = Ptr{
                .child = try fromChibiAlloc(ally, ty.base.?),
            } },

            .func => func: {
                const nparams = countChibi(chibi.Type, ty.params);
                const params = try ally.alloc(*Self, nparams);

                var i: usize = 0;
                var iter = iterateChibi(chibi.Type, ty.params);
                while (iter.next()) |param_ty| : (i += 1) {
                    params[i] = try fromChibiAlloc(ally, param_ty);
                }

                const returns = try fromChibiAlloc(ally, ty.return_ty.?);

                break :func Data{ .func = .{
                    .params = params,
                    .returns = returns,
                }};
            },

            inline else => |type_kind| @unionInit(
                Data,
                @tagName(type_kind),
                {},
            ),
        };

        return Self{
            .name = name,
            .data = data,
        };
    }

    /// just heap allocates the result of `fromChibi`
    fn fromChibiAlloc(ally: Allocator, ty: *chibi.Type) Allocator.Error!*Self {
        const box = try ally.create(Self);
        box.* = try fromChibi(ally, ty);
        return box;
    }

    /// formatting these with zig-style syntax for a lot of thigns for my own
    /// readability
    pub fn format(
        self: Self,
        comptime _: []const u8,
        _: std.fmt.FormatOptions,
        writer: anytype,
    ) @TypeOf(writer).Error!void {
        switch (self.data) {
            .void, .bool, .float, .double => {
                try writer.print("{s}", .{@tagName(self.data)});
            },
            .char, .short, .int, .long => |int| {
                if (int.signedness == .unsigned) {
                    try writer.writeAll("unsigned ");
                }

                try writer.writeAll(@tagName(self.data));
            },
            .ldouble => {
                try writer.writeAll("long double");
            },

            .ptr => |ptr| {
                try writer.print("*{}", .{ptr.child});
            },
            .func => |func| {
                try writer.writeAll("fn(");
                for (func.params, 0..) |param, i| {
                    if (i > 0) try writer.writeAll(", ");
                    try writer.print("{}", .{param});
                }
                try writer.print(") {}", .{func.returns});
            },

            else => @panic("TODO"),
        }
    }
};

const Object = struct {
    const Self = @This();

    name: []const u8,
    ty: Type,

    fn fromChibi(ally: Allocator, obj: *chibi.Obj) Allocator.Error!Self {
        const name = cStrSlice(obj.name);
        const ty = try Type.fromChibi(ally, obj.ty);

        return Self{
            .name = name,
            .ty = ty,
        };
    }

    pub fn format(
        self: Self,
        comptime _: []const u8,
        _: std.fmt.FormatOptions,
        writer: anytype,
    ) @TypeOf(writer).Error!void {
        try writer.print("object(name=\"{s}\", ty={})", .{self.name, self.ty});
    }
};

const InterpretError = error{};

fn interpret(
    backing_ally: Allocator,
    sources: []const Source,
) (Allocator.Error || InterpretError)!void {
    var arena = std.heap.ArenaAllocator.init(backing_ally);
    defer arena.deinit();
    const ally = arena.allocator();

    var files = try std.ArrayListUnmanaged(*chibi.File).initCapacity(ally, sources.len);
    defer {
        for (files.items) |file| std.c.free(file);
    }

    for (sources, 0..) |source, i| {
        // load source
        const file = chibi.new_file(
            source.name.ptr,
            @intCast(i),
            source.contents.ptr,
        );
        files.append(ally, file) catch unreachable;

        // tokenize
        const raw_tokens = chibi.tokenize(file) orelse {
            // no tokens, TODO check for errors
            std.debug.print("failed to tokenize {s}\n", .{source.name});
            continue;
        };
        const tokens = chibi.preprocess(raw_tokens);

        // parse
        const chibi_program = chibi.parse(tokens) orelse {
            std.debug.print("failed to parse {s}\n", .{source.name});
            continue;
        };

        var objects = std.ArrayListUnmanaged(Object){};
        var trav: ?*chibi.Obj = chibi_program;
        while (trav) |chibi_obj| : (trav = chibi_obj.next) {
            if (!chibi_obj.is_live) continue;

            const object = try Object.fromChibi(ally, chibi_obj);
            try objects.append(ally, object);

            std.debug.print("[object]\n{}\n\n", .{object});
        }

        // codegen
        // TODO
    }
}

pub fn main() !void {
    // init stuff
    var gpa = std.heap.GeneralPurposeAllocator(.{}){
        // may as well use the same allocator as chibi
        .backing_allocator = std.heap.c_allocator,
    };
    defer _ = gpa.deinit();
    const ally = gpa.allocator();

    chibi.init_macros();

    // interpret sources
    var sources = [_]Source{
        .{
            .name = "test_file",
            .contents =
            \\int add(int a, int b) {
            \\  return a + b;
            \\}
            \\
            \\int main(int argc, char **argv) {
            \\  return 0;
            \\}
            \\
            ,
        },
    };

    try interpret(ally, &sources);
}
