//! fluent source registry

const std = @import("std");
const Allocator = std.mem.Allocator;
const builtin = @import("builtin");

pub const Source = enum(u16) {
    _,

    pub fn get(src: Source) File {
        return map.get(src).?;
    }
};

pub const File = struct {
    filename: []const u8,
    text: []const u8,
};

const SourceMap = std.AutoArrayHashMapUnmanaged(Source, File);

pub const Loc = packed struct(u64) {
    const Self = @This();

    source: Source,
    line_index: u32,
    char_index: u16,

    pub fn lineno(self: Self) usize {
        return self.line_index + 1;
    }

    pub fn charno(self: Self) usize {
        return self.char_index + 1;
    }

    pub fn format(
        self: Self,
        comptime _: []const u8,
        _: std.fmt.FormatOptions,
        writer: anytype,
    ) @TypeOf(writer).Error!void {
        try writer.print("{s}:{d}:{d}", .{
            self.source.get().filename,
            self.lineno(),
            self.charno(),
        });
    }
};

var gpa = std.heap.GeneralPurposeAllocator(.{}){};
const ally = gpa.allocator();

var map = SourceMap{};
var filenames = std.StringHashMapUnmanaged(Source){};

pub fn deinit() void {
    for (map.values()) |f| {
        ally.free(f.filename);
        ally.free(f.text);
    }

    filenames.deinit(ally);
    map.deinit(ally);

    // reset for tests
    if (builtin.is_test) {
        filenames = .{};
        map = .{};
        gpa = .{};
    }
}

pub fn add(filename: []const u8, text: []const u8) Allocator.Error!Source {
    const src: Source = @enumFromInt(map.count());
    try map.put(ally, src, .{
        .filename = try ally.dupe(u8, filename),
        .text = try ally.dupe(u8, text),
    });
    return src;
}

fn RetError(comptime T: type) type {
    comptime {
        const Ret = @typeInfo(T).Fn.return_type.?;
        return @typeInfo(Ret).ErrorUnion.error_set;
    }
}

pub const AddPathError =
    RetError(@TypeOf(std.fs.Dir.readFileAlloc)) ||
    RetError(@TypeOf(std.fs.path.relative));

/// load a file from a filepath and store it
pub fn addPath(filepath: []const u8) AddPathError!Source {
    const filename = try std.fs.path.relative(ally, ".", filepath);
    errdefer ally.free(filename);

    var res = try filenames.getOrPut(ally, filename);
    if (res.found_existing) {
        ally.free(filename);
    } else {
        const text = try std.fs.cwd().readFileAlloc(ally, filepath, std.math.maxInt(u32));
        errdefer ally.free(text);

        const src: Source = @enumFromInt(map.count());
        try map.put(ally, src, .{
            .filename = filename,
            .text = text,
        });
        res.value_ptr.* = src;
    }

    return res.value_ptr.*;
}

/// sometimes in tests you need to create an ast node but you don't have a
/// source
pub fn testLoc() Allocator.Error!Loc {
    const Ns = struct {
        var test_source: ?Source = null;
    };

    if (Ns.test_source == null) {
        Ns.test_source = try add(
            "virtual-test-source.fl",
            "this is a virtual test source file",
        );
    }

    return Loc{
        .source = Ns.test_source.?,
        .line_index = 0,
        .char_index = 0,
    };
}
