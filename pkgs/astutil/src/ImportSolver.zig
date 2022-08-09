const std = @import("std");
const FixedPath = @import("./FixedPath.zig");
const logger = std.log.scoped(.ImportSolver);

pub fn unquote(text: []const u8) []const u8 {
    return if (text.len > 2 and text[0] == '"' and text[text.len - 1] == '"')
        text[1 .. text.len - 1]
    else
        text;
}

pub const ImportType = union(enum) {
    pkg: []const u8,
    file: []const u8,

    pub fn fromText(text: []const u8) @This() {
        return if (std.mem.endsWith(u8, text, ".zig")) .{ .file = unquote(text) } else .{ .pkg = unquote(text) };
    }
};

const Self = @This();

allocator: std.mem.Allocator,
pkg_path_map: std.StringHashMap(FixedPath),

pub fn init(allocator: std.mem.Allocator) Self {
    return Self{
        .allocator = allocator,
        .pkg_path_map = std.StringHashMap(FixedPath).init(allocator),
    };
}

pub fn deinit(self: *Self) void {
    var it = self.pkg_path_map.iterator();
    while (it.next()) |entry| {
        self.allocator.free(entry.key_ptr.*);
    }
    self.pkg_path_map.deinit();
}

pub fn push(self: *Self, pkg: []const u8, path: FixedPath) !void {
    const copy = try self.allocator.dupe(u8, pkg);
    try self.pkg_path_map.put(copy, path);
}

pub fn solve(self: Self, base_path: FixedPath, import: ImportType) ?FixedPath {
    switch (import) {
        .pkg => |pkg_name| {
            const text = unquote(pkg_name);
            if (self.pkg_path_map.get(text)) |found| {
                return found;
            } else {
                logger.warn("pkg {s} not found", .{text});
                return null;
            }
        },
        .file => |relative_path| {
            const text = unquote(relative_path);
            return base_path.child(text);
        },
    }
}
