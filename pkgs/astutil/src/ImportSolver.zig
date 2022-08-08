const std = @import("std");
const FixedPath = @import("./FixedPath.zig");

pub const ImportType = union(enum) {
    pkg: []const u8,
    file: []const u8,
};

const Self = @This();

pkg_path_map: std.StringHashMap(FixedPath),

pub fn init(allocator: std.mem.Allocator) Self {
    return Self{
        .pkg_path_map = std.StringHashMap(FixedPath).init(allocator),
    };
}

pub fn deinit(self: Self) void {
    self.pkg_path_map.deinit();
}

pub fn solve(self: Self, base_path: FixedPath, import: ImportType) ?FixedPath {
    return switch (import) {
        .pkg => |pkg_name| if (self.pkg_path_map.get(pkg_name)) |found|
            found.*
        else
            null,
        .file => |relative_path| base_path.child(relative_path),
    };
}
