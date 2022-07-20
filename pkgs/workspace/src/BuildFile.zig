const std = @import("std");
const Pkg = struct {
    name: []const u8,
    uri: []const u8,
};

const Self = @This();

refs: usize,
uri: []const u8,
packages: std.ArrayListUnmanaged(Pkg),

builtin_uri: ?[]const u8 = null,

pub fn destroy(self: *Self, allocator: std.mem.Allocator) void {
    if (self.builtin_uri) |builtin_uri| allocator.free(builtin_uri);
    allocator.destroy(self);
}
