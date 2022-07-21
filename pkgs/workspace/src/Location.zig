const std = @import("std");
const Self = @This();

allocator: std.mem.Allocator,
uri: []const u8,
row: u32 = 0,
col: u32 = 0,

pub fn init(
    allocator: std.mem.Allocator,
    uri: []const u8,
    location: struct { row: u32 = 0, col: u32 = 0 },
) Self {
    return Self{
        .allocator = allocator,
        .uri = allocator.dupe(u8, uri) catch unreachable,
        .row = location.row,
        .col = location.col,
    };
}

pub fn deinit(self: *Self) void {
    self.allocator.free(self.uri);
}
