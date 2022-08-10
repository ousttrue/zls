const std = @import("std");

const Arg = struct {
    name: []const u8,
    document: []const u8,
};

const Self = @This();

name: []const u8,
document: []const u8,
args: std.ArrayList(Arg),

pub fn init(allocator: std.mem.Allocator, name: []const u8, document: []const u8) Self {
    return Self{
        .name = name,
        .document = document,
        .args = std.ArrayList(Arg).init(allocator),
    };
}

pub fn deinit(self: Self) void {
    self.args.deinit();
}
