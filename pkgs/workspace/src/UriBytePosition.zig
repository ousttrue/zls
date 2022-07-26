const std = @import("std");
const Self = @This();

uri: []const u8,
loc: std.zig.Token.Loc,
