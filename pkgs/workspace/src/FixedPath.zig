const std = @import("std");
const Self = @This();
const MAXPATH = 260;

_buffer: [MAXPATH]u8 = undefined,
len: usize = 0,

pub fn fromFullpath(fullpath: []const u8) Self {
    var self = Self{};
    std.mem.copy(u8, &self._buffer, fullpath);
    self.len = fullpath.len;
    return self;
}

pub fn fromCwd() !Self {
    var self = Self{};
    self.len = (try std.os.getcwd(&self._buffer)).len;
    return self;
}

pub fn fromUri(uri: []const u8) !Self {
    var self = Self{};
    try self.parseUri(uri);
    return self;
}

// Original code: https://github.com/andersfr/zig-lsp/blob/master/uri.zig
fn parseHex(c: u8) !u8 {
    return switch (c) {
        '0'...'9' => c - '0',
        'a'...'f' => c - 'a' + 10,
        'A'...'F' => c - 'A' + 10,
        else => return error.UriBadHexChar,
    };
}

pub fn parseUri(self: *Self, str: []const u8) !void {
    if (str.len < 7 or !std.mem.eql(u8, "file://", str[0..7])) return error.UriBadScheme;

    const path = if (std.fs.path.sep == '\\') str[8..] else str[7..];
    var i: usize = 0;
    var j: usize = 0;
    while (j < path.len) : (i += 1) {
        if (path[j] == '%') {
            if (j + 2 >= path.len) return error.UriBadEscape;
            const upper = try parseHex(path[j + 1]);
            const lower = try parseHex(path[j + 2]);
            self._buffer[i] = (upper << 4) + lower;
            j += 3;
        } else {
            self._buffer[i] = if (path[j] == '/') std.fs.path.sep else path[j];
            j += 1;
        }
    }

    // Remove trailing separator
    if (i > 0 and self._buffer[i - 1] == std.fs.path.sep) {
        i -= 1;
    }
    self.len = i;
}

pub fn slice(self: Self) []const u8 {
    return self._buffer[0..self.len];
}
