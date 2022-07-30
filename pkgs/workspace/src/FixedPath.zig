const std = @import("std");
const logger = std.log.scoped(.FixedPath);
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

pub fn child(self: Self, name: []const u8) Self {
    var copy = fromFullpath(self.slice());
    copy._buffer[copy.len] = '/';
    std.mem.copy(u8, copy._buffer[(copy.len + 1)..], name);
    copy.len += 1 + name.len;
    return copy;
}

pub fn isAbsoluteExists(self: Self) bool {
    if (!std.fs.path.isAbsolute(self.slice())) {
        return false;
    }
    std.fs.cwd().access(self.slice(), .{}) catch {
        return false;
    };
    return true;
}

pub fn exec(self: Self, allocator: std.mem.Allocator, args: anytype) !std.ChildProcess.ExecResult {
    var buffer = std.ArrayList(u8).init(allocator);
    defer buffer.deinit();
    var w = buffer.writer();
    var _args: [args.len + 1][]const u8 = undefined;
    _args[0] = self.slice();
    try w.print("{s}", .{self.slice()});

    inline for (args) |arg, i| {
        try w.print(" {s}", .{arg});
        _args[i + 1] = arg;
    }

    logger.debug("{s}", .{buffer.items});
    return std.ChildProcess.exec(.{
        .allocator = allocator,
        .argv = &_args,
        .max_output_bytes = 1024 * 1024 * 50,
    });
}
