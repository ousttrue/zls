const std = @import("std");
const logger = std.log.scoped(.FixedPath);
const Self = @This();

_buffer: [std.fs.MAX_PATH_BYTES]u8 = undefined,
len: usize = 0,

pub fn fromFullpath(fullpath: []const u8) Self {
    var self = Self{};
    std.mem.copy(u8, &self._buffer, fullpath);
    var i: usize = 0;
    while (i < fullpath.len) {
        var c = fullpath[i];
        var len = std.unicode.utf8ByteSequenceLength(c) catch unreachable;
        if (c == '\\') {
            self._buffer[i] = '/';
        }
        i += len;
    }
    self.len = fullpath.len;
    return self;
}

pub fn fromCwd() !Self {
    var self = Self{};
    self.len = (try std.os.getcwd(&self._buffer)).len;
    return self;
}

pub fn fromSelfExe() !Self {
    var exe_dir_bytes: [std.fs.MAX_PATH_BYTES]u8 = undefined;
    const exe_dir_path = try std.fs.selfExeDirPath(&exe_dir_bytes);
    return fromFullpath(exe_dir_path);
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

pub fn parent(self: Self) ?Self {
    return if (std.fs.path.dirname(self.slice())) |dirname|
        fromFullpath(dirname)
    else
        null;
}

// try std.fs.path.resolve(allocator, &[_][]const u8{ exe_dir_path,  name});
pub fn child(self: Self, name: []const u8) Self {
    var copy = fromFullpath(self.slice());
    copy._buffer[copy.len] = '/';
    copy.len += 1;

    if (std.mem.startsWith(u8, name, "./")) {
        std.mem.copy(u8, copy._buffer[copy.len..], name[2..]);
        copy.len += (name.len - 2);
    } else if (name[0] == '/') {
        std.mem.copy(u8, copy._buffer[copy.len..], name[1..]);
        copy.len += (name.len - 1);
    } else {
        std.mem.copy(u8, copy._buffer[copy.len..], name);
        copy.len += name.len;
    }

    logger.debug("child: {s} + {s} = {s}", .{ self.slice(), name, copy.slice() });

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

pub fn exec(self: Self, allocator: std.mem.Allocator, args: []const []const u8) !std.ChildProcess.ExecResult {
    var buffer = std.ArrayList(u8).init(allocator);
    defer buffer.deinit();
    var _args = std.ArrayList([]const u8).init(allocator);
    defer _args.deinit();

    var w = buffer.writer();
    try w.print("{s}", .{self.slice()});
    try _args.append(self.slice());
    for (args) |arg| {
        try w.print(" {s}", .{arg});
        try _args.append(arg);
    }

    logger.debug("{s}", .{buffer.items});
    return std.ChildProcess.exec(.{
        .allocator = allocator,
        .argv = _args.items,
        .max_output_bytes = 1024 * 1024 * 50,
    });
}

pub fn readContents(self: Self, allocator: std.mem.Allocator) ![]const u8 {
    var file = try std.fs.cwd().openFile(self.slice(), .{});
    defer file.close();

    return try file.readToEndAllocOptions(
        allocator,
        std.math.maxInt(usize),
        null,
        @alignOf(u8),
        0,
    );
}
