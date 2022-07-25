const std = @import("std");
const Self = @This();

line_heads: std.ArrayList(u32),
text: []const u8,

pub fn init(allocator: std.mem.Allocator, text: []const u8) !Self {
    var self = Self{
        .line_heads = std.ArrayList(u32).init(allocator),
        .text = text,
    };

    self.line_heads.append(0) catch unreachable;
    var i: u32 = 0;
    while (i < text.len) : (i += @intCast(u32, try std.unicode.utf8ByteSequenceLength(text[i]))) {
        const c = text[i];
        if (c == '\n') {
            self.line_heads.append(@intCast(u32, i + 1)) catch unreachable;
        }
    }
    return self;
}

pub fn deinit(self: Self) void {
    self.line_heads.deinit();
}

pub fn getLineIndexFromBytePosition(self: Self, byte_position: usize) !usize {
    const line_count = self.line_heads.items.len;
    var top: usize = 0;
    var bottom: usize = line_count - 1;
    while (true) {
        var line: usize = (bottom + top) / 2;
        const begin = self.line_heads.items[line];
        const end = if (line + 1 < line_count)
            self.line_heads.items[line + 1] - 1
        else
            self.text.len;
        // std.debug.print("line: [{}, {} => {}]: {} ~ {} <= {}\n", .{ top, bottom, line, begin, end, byte_position });
        if (byte_position >= begin and byte_position <= end) {
            return line;
        }
        if (top == bottom) {
            unreachable;
        }

        if (byte_position < begin) {
            if (bottom != line) {
                bottom = line;
            } else {
                bottom = line - 1;
            }
        } else if (byte_position > end) {
            if (top != line) {
                top = line;
            } else {
                top = line + 1;
            }
        } else {
            unreachable;
        }
    }
    unreachable;
}

const LineX = struct {
    line: u32,
    x: u32,
};

pub fn getPositionFromBytePosition(self: Self, byte_position: usize) !LineX {
    const line = try self.getLineIndexFromBytePosition(byte_position);
    const begin = self.line_heads.items[line];
    var i: u32 = begin;
    var x: u32 = 0;
    while (i < byte_position) {
        const len: u32 = try std.unicode.utf8ByteSequenceLength(self.text[i]);
        i += len;
        x += len;
    }
    return LineX{ .line = @intCast(u32, line), .x = x };
}

pub fn utf8PositionToUtf16(self: Self, src: LineX) !LineX {
    const begin = self.line_heads.items[src.line];
    var i: u32 = begin;
    var x: u32 = 0;
    var n: u32 = 0;
    while (x < src.x) {
        const len: u32 = try std.unicode.utf8ByteSequenceLength(self.text[i]);
        i += len;
        x += len;
        n += 1;
    }
    return LineX{ .line = src.line, .x = n };
}

test "LinePosition" {
    const text =
        \\0
        \\1
        \\2
        \\345
    ;
    const ls = try Self.init(std.testing.allocator, text);
    defer ls.deinit();
    std.debug.print("\n", .{});
    try std.testing.expect((try ls.getLineIndexFromBytePosition(0)) == @as(usize, 0));
    try std.testing.expect((try ls.getLineIndexFromBytePosition(2)) == @as(usize, 1));
    try std.testing.expect((try ls.getLineIndexFromBytePosition(6)) == @as(usize, 3));
    std.debug.print("\n", .{});

    try std.testing.expectEqual((try ls.getPositionFromBytePosition(0)), .{ .line = 0, .x = 0 });
    try std.testing.expectEqual((try ls.getPositionFromBytePosition(7)), .{ .line = 3, .x = 1 });
}
