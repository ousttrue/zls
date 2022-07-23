const std = @import("std");

fn find(src: []const u8) ?usize {
    for (src) |c, i| {
        if (c == '\n') {
            return i;
        }
    }
    return null;
}

const Self = @This();

row: usize,
col: usize,
line: []const u8,
absolute_index: usize,
all: []const u8,

pub fn getLine(text: []const u8, dst: usize) !Self {
    // doc: lsp.TextDocument
    // var split_iterator = std.mem.split(u8, doc.text, "\n");
    // const dst = position.line - 1;
    // var line: []const u8 = "";
    var i: usize = 0;
    var start: usize = 0;
    var end: usize = text.len;
    while (true) : ({
        i += 1;
        start = end;
    }) {
        if (find(text[start..])) |found| {
            end = start + found;
        } else {
            end = text.len;
            break;
        }

        if (i >= dst) {
            break;
        }
        end += 1;
    }
    if (i != dst) {
        return error.LineNotFound;
    }
    return Self{
        .row = i,
        .col = 0,
        .line = text[start..end],
        .absolute_index = start,
        .all = text,
    };
}

pub fn fromUtf8Pos(text: []const u8, pos: struct { line: u32, x: u32 = 0 }) !Self {
    const line = try getLine(text, pos.line);
    return line.advance(pos.x);
}

fn getUtf8Length(utf8: []const u8, utf16Characters: i64) usize {
    var utf8_idx: usize = 0;
    var utf16_idx: usize = 0;
    while (utf16_idx < utf16Characters) {
        if (utf8_idx > utf8.len) {
            unreachable;
            // return error.InvalidParams;
        }

        const n = std.unicode.utf8ByteSequenceLength(utf8[utf8_idx]) catch unreachable;
        const next_utf8_idx = utf8_idx + n;
        const codepoint = std.unicode.utf8Decode(utf8[utf8_idx..next_utf8_idx]) catch unreachable;
        if (codepoint < 0x10000) {
            utf16_idx += 1;
        } else {
            utf16_idx += 2;
        }
        utf8_idx = next_utf8_idx;
    }
    return utf8_idx;
}

pub fn fromUtf16Pos(text: []const u8, pos: struct { line: u32, x: u32 = 0 }) !Self {
    const line = try getLine(text, pos.line);
    const utf8 = text[line.absolute_index..];
    const utf8_idx = getUtf8Length(utf8, pos.x);
    return line.advance(utf8_idx);
}

pub fn advance(self: Self, delta: usize) Self {
    return Self{
        .row = self.row,
        .col = self.col + delta,
        .line = self.line,
        .absolute_index = self.absolute_index + delta,
        .all = self.all,
    };
}

pub fn back(self: Self, delta: usize) Self {
    return Self{
        .row = self.row,
        .col = self.col - delta,
        .line = self.line,
        .absolute_index = self.absolute_index - delta,
        .all = self.all,
    };
}

test "getLine" {
    const src =
        \\a
        \\b
        \\c
    ;
    const result = Self.getLine(src, 0).?;
    try std.testing.expectEqual(@as(usize, 0), result.absolute_index);
    try std.testing.expectEqual(@as(usize, 1), result.line.len);
    try std.testing.expectEqualStrings("a", result.line);
    try std.testing.expectEqualStrings("c", Self.getLine(src, 2).?.line);
    try std.testing.expect(Self.getLine(src, 3) == null);
}
