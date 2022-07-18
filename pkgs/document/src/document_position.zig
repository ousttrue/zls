const std = @import("std");

pub fn find(src: []const u8) ?usize {
    for (src) |c, i| {
        if (c == '\n') {
            return i;
        }
    }
    return null;
}

pub const DocumentPosition = struct {
    const Self = @This();

    row: usize,
    col: usize,
    line: []const u8,
    absolute_index: usize,
    all: []const u8,

    pub fn getLine(text: []const u8, dst: usize) ?Self {
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
            return null;
        }
        return Self{
            .row = i,
            .col = 0,
            .line = text[start..end],
            .absolute_index = start,
            .all = text,
        };
    }

    pub fn advance(self: Self, delta: usize) DocumentPosition {
        return DocumentPosition{
            .row = self.row,
            .col = self.col + delta,
            .line = self.line,
            .absolute_index = self.absolute_index + delta,
            .all = self.all,
        };
    }

    pub fn back(self: Self, delta: usize) DocumentPosition {
        return DocumentPosition{
            .row = self.row,
            .col = self.col - delta,
            .line = self.line,
            .absolute_index = self.absolute_index - delta,
            .all = self.all,
        };
    }
};

test "getLine" {
    const src =
        \\a
        \\b
        \\c
    ;
    const result = DocumentPosition.getLine(src, 0).?;
    try std.testing.expectEqual(@as(usize, 0), result.absolute_index);
    try std.testing.expectEqual(@as(usize, 1), result.line.len);
    try std.testing.expectEqualStrings("a", result.line);
    try std.testing.expectEqualStrings("c", DocumentPosition.getLine(src, 2).?.line);
    try std.testing.expect(DocumentPosition.getLine(src, 3) == null);
}
