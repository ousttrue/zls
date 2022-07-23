const std = @import("std");
const lsp = @import("lsp");
const ws = @import("workspace");
const DocumentPosition = ws.DocumentPosition;

pub const Encoding = ws.offsets.Encoding;

pub fn utf8BytePositionFromUtf8Pos(
    text: []const u8,
    pos: struct { line: u32, x: u32 = 0 },
) !u32 {
    const doc_position = try DocumentPosition.fromUtf8Pos(text, .{ .line = pos.line, .x = pos.x });
    return @intCast(u32, doc_position.absolute_index);
}

pub fn utf8BytePositionFromUtf16Pos(
    text: []const u8,
    pos: struct { line: u32, x: u32 = 0 },
) !u32 {
    const doc_position = try DocumentPosition.fromUtf16Pos(text, .{ .line = pos.line, .x = pos.x });
    return @intCast(u32, doc_position.absolute_index);
}

pub fn utf8PositionToUtf16(text: []const u8, src: lsp.Position) !lsp.Position {
    var n: u32 = 0;
    var utf8: u32 = 0;
    const line_pos = try DocumentPosition.getLine(text, @intCast(usize, src.line));
    while (utf8 < src.character) : (n += 1) {
        utf8 += @intCast(u32, try std.unicode.utf8ByteSequenceLength(line_pos.line[utf8]));
    }
    return lsp.Position{ .line = src.line, .character = n };
}
