const ws = @import("workspace");
const DocumentPosition = ws.DocumentPosition;

// pub const Encoding = enum {
//     utf8,
//     utf16,
// };

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
