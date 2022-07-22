const ws = @import("workspace");
const offsets = ws.offsets;

// pub const Encoding = enum {
//     utf8,
//     utf16,
// };

pub const Encoding = offsets.Encoding;

pub fn getUtf8BytePosition(
    text: []const u8,
    pos: struct { line: u32, x: u32 = 0 },
    encoding: Encoding,
) !usize {
    const doc_position = try offsets.documentPosition(text, .{ .line = pos.line, .x = pos.x }, encoding);
    return doc_position.absolute_index;
}
