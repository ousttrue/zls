const std = @import("std");
const Ast = std.zig.Ast;

fn findTokenIndex(tree: *const Ast, byte_position: u32) ?u32 {
    const token_start = tree.tokens.items(.start);
    for (token_start) |start, i| {
        if (start == byte_position) {
            return @intCast(u32, i);
        } else if (start > byte_position) {
            if (i == 0) {
                return null;
            }
            const index = @intCast(u32, i - 1);
            const prev = tree.tokenSlice(index);
            const prev_start = token_start[index];
            if (prev_start + prev.len <= byte_position) {
                return null;
            }
            return index;
        }
    }
    return null;
}

const Self = @This();

tree: *const Ast,
index: u32,

pub fn init(tree: *const Ast, index: u32) Self {
    return Self{
        .tree = tree,
        .index = index,
    };
}

pub fn fromBytePosition(tree: *const Ast, byte_position: u32) ?Self {
    return if (findTokenIndex(tree, byte_position)) |index|
        init(tree, index)
    else
        null;
}

pub fn getText(self: Self) []const u8 {
    return self.tree.tokenSlice(self.index);
}

pub fn getTag(self: Self) std.zig.Token.Tag {
    const token_tag = self.tree.tokens.items(.tag);
    return token_tag[self.index];
}

test {
    const source =
        \\pub fn main() !void {
        \\    
        \\}
    ;

    const allocator = std.testing.allocator;

    var tree = try std.zig.parse(allocator, source);
    defer tree.deinit(allocator);

    const token = fromBytePosition(&tree, 1).?;
    try std.testing.expect(token.index == 0);
    try std.testing.expectEqualSlices(u8, token.getText(), "pub");
    try std.testing.expectEqual(token.getTag(), .keyword_pub);

    try std.testing.expect(fromBytePosition(&tree, 3) == null);

    std.debug.print("\n{}\n", .{token.getTag()});
}
