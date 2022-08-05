const std = @import("std");
const AstContext = @import("./AstContext.zig");
const Self = @This();

context: *const AstContext,
index: u32,

pub fn init(context: *const AstContext, index: u32) Self {
    return Self{
        .context = context,
        .index = index,
    };
}

pub fn fromTokenIndex(context: *const AstContext, token_idx: u32) Self {
    const idx = context.tokens_node[token_idx];
    return init(context, idx);
}

test {
    const source =
        \\pub fn main() !void {
        \\    
        \\}
    ;
    const allocator = std.testing.allocator;
    const text = allocator.dupeZ(u8, source);
    defer allocator.free(text);

    const context = AstContext.new(allocator, text);
    defer context.delete();

    const node = fromTokenIndex(context, 0);
    _ = node;
    // try std.testing.expect()
}
