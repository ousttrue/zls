const std = @import("std");
const lsp = @import("lsp");
const DocumentPosition = @import("./document_position.zig").DocumentPosition;
const logger = std.log.scoped(.position_context);
const SourceRange = std.zig.Token.Loc;

pub const PositionContext = union(enum) {
    builtin: SourceRange,
    // comment,
    string_literal: SourceRange,
    field_access: SourceRange,
    var_access: SourceRange,
    global_error_set,
    enum_literal,
    // pre_label,
    label: bool,
    // other,
    keyword,
    empty,
};

const TokenItem = struct {
    const Self = @This();

    on_pos: bool,
    token: std.zig.Token,
    slice: []const u8,

    fn print(self: Self, i: usize) void {
        if (self.on_pos) {
            logger.debug("<{}> {s}: \"{s}\"", .{ i, @tagName(self.token.tag), self.slice });
        } else {
            logger.debug("[{}] {s}: \"{s}\"", .{ i, @tagName(self.token.tag), self.slice });
        }
    }
};

const LineParser = struct {
    const Self = @This();
    const TokenItemList = std.ArrayList(TokenItem);

    allocator: std.mem.Allocator,
    tokens: TokenItemList,

    fn init(arena: *std.heap.ArenaAllocator, doc_position: DocumentPosition) Self {
        const allocator = arena.allocator();
        var self = Self{
            .allocator = allocator,
            .tokens = TokenItemList.init(allocator),
        };
        var dup = allocator.dupeZ(u8, doc_position.line) catch unreachable;
        defer allocator.free(dup);
        var tokenizer = std.zig.Tokenizer.init(dup);
        while (true) {
            const tok = tokenizer.next();
            switch (tok.tag) {
                .eof => break,
                else => {},
            }
            self.tokens.append(.{
                .on_pos = tok.loc.start <= doc_position.col and tok.loc.end >= doc_position.col,
                .token = tok,
                .slice = doc_position.line[tok.loc.start..tok.loc.end],
            }) catch unreachable;
        }
        return self;
    }

    fn deinit(self: *Self) void {
        self.tokens.deinit();
    }

    fn getState(self: Self) PositionContext {
        for (self.tokens.items) |item, i| {
            if (item.on_pos) {
                if (std.mem.startsWith(u8, @tagName(item.token.tag), "keyword_")) {
                    return .keyword;
                }

                switch (item.token.tag) {
                    .string_literal => return PositionContext{ .string_literal = item.token.loc },
                    .builtin => return PositionContext{ .builtin = item.token.loc },
                    .identifier => {
                        if (i >= 2 and self.tokens.items[i - 1].token.tag == .period) {
                            var j = i;
                            while (j >= 2) : (j -= 2) {
                                if (self.tokens.items[j - 1].token.tag == .period and self.tokens.items[j - 2].token.tag == .identifier) {
                                    continue;
                                } else {
                                    break;
                                }
                            }

                            if (i > j and (i - j) % 2 == 0) {
                                return PositionContext{ .field_access = .{
                                    .start = self.tokens.items[j].token.loc.start,
                                    .end = item.token.loc.end,
                                } };
                            }
                        }
                        return PositionContext{ .var_access = item.token.loc };
                    },
                    else => {
                        logger.debug("{s}", .{@tagName(item.token.tag)});
                    },
                }
                break;
            }
        }

        return .empty;
    }
};

pub fn documentPositionContext(arena: *std.heap.ArenaAllocator, doc_position: DocumentPosition) PositionContext {
    var parser = LineParser.init(arena, doc_position);
    defer parser.deinit();

    logger.debug("[doc_position]{s}", .{doc_position.line});
    for (parser.tokens.items) |item, i| {
        item.print(i);
        if (item.on_pos) {
            break;
        }
    }

    return parser.getState();
}

fn getSlice(all: []const u8, tok: std.zig.Token) []const u8 {
    return all[tok.loc.start..tok.loc.end];
}

test "token" {
    const src =
        \\ const src = "";
    ;
    var tokenizer = std.zig.Tokenizer.init(src);

    std.debug.print("{s}\n", .{getSlice(src, tokenizer.next())});
}
