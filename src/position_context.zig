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

    // pub fn range(self: PositionContext) ?SourceRange {
    //     return switch (self) {
    //         .builtin => |r| r,
    //         .comment => null,
    //         .string_literal => |r| r,
    //         .field_access => |r| r,
    //         .var_access => |r| r,
    //         .enum_literal => null,
    //         .pre_label => null,
    //         .label => null,
    //         .other => null,
    //         .empty => null,
    //         .global_error_set => null,
    //     };
    // }
};

fn tokenRangeAppend(prev: SourceRange, token: std.zig.Token) SourceRange {
    return .{
        .start = prev.start,
        .end = token.loc.end,
    };
}
// // Early exits.
// switch (tok.tag) {
//     .invalid => {
//         // Single '@' do not return a builtin token so we check this on our own.
//         if (line[doc_position.col - 1] == '@') {
//             return Error.AtMark;
//         }
//         return Error.Other;
//     },
//     .doc_comment, .container_doc_comment => return Error.Comment,
// }

// State changes
// switch (tok.tag) {
// .string_literal, .multiline_string_literal_line => curr_ctx.ctx = .{ .string_literal = tok.loc },
// .identifier => switch (curr_ctx.ctx) {
//     .empty, .pre_label => curr_ctx.ctx = .{ .var_access = tok.loc },
//     .label => |filled| if (!filled) {
//         curr_ctx.ctx = .{ .label = true };
//     } else {
//         curr_ctx.ctx = .{ .var_access = tok.loc };
//     },
//     else => {},
// },
// .builtin => switch (curr_ctx.ctx) {
//     .empty, .pre_label => curr_ctx.ctx = .{ .builtin = tok.loc },
//     else => {},
// },
// .period, .period_asterisk => switch (curr_ctx.ctx) {
//     .empty, .pre_label => curr_ctx.ctx = .enum_literal,
//     .enum_literal => curr_ctx.ctx = .empty,
//     .field_access => {},
//     .other => {},
//     .global_error_set => {},
//     else => curr_ctx.ctx = .{
//         .field_access = tokenRangeAppend(curr_ctx.ctx.range().?, tok),
//     },
// },
// .keyword_break, .keyword_continue => curr_ctx.ctx = .pre_label,
// .colon => if (curr_ctx.ctx == .pre_label) {
//     curr_ctx.ctx = .{ .label = false };
// } else {
//     curr_ctx.ctx = .empty;
// },
// .question_mark => switch (curr_ctx.ctx) {
//     .field_access => {},
//     else => curr_ctx.ctx = .empty,
// },
// .l_paren => stack.append(.{ .ctx = .empty, .stack_id = .Paren }),
// .l_bracket => stack.append(.{ .ctx = .empty, .stack_id = .Bracket }),
// .r_paren => {
//     stack.pop();
//     if (curr_ctx.stack_id != .Paren) {
//         stack.peek().ctx = .empty;
//     }
// },
// .r_bracket => {
//     stack.pop();
//     if (curr_ctx.stack_id != .Bracket) {
//         stack.peek().ctx = .empty;
//     }
// },
// .keyword_error => curr_ctx.ctx = .global_error_set,
// else => curr_ctx.ctx = .empty,
// }

// switch (curr_ctx.ctx) {
//     .field_access => |r| curr_ctx.ctx = .{
//         .field_access = tokenRangeAppend(r, tok),
//     },
//     else => {},
// }

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
        for (self.tokens.items) |item| {
            if (item.on_pos) {
                if (std.mem.startsWith(u8, @tagName(item.token.tag), "keyword_")) {
                    return .keyword;
                }

                switch (item.token.tag) {
                    .identifier => return PositionContext{ .var_access = item.token.loc },
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

// const state_or_null = getState(arena, document, doc_position) catch |err|
//     {
//     return switch (err) {
//         Error.AtMark => PositionContext{
//             .builtin = .{
//                 .start = doc_position.col - 1,
//                 .end = doc_position.col,
//             },
//         },
//         Error.Other => .other,
//         Error.Comment => .comment,
//     };
// };

// if (state_or_null) |state| {
//     switch (state.ctx) {
//         .empty => {},
//         .label => |filled| {
//             // We need to check this because the state could be a filled
//             // label if only a space follows it
//             const last_char = doc_position.line[doc_position.col - 1];
//             if (!filled or last_char != ' ') {
//                 return state.ctx;
//             }
//         },
//         else => {
//             logger.debug("StackState: {s}", .{@tagName(state.ctx)});
//             return state.ctx;
//         },
//     }
// }

// const line = doc_position.line;
// const line_mem_start = @ptrToInt(line.ptr) - @ptrToInt(document.mem.ptr);
// if (doc_position.col < line.len) {
//     var held_line = document.borrowNullTerminatedSlice(
//         line_mem_start + doc_position.col,
//         line_mem_start + line.len,
//     );
//     defer held_line.release();

//     switch (line[doc_position.col]) {
//         'a'...'z', 'A'...'Z', '_', '@' => {},
//         else => return .empty,
//     }
//     var tokenizer = std.zig.Tokenizer.init(held_line.data());
//     const tok = tokenizer.next();
//     if (tok.tag == .identifier) {
//         return PositionContext{ .var_access = tok.loc };
//     }
// }

pub fn documentPositionContext(arena: *std.heap.ArenaAllocator, doc_position: DocumentPosition) PositionContext {
    var parser = LineParser.init(arena, doc_position);
    defer parser.deinit();

    logger.debug("[doc_position]{s}", .{doc_position.line});
    for (parser.tokens.items) |item, i| {
        // logger.debug("[{}] {s}", .{ i, @tagName(tok.tag) });
        item.print(i);
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
