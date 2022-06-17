const std = @import("std");
const lsp = @import("lsp");
const DocumentPosition = @import("./document_position.zig").DocumentPosition;

const logger = std.log.scoped(.position_context);

const Error = error{
    AtMark,
    Other,
    Comment,
};

const SourceRange = std.zig.Token.Loc;

pub const PositionContext = union(enum) {
    builtin: SourceRange,
    comment,
    string_literal: SourceRange,
    field_access: SourceRange,
    var_access: SourceRange,
    global_error_set,
    enum_literal,
    pre_label,
    label: bool,
    other,
    empty,

    pub fn range(self: PositionContext) ?SourceRange {
        return switch (self) {
            .builtin => |r| r,
            .comment => null,
            .string_literal => |r| r,
            .field_access => |r| r,
            .var_access => |r| r,
            .enum_literal => null,
            .pre_label => null,
            .label => null,
            .other => null,
            .empty => null,
            .global_error_set => null,
        };
    }
};

fn tokenRangeAppend(prev: SourceRange, token: std.zig.Token) SourceRange {
    return .{
        .start = prev.start,
        .end = token.loc.end,
    };
}

const StackState = struct {
    ctx: PositionContext,
    stack_id: enum { Paren, Bracket, Global },
};

const Stack = struct {
    const Self = @This();

    stack: std.ArrayList(StackState),

    fn init(allocator: std.mem.Allocator) Self {
        return .{ .stack = std.ArrayList(StackState).initCapacity(allocator, 8) catch unreachable };
    }

    fn append(self: *Self, state: StackState) void {
        self.stack.append(state) catch unreachable;
    }

    fn pop(self: *Self) void {
        _ = self.stack.pop();
    }

    fn popOrNull(self: *Self) ?StackState {
        return self.stack.popOrNull();
    }

    fn peek(self: *Self) *StackState {
        if (self.stack.items.len == 0) {
            self.stack.append(.{ .ctx = .empty, .stack_id = .Global }) catch unreachable;
        }
        return &self.stack.items[self.stack.items.len - 1];
    }
};

fn getState(arena: *std.heap.ArenaAllocator, document: lsp.TextDocument, doc_position: DocumentPosition) Error!?StackState {
    const line = doc_position.line;
    const line_mem_start = @ptrToInt(line.ptr) - @ptrToInt(document.mem.ptr);

    var stack = Stack.init(arena.allocator());

    {
        var held_line = document.borrowNullTerminatedSlice(
            line_mem_start,
            line_mem_start + doc_position.col,
        );
        defer held_line.release();

        var tokenizer = std.zig.Tokenizer.init(held_line.data());
        while (true) {
            const tok = tokenizer.next();

            // Early exits.
            switch (tok.tag) {
                .invalid => {
                    // Single '@' do not return a builtin token so we check this on our own.
                    if (line[doc_position.col - 1] == '@') {
                        return Error.AtMark;
                    }
                    return Error.Other;
                },
                .doc_comment, .container_doc_comment => return Error.Comment,
                .eof => break,
                else => {},
            }

            // State changes
            var curr_ctx = stack.peek();
            switch (tok.tag) {
                .string_literal, .multiline_string_literal_line => curr_ctx.ctx = .{ .string_literal = tok.loc },
                .identifier => switch (curr_ctx.ctx) {
                    .empty, .pre_label => curr_ctx.ctx = .{ .var_access = tok.loc },
                    .label => |filled| if (!filled) {
                        curr_ctx.ctx = .{ .label = true };
                    } else {
                        curr_ctx.ctx = .{ .var_access = tok.loc };
                    },
                    else => {},
                },
                .builtin => switch (curr_ctx.ctx) {
                    .empty, .pre_label => curr_ctx.ctx = .{ .builtin = tok.loc },
                    else => {},
                },
                .period, .period_asterisk => switch (curr_ctx.ctx) {
                    .empty, .pre_label => curr_ctx.ctx = .enum_literal,
                    .enum_literal => curr_ctx.ctx = .empty,
                    .field_access => {},
                    .other => {},
                    .global_error_set => {},
                    else => curr_ctx.ctx = .{
                        .field_access = tokenRangeAppend(curr_ctx.ctx.range().?, tok),
                    },
                },
                .keyword_break, .keyword_continue => curr_ctx.ctx = .pre_label,
                .colon => if (curr_ctx.ctx == .pre_label) {
                    curr_ctx.ctx = .{ .label = false };
                } else {
                    curr_ctx.ctx = .empty;
                },
                .question_mark => switch (curr_ctx.ctx) {
                    .field_access => {},
                    else => curr_ctx.ctx = .empty,
                },
                .l_paren => stack.append(.{ .ctx = .empty, .stack_id = .Paren }),
                .l_bracket => stack.append(.{ .ctx = .empty, .stack_id = .Bracket }),
                .r_paren => {
                    stack.pop();
                    if (curr_ctx.stack_id != .Paren) {
                        stack.peek().ctx = .empty;
                    }
                },
                .r_bracket => {
                    stack.pop();
                    if (curr_ctx.stack_id != .Bracket) {
                        stack.peek().ctx = .empty;
                    }
                },
                .keyword_error => curr_ctx.ctx = .global_error_set,
                else => curr_ctx.ctx = .empty,
            }

            switch (curr_ctx.ctx) {
                .field_access => |r| curr_ctx.ctx = .{
                    .field_access = tokenRangeAppend(r, tok),
                },
                else => {},
            }
        }
    }

    return stack.popOrNull();
}

pub fn documentPositionContext(arena: *std.heap.ArenaAllocator, document: lsp.TextDocument, doc_position: DocumentPosition) PositionContext {
    _ = arena;
    _ = document;
    _ = doc_position;
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

    logger.debug("[doc_position]{s}", .{doc_position.line});

    return .empty;
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
