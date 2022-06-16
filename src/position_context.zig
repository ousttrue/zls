const std = @import("std");
const lsp = @import("lsp");
const DocumentPosition = @import("./document_position.zig").DocumentPosition;

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

fn peek(arr: *std.ArrayList(StackState)) !*StackState {
    if (arr.items.len == 0) {
        try arr.append(.{ .ctx = .empty, .stack_id = .Global });
    }
    return &arr.items[arr.items.len - 1];
}

pub fn documentPositionContext(arena: *std.heap.ArenaAllocator, document: lsp.TextDocument, doc_position: DocumentPosition) PositionContext {
    const line = doc_position.line;
    const line_mem_start = @ptrToInt(line.ptr) - @ptrToInt(document.mem.ptr);

    var stack = std.ArrayList(StackState).initCapacity(arena.allocator(), 8) catch @panic("initCapacity");

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
                        return PositionContext{
                            .builtin = .{
                                .start = doc_position.col - 1,
                                .end = doc_position.col,
                            },
                        };
                    }
                    return .other;
                },
                .doc_comment, .container_doc_comment => return .comment,
                .eof => break,
                else => {},
            }

            // State changes
            var curr_ctx = peek(&stack) catch @panic("peek");
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
                .l_paren => stack.append(.{ .ctx = .empty, .stack_id = .Paren }) catch @panic("append"),
                .l_bracket => stack.append(.{ .ctx = .empty, .stack_id = .Bracket }) catch @panic("append"),
                .r_paren => {
                    _ = stack.pop();
                    if (curr_ctx.stack_id != .Paren) {
                        (peek(&stack) catch @panic("peek")).ctx = .empty;
                    }
                },
                .r_bracket => {
                    _ = stack.pop();
                    if (curr_ctx.stack_id != .Bracket) {
                        (peek(&stack) catch @panic("peek")).ctx = .empty;
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

    return block: {
        if (stack.popOrNull()) |state| {
            switch (state.ctx) {
                .empty => {},
                .label => |filled| {
                    // We need to check this because the state could be a filled
                    // label if only a space follows it
                    const last_char = line[doc_position.col - 1];
                    if (!filled or last_char != ' ') {
                        break :block state.ctx;
                    }
                },
                else => break :block state.ctx,
            }
        }
        if (doc_position.col < line.len) {
            var held_line = document.borrowNullTerminatedSlice(
                line_mem_start + doc_position.col,
                line_mem_start + line.len,
            );
            defer held_line.release();

            switch (line[doc_position.col]) {
                'a'...'z', 'A'...'Z', '_', '@' => {},
                else => break :block .empty,
            }
            var tokenizer = std.zig.Tokenizer.init(held_line.data());
            const tok = tokenizer.next();
            if (tok.tag == .identifier)
                break :block PositionContext{ .var_access = tok.loc };
        }
        break :block .empty;
    };
}
