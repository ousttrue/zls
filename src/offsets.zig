const std = @import("std");
const lsp = @import("lsp");
const Ast = std.zig.Ast;

pub const OffsetError = error{
    PositionNegativeCharacter,
};

pub const Encoding = enum {
    utf8,
    utf16,
};

pub const DocumentPosition = struct {
    line: []const u8,
    line_index: usize,
    absolute_index: usize,
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

pub fn documentPosition(doc: lsp.TextDocument, position: lsp.Position, encoding: Encoding) !DocumentPosition {
    if (position.character < 0) {
        return OffsetError.PositionNegativeCharacter;
    }

    var split_iterator = std.mem.split(u8, doc.text, "\n");

    var line_idx: i64 = 0;
    var line: []const u8 = "";
    while (line_idx < position.line) : (line_idx += 1) {
        line = split_iterator.next() orelse return error.InvalidParams;
    }

    const line_start_idx = split_iterator.index.?;
    line = split_iterator.next() orelse return error.InvalidParams;

    if (encoding == .utf8) {
        const index = @intCast(i64, line_start_idx) + position.character;
        if (index < 0 or index > @intCast(i64, doc.text.len)) {
            return error.InvalidParams;
        }
        return DocumentPosition{
            .line = line,
            .absolute_index = @intCast(usize, index),
            .line_index = @intCast(usize, position.character),
        };
    } else {
        const utf8 = doc.text[line_start_idx..];
        var utf8_idx: usize = 0;
        var utf16_idx: usize = 0;
        while (utf16_idx < position.character) {
            if (utf8_idx > utf8.len) {
                return error.InvalidParams;
            }

            const n = try std.unicode.utf8ByteSequenceLength(utf8[utf8_idx]);
            const next_utf8_idx = utf8_idx + n;
            const codepoint = try std.unicode.utf8Decode(utf8[utf8_idx..next_utf8_idx]);
            if (codepoint < 0x10000) {
                utf16_idx += 1;
            } else {
                utf16_idx += 2;
            }
            utf8_idx = next_utf8_idx;
        }
        return DocumentPosition{
            .line = line,
            .absolute_index = line_start_idx + utf8_idx,
            .line_index = utf8_idx,
        };
    }
}

pub fn lineSectionLength(tree: Ast, start_index: usize, end_index: usize, encoding: Encoding) !usize {
    const source = tree.source[start_index..];
    std.debug.assert(end_index >= start_index and source.len >= end_index - start_index);
    if (encoding == .utf8) {
        return end_index - start_index;
    }

    var result: usize = 0;
    var i: usize = 0;
    while (i + start_index < end_index) {
        std.debug.assert(source[i] != '\n');

        const n = try std.unicode.utf8ByteSequenceLength(source[i]);
        if (i + n >= source.len)
            return error.CodepointTooLong;

        const codepoint = try std.unicode.utf8Decode(source[i .. i + n]);

        result += 1 + @as(usize, @boolToInt(codepoint >= 0x10000));
        i += n;
    }
    return result;
}

pub const TokenLocation = struct {
    line: usize,
    column: usize,
    offset: usize,

    pub fn add(lhs: TokenLocation, rhs: TokenLocation) TokenLocation {
        return .{
            .line = lhs.line + rhs.line,
            .column = if (rhs.line == 0)
                lhs.column + rhs.column
            else
                rhs.column,
            .offset = rhs.offset,
        };
    }
};

pub fn tokenRelativeLocation(tree: Ast, start_index: usize, token_start: usize, encoding: Encoding) !TokenLocation {
    std.debug.assert(token_start >= start_index);
    var loc = TokenLocation{
        .line = 0,
        .column = 0,
        .offset = 0,
    };

    const source = tree.source[start_index..];
    var i: usize = 0;
    while (i + start_index < token_start) {
        const c = source[i];
        if (c == '\n') {
            loc.line += 1;
            loc.column = 0;
            i += 1;
        } else {
            if (encoding == .utf16) {
                const n = try std.unicode.utf8ByteSequenceLength(c);
                if (i + n >= source.len)
                    return error.CodepointTooLong;

                const codepoint = try std.unicode.utf8Decode(source[i .. i + n]);
                loc.column += 1 + @as(usize, @boolToInt(codepoint >= 0x10000));
                i += n;
            } else {
                loc.column += 1;
                i += 1;
            }
        }
    }
    loc.offset = i + start_index;
    return loc;
}

/// Asserts the token is comprised of valid utf8
pub fn tokenLength(tree: Ast, token: Ast.TokenIndex, encoding: Encoding) usize {
    const token_loc = tokenLocation(tree, token);
    if (encoding == .utf8)
        return token_loc.end - token_loc.start;

    var i: usize = token_loc.start;
    var utf16_len: usize = 0;
    while (i < token_loc.end) {
        const n = std.unicode.utf8ByteSequenceLength(tree.source[i]) catch unreachable;
        const codepoint = std.unicode.utf8Decode(tree.source[i .. i + n]) catch unreachable;
        if (codepoint < 0x10000) {
            utf16_len += 1;
        } else {
            utf16_len += 2;
        }
        i += n;
    }
    return utf16_len;
}

/// Token location inside source
pub const Loc = struct {
    start: usize,
    end: usize,
};

pub fn tokenLocation(tree: Ast, token_index: Ast.TokenIndex) Loc {
    const start = tree.tokens.items(.start)[token_index];
    const tag = tree.tokens.items(.tag)[token_index];

    // For some tokens, re-tokenization is needed to find the end.
    var tokenizer: std.zig.Tokenizer = .{
        .buffer = tree.source,
        .index = start,
        .pending_invalid_token = null,
    };

    const token = tokenizer.next();
    std.debug.assert(token.tag == tag);
    return .{ .start = token.loc.start, .end = token.loc.end };
}

pub fn documentRange(doc: lsp.TextDocument, encoding: Encoding) !lsp.Range {
    var line_idx: i64 = 0;
    var curr_line: []const u8 = doc.text;

    var split_iterator = std.mem.split(u8, doc.text, "\n");
    while (split_iterator.next()) |line| : (line_idx += 1) {
        curr_line = line;
    }

    if (encoding == .utf8) {
        return lsp.Range{
            .start = .{
                .line = 0,
                .character = 0,
            },
            .end = .{
                .line = line_idx,
                .character = @intCast(i64, curr_line.len),
            },
        };
    } else {
        var utf16_len: usize = 0;
        var line_utf8_idx: usize = 0;
        while (line_utf8_idx < curr_line.len) {
            const n = try std.unicode.utf8ByteSequenceLength(curr_line[line_utf8_idx]);
            const codepoint = try std.unicode.utf8Decode(curr_line[line_utf8_idx .. line_utf8_idx + n]);
            if (codepoint < 0x10000) {
                utf16_len += 1;
            } else {
                utf16_len += 2;
            }
            line_utf8_idx += n;
        }
        return lsp.Range{
            .start = .{
                .line = 0,
                .character = 0,
            },
            .end = .{
                .line = line_idx,
                .character = @intCast(i64, utf16_len),
            },
        };
    }
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

fn tokenRangeAppend(prev: SourceRange, token: std.zig.Token) SourceRange {
    return .{
        .start = prev.start,
        .end = token.loc.end,
    };
}

pub fn documentPositionContext(arena: *std.heap.ArenaAllocator, document: lsp.TextDocument, doc_position: DocumentPosition) PositionContext {
    const line = doc_position.line;
    const line_mem_start = @ptrToInt(line.ptr) - @ptrToInt(document.mem.ptr);

    var stack = std.ArrayList(StackState).initCapacity(arena.allocator(), 8) catch @panic("initCapacity");

    {
        var held_line = document.borrowNullTerminatedSlice(
            line_mem_start,
            line_mem_start + doc_position.line_index,
        );
        defer held_line.release();

        var tokenizer = std.zig.Tokenizer.init(held_line.data());
        while (true) {
            const tok = tokenizer.next();

            // Early exits.
            switch (tok.tag) {
                .invalid => {
                    // Single '@' do not return a builtin token so we check this on our own.
                    if (line[doc_position.line_index - 1] == '@') {
                        return PositionContext{
                            .builtin = .{
                                .start = doc_position.line_index - 1,
                                .end = doc_position.line_index,
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
                    const last_char = line[doc_position.line_index - 1];
                    if (!filled or last_char != ' ') {
                        break :block state.ctx;
                    }
                },
                else => break :block state.ctx,
            }
        }
        if (doc_position.line_index < line.len) {
            var held_line = document.borrowNullTerminatedSlice(
                line_mem_start + doc_position.line_index,
                line_mem_start + line.len,
            );
            defer held_line.release();

            switch (line[doc_position.line_index]) {
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
