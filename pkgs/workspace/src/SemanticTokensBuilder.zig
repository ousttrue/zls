const std = @import("std");
const semantic_tokens = @import("./semantic_tokens.zig");
const SemanticTokenType = semantic_tokens.SemanticTokenType;
const SemanticTokenModifiers = semantic_tokens.SemanticTokenModifiers;

const LineX = struct {
    line: u32,
    x: u32,
};

fn getPositionFromBytePosition(text: []const u8, byte_position: usize) !LineX {
    var line: u32 = 0;
    var x: u32 = 0;
    var i: u32 = 0;
    while (i < byte_position) {
        const len: u32 = try std.unicode.utf8ByteSequenceLength(text[i]);
        if (text[i] == '\n') {
            // new line
            line += 1;
            x = 0;
        } else {
            x += len;
        }
        i += len;
    }
    return LineX{ .line = line, .x = x };
}

const Self = @This();

array: std.ArrayList(u32),
text: []const u8,

fn init(allocator: std.mem.Allocator, text: []const u8) Self {
    return Self{
        .array = std.ArrayList(u32).init(allocator),
        .text = text,
    };
}

fn push(self: *Self, token: std.zig.Token) !void {
    switch (token.tag) {
        .eof => unreachable,
        .string_literal,
        .multiline_string_literal_line,
        .char_literal,
        => {
            try self.push_semantic_token(token.loc, .string, .{});
        },
        .integer_literal,
        .float_literal,
        => {
            try self.push_semantic_token(token.loc, .number, .{});
        },
        .identifier => {
            // container variable
            // field
            // decl, call
            // type
            try self.push_semantic_token(token.loc, .variable, .{});
        },
        .builtin => {
            try self.push_semantic_token(token.loc, .function, .{
                .defaultLibrary = true,
            });
        },
        .invalid,
        .invalid_periodasterisks,
        => {},
        .bang,
        .pipe,
        .pipe_pipe,
        .pipe_equal,
        .equal,
        .equal_equal,
        .equal_angle_bracket_right,
        .bang_equal,
        .l_paren,
        .r_paren,
        .semicolon,
        .percent,
        .percent_equal,
        .l_brace,
        .r_brace,
        .l_bracket,
        .r_bracket,
        .period,
        .period_asterisk,
        .ellipsis2,
        .ellipsis3,
        .caret,
        .caret_equal,
        .plus,
        .plus_plus,
        .plus_equal,
        .plus_percent,
        .plus_percent_equal,
        .plus_pipe,
        .plus_pipe_equal,
        .minus,
        .minus_equal,
        .minus_percent,
        .minus_percent_equal,
        .minus_pipe,
        .minus_pipe_equal,
        .asterisk,
        .asterisk_equal,
        .asterisk_asterisk,
        .asterisk_percent,
        .asterisk_percent_equal,
        .asterisk_pipe,
        .asterisk_pipe_equal,
        .arrow,
        .colon,
        .slash,
        .slash_equal,
        .comma,
        .ampersand,
        .ampersand_equal,
        .question_mark,
        .angle_bracket_left,
        .angle_bracket_left_equal,
        .angle_bracket_angle_bracket_left,
        .angle_bracket_angle_bracket_left_equal,
        .angle_bracket_angle_bracket_left_pipe,
        .angle_bracket_angle_bracket_left_pipe_equal,
        .angle_bracket_right,
        .angle_bracket_right_equal,
        .angle_bracket_angle_bracket_right,
        .angle_bracket_angle_bracket_right_equal,
        .tilde,
        => {
            try self.push_semantic_token(token.loc, .operator, .{});
        },
        .doc_comment,
        .container_doc_comment,
        => {
            try self.push_semantic_token(token.loc, .comment, .{ .documentation = true });
        },
        .keyword_addrspace,
        .keyword_align,
        .keyword_allowzero,
        .keyword_and,
        .keyword_anyframe,
        .keyword_anytype,
        .keyword_asm,
        .keyword_async,
        .keyword_await,
        .keyword_break,
        .keyword_callconv,
        .keyword_catch,
        .keyword_comptime,
        .keyword_const,
        .keyword_continue,
        .keyword_defer,
        .keyword_else,
        .keyword_enum,
        .keyword_errdefer,
        .keyword_error,
        .keyword_export,
        .keyword_extern,
        .keyword_fn,
        .keyword_for,
        .keyword_if,
        .keyword_inline,
        .keyword_noalias,
        .keyword_noinline,
        .keyword_nosuspend,
        .keyword_opaque,
        .keyword_or,
        .keyword_orelse,
        .keyword_packed,
        .keyword_pub,
        .keyword_resume,
        .keyword_return,
        .keyword_linksection,
        .keyword_struct,
        .keyword_suspend,
        .keyword_switch,
        .keyword_test,
        .keyword_threadlocal,
        .keyword_try,
        .keyword_union,
        .keyword_unreachable,
        .keyword_usingnamespace,
        .keyword_var,
        .keyword_volatile,
        .keyword_while,
        => {
            try self.push_semantic_token(token.loc, .keyword, .{});
        },
    }
}

fn push_semantic_token(
    self: *Self,
    loc: std.zig.Token.Loc,
    token_type: SemanticTokenType,
    modifier: SemanticTokenModifiers,
) !void {
    const pos_x = try getPositionFromBytePosition(self.text, loc.start);
    try self.array.appendSlice(&.{
        pos_x.line,
        pos_x.x,
        @truncate(u32, loc.end - loc.start),
        @enumToInt(token_type),
        modifier.toInt(),
    });
}

pub fn writeAllSemanticTokens(arena: *std.heap.ArenaAllocator, text: []const u8) ![]u32 {
    const allocator = arena.allocator();

    var self = init(allocator, text);
    var tokenizer = std.zig.Tokenizer.init(try allocator.dupeZ(u8, text));
    while (true) {
        const token = tokenizer.next();
        if (token.tag == .eof) {
            break;
        }
        try self.push(token);
    }

    return self.array.items;
}
