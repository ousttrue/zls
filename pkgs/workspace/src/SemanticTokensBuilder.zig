const std = @import("std");
const semantic_tokens = @import("./semantic_tokens.zig");
const SemanticTokenType = semantic_tokens.SemanticTokenType;
const SemanticTokenModifiers = semantic_tokens.SemanticTokenModifiers;
const Document = @import("./Document.zig");

const Self = @This();

array: std.ArrayList(u32),
document: *Document,

fn init(allocator: std.mem.Allocator, document: *Document) Self {
    return Self{
        .array = std.ArrayList(u32).init(allocator),
        .document = document,
    };
}

fn push(self: *Self, token_idx: usize, token: std.zig.Token) !void {
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
            try self.push_identifier(token_idx, token.loc);
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

fn is_call(node_tag: std.zig.Ast.Node.Tag) bool {
    return switch (node_tag) {
        .call, .call_one => true,
        else => false,
    };
}

fn is_operator(node_tag: std.zig.Ast.Node.Tag) bool {
    return switch (node_tag) {
        .@"errdefer",
        .@"defer",
        .@"catch",
        // .field_access,
        .unwrap_optional,
        .equal_equal,
        .bang_equal,
        .less_than,
        .greater_than,
        .less_or_equal,
        .greater_or_equal,
        .assign_mul,
        .assign_div,
        .assign_mod,
        .assign_add,
        .assign_sub,
        .assign_shl,
        .assign_shl_sat,
        .assign_shr,
        .assign_bit_and,
        .assign_bit_xor,
        .assign_bit_or,
        .assign_mul_wrap,
        .assign_add_wrap,
        .assign_sub_wrap,
        .assign_mul_sat,
        .assign_add_sat,
        .assign_sub_sat,
        .assign,
        .merge_error_sets,
        .mul,
        .div,
        .mod,
        .array_mult,
        .mul_wrap,
        .mul_sat,
        .add,
        .sub,
        .array_cat,
        .add_wrap,
        .sub_wrap,
        .add_sat,
        .sub_sat,
        .shl,
        .shl_sat,
        .shr,
        .bit_and,
        .bit_xor,
        .bit_or,
        .@"orelse",
        .bool_and,
        .bool_or,
        .bool_not,
        .negation,
        .bit_not,
        .negation_wrap,
        .address_of,
        .@"try",
        => true,
        else => false,
    };
}

fn is_variable(node_tag: std.zig.Ast.Node.Tag) bool {
    if (is_call(node_tag)) {
        return true;
    }
    if (is_operator(node_tag)) {
        return true;
    }
    return switch (node_tag) {
        .switch_comma => true,
        else => false,
    };
}

fn is_literal(name: []const u8) bool {
    for ([_][]const u8{ "true", "false", "null", "undefined" }) |value| {
        if (std.mem.eql(u8, name, value)) {
            return true;
        }
    }
    return false;
}

fn is_type(name: []const u8) bool {
    for ([_][]const u8{ "type", "bool", "f16", "f32", "f64", "i8", "i16", "i32", "i64", "u8", "u16", "u32", "u64", "usize" }) |value| {
        if (std.mem.eql(u8, name, value)) {
            return true;
        }
    }
    return false;
}

fn push_identifier(self: *Self, token_idx: usize, loc: std.zig.Token.Loc) !void {
    const ast_context = self.document.ast_context;
    const idx = ast_context.tokens_node[token_idx];
    const tag = ast_context.tree.nodes.items(.tag);
    const node_tag = tag[idx];
    // const parent_idx = ast_context.nodes_parent[idx];
    // const parent_tag = tag[parent_idx];
    const name = ast_context.getText(loc);
    switch (node_tag) {
        .enum_literal => {
            try self.push_semantic_token(loc, .enumMember, .{});
        },
        .identifier, .field_access => {
            if (is_literal(name)) {
                try self.push_semantic_token(loc, .number, .{});
            } else if (std.ascii.isUpper(name[0])) {
                try self.push_semantic_token(loc, .type, .{});
            } else if (is_type(name)) {
                try self.push_semantic_token(loc, .type, .{});
            } else {
                try self.push_semantic_token(loc, .variable, .{});
            }
        },
        .ptr_type_aligned => {
            try self.push_semantic_token(loc, .type, .{});
        },
        .simple_var_decl => {
            try self.push_semantic_token(loc, .variable, .{});
        },
        .fn_proto_multi, .fn_proto_simple => {
            try self.push_semantic_token(loc, .function, .{});
        },
        .container_field_init => {
            try self.push_semantic_token(loc, .property, .{});
        },
        else => {
            // try self.push_semantic_token(loc, .variable, .{});
        },
    }
}

fn push_semantic_token(
    self: *Self,
    loc: std.zig.Token.Loc,
    token_type: SemanticTokenType,
    modifier: SemanticTokenModifiers,
) !void {
    const pos_x = try self.document.line_position.getPositionFromBytePosition(loc.start);
    try self.array.appendSlice(&.{
        pos_x.line,
        pos_x.x,
        @truncate(u32, loc.end - loc.start),
        @enumToInt(token_type),
        modifier.toInt(),
    });
}

pub fn writeAllSemanticTokens(arena: *std.heap.ArenaAllocator, document: *Document) ![]u32 {
    const allocator = arena.allocator();

    var self = init(allocator, document);
    for (document.ast_context.tokens.items) |token, i| {
        try self.push(i, token);
    }

    return self.array.items;
}
