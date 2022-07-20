const std = @import("std");
const Ast = std.zig.Ast;
const analysis = @import("./analysis.zig");
const Utf8Buffer = @import("./Utf8Buffer.zig");
const BuildFile = @import("./BuildFile.zig");
const DocumentPosition = @import("./DocumentPosition.zig");
const PositionContext = @import("./position_context.zig").PositionContext;
const AstContext = @import("./AstContext.zig");
const Self = @This();
const logger = std.log.scoped(.Document);

allocator: std.mem.Allocator,
utf8_buffer: Utf8Buffer,
count: usize,
/// Contains one entry for every import in the document
import_uris: []const []const u8,
/// Items in this array list come from `import_uris`
imports_used: std.ArrayListUnmanaged([]const u8),
tree: Ast,
ast_context: *AstContext,
document_scope: analysis.DocumentScope,

associated_build_file: ?*BuildFile,
is_build_file: ?*BuildFile,

pub fn new(allocator: std.mem.Allocator, uri: []const u8, text: [:0]u8) !*Self {
    var self = try allocator.create(Self);
    errdefer allocator.destroy(self);

    var tree = try std.zig.parse(allocator, text);
    errdefer tree.deinit(allocator);

    var document_scope = try analysis.makeDocumentScope(allocator, tree);
    errdefer document_scope.deinit(allocator);

    self.* = Self{
        .allocator = allocator,
        .count = 1,
        .import_uris = &.{},
        .imports_used = .{},
        .utf8_buffer = Utf8Buffer.init(uri, text),
        .tree = tree,
        .ast_context = undefined,
        .document_scope = document_scope,
        .associated_build_file = null,
        .is_build_file = null,
    };

    self.ast_context = AstContext.new(allocator, &self.tree);

    return self;
}

pub fn delete(self: *Self) void {
    for (self.import_uris) |imp_uri| {
        self.allocator.free(imp_uri);
    }
    self.allocator.free(self.import_uris);
    self.imports_used.deinit(self.allocator);
    self.document_scope.deinit(self.allocator);
    self.ast_context.delete();
    self.tree.deinit(self.allocator);
    self.allocator.free(self.utf8_buffer.mem);
    self.allocator.destroy(self);
}

pub fn getPositionContext(self: Self, byte_pos: usize) PositionContext {
    const token_with_index = self.ast_context.tokenFromBytePos(byte_pos) orelse {
        return .empty;
    };
    const token = token_with_index.token;
    const token_index = token_with_index.index;
    const token_text = self.ast_context.getTokenText(token);
    const node_index = self.ast_context.tokens_node[token_index];
    const tag = self.tree.nodes.items(.tag);
    const node_tag = tag[node_index];
    logger.info("{s}: {s} => {s}", .{ @tagName(token.tag), token_text, @tagName(node_tag) });

    return switch (token_with_index.token.tag) {
        .builtin => .{ .builtin = token.loc },
        .string_literal => .{ .string_literal = token.loc },
        // field_access: SourceRange,
        // var_access: SourceRange,
        // global_error_set,
        // enum_literal,
        // // pre_label,
        // label: bool,
        // // other,
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
        => .keyword,

        else => .empty,
    };
}
