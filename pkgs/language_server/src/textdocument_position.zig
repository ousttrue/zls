const std = @import("std");
const astutil = @import("astutil");
const ws = @import("workspace");
const Config = ws.Config;
const Workspace = ws.Workspace;
const Document = ws.Document;
const PathPosition = astutil.PathPosition;
const DeclWithHandle = ws.DeclWithHandle;
const SymbolLookup = ws.SymbolLookup;
const FixedPath = ws.FixedPath;
const AstToken = astutil.AstToken;
const AstNode = astutil.AstNode;
const Declaration = astutil.Declaration;
const VarType = astutil.VarType;
const ast = ws.ast;
const builtin_completions = ws.builtin_completions;
const logger = std.log.scoped(.textdocument_position);

pub const Hover = struct {
    text: []const u8,
    loc: ?std.zig.Token.Loc = null,
};

pub fn getHover(
    arena: *std.heap.ArenaAllocator,
    // workspace: *Workspace,
    doc: *Document,
    token: AstToken,
    // hover_kind: ast.MarkupFormat,
) !?Hover {
    const allocator = arena.allocator();
    const token_info = try token.allocPrint(allocator);
    const node = AstNode.fromTokenIndex(doc.ast_context, token.index);
    const node_info = try node.allocPrint(allocator);

    var text_buffer = std.ArrayList(u8).init(allocator);
    const w = text_buffer.writer();
    try w.print("`{s} => {s}`\n\n", .{ node_info, token_info });

    switch (token.getTag()) {
        .builtin => {
            if (builtin_completions.find(token.getText())) |builtin| {
                try w.print(
                    "\n```zig\n{s}\n```\n\n{s}",
                    .{ builtin.signature, builtin.documentation },
                );
                return Hover{
                    .text = text_buffer.items,
                };
            }
        },
        .identifier => {
            switch (node.getTag()) {
                .identifier => {
                    if (Declaration.findFromBlock(node)) |decl| {
                        const text = try decl.allocPrint(allocator);
                        try w.print("local => {s}", .{text});
                        return Hover{
                            .text = text_buffer.items,
                            .loc = decl.token.getLoc(),
                        };
                    } else {
                        logger.debug("identifer: decl not found", .{});
                    }
                },
                else => {
                    const var_type = VarType.init(node);
                    const text = try var_type.allocPrint(allocator);
                    try w.print("var_type: {s}", .{text});
                    return Hover{
                        .text = text_buffer.items,
                    };
                },
            }
        },
        else => {},
    }

    return Hover{
        .text = text_buffer.items,
    };
}

pub fn getRename(
    arena: *std.heap.ArenaAllocator,
    workspace: *Workspace,
    doc: *Document,
    token: AstToken,
) !?[]const PathPosition {
    if (token.getTag() != .identifier) {
        return null;
    }

    var lookup = SymbolLookup.init(arena.allocator());
    defer lookup.deinit();
    const decl = lookup.lookupIdentifier(arena, workspace, doc, token) orelse {
        return null;
    };

    return try decl.renameSymbol(arena, workspace);
}

pub fn getGoto(
    arena: *std.heap.ArenaAllocator,
    workspace: *Workspace,
    doc: *Document,
    token: AstToken,
) !?PathPosition {
    _ = arena;
    const node = AstNode.fromTokenIndex(doc.ast_context, token.index);

    switch (token.getTag()) {
        .string_literal => {
            // goto import file
            const text = token.getText();
            if (text.len > 2) {
                const path = try workspace.resolveImportPath(doc, text[1 .. text.len - 1]);
                return PathPosition{ .path = path, .loc = .{ .start = 0, .end = 0 } };
            } else {
                return null;
            }
        },
        .identifier => {
            switch (node.getTag()) {
                .identifier => {
                    if (Declaration.findFromBlock(node)) |decl| {
                        return PathPosition{ .path = doc.path, .loc = decl.token.getLoc() };
                    } else {
                        return error.DeclNotFound;
                    }
                },
                else => {
                    const var_type = VarType.init(node);
                    _ = var_type;
                    return null;
                },
            }
        },
        else => {
            return null;
        },
    }
}

pub fn getRenferences(
    arena: *std.heap.ArenaAllocator,
    workspace: *Workspace,
    doc: *Document,
    token: AstToken,
    include_decl: bool,
    config: *Config,
) !?[]PathPosition {
    if (token.getTag() != .identifier) {
        return null;
    }

    var lookup = SymbolLookup.init(arena.allocator());
    defer lookup.deinit();
    const decl = lookup.lookupIdentifier(arena, workspace, doc, token) orelse {
        return null;
    };

    var locs = std.ArrayList(PathPosition).init(arena.allocator());
    try decl.symbolReferences(arena, workspace, include_decl, &locs, config.skip_std_references);
    return locs.items;
}
