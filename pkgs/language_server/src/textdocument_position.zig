const std = @import("std");
const ws = @import("workspace");
const Config = ws.Config;
const Workspace = ws.Workspace;
const Document = ws.Document;
const UriBytePosition = ws.UriBytePosition;
const DeclWithHandle = ws.DeclWithHandle;
const SymbolLookup = ws.SymbolLookup;
const ast = ws.ast;
const builtin_completions = ws.builtin_completions;

pub fn getHover(
    arena: *std.heap.ArenaAllocator,
    workspace: *Workspace,
    doc: *Document,
    token_index: u32,
    hover_kind: ast.MarkupFormat,
) !?[]const u8 {
    var context_info = try doc.ast_context.getTokenIndexContext(arena.allocator(), token_index);
    const token = doc.ast_context.tokens.items[token_index];
    const name = doc.ast_context.getTokenText(token);
    const allocator = arena.allocator();
    switch (token.tag) {
        .builtin => {
            if (builtin_completions.find(name)) |builtin| {
                return try std.fmt.allocPrint(
                    allocator,
                    "# builtin: {s}\n\n```zig\n{s}\n```\n\n`{s}`\n\n{s}",
                    .{ name, builtin.signature, context_info, builtin.documentation },
                );
            } else {
                return try std.fmt.allocPrint(
                    allocator,
                    "{s}\n* builtin {s} not found",
                    .{ context_info, name },
                );
            }
        },
        .identifier => {
            var lookup = SymbolLookup.init(arena.allocator());
            defer lookup.deinit();
            if (lookup.lookupIdentifier(arena, workspace, doc, token_index)) |decl| {
                const hover = try decl.hoverSymbol(arena, workspace, hover_kind);
                return try std.fmt.allocPrint(
                    allocator,
                    "# {s}\n\n{s}\n\n{s}",
                    .{ name, context_info, hover },
                );
            } else {
                return try std.fmt.allocPrint(
                    allocator,
                    "{s}\n* decl {s} not found",
                    .{ context_info, name },
                );
            }
        },
        else => {
            return context_info;
        },
    }
}

pub fn getRename(
    arena: *std.heap.ArenaAllocator,
    workspace: *Workspace,
    doc: *Document,
    token_index: u32,
) !?[]const UriBytePosition {
    const token = doc.ast_context.tokens.items[token_index];
    if (token.tag != .identifier) {
        return null;
    }

    var lookup = SymbolLookup.init(arena.allocator());
    defer lookup.deinit();
    const decl = lookup.lookupIdentifier(arena, workspace, doc, token_index) orelse {
        return null;
    };

    return try decl.renameSymbol(arena, workspace);
}

pub fn getGoto(
    arena: *std.heap.ArenaAllocator,
    workspace: *Workspace,
    doc: *Document,
    token_index: u32,
    resolve_alias: bool,
) !?UriBytePosition {
    const token = doc.ast_context.tokens.items[token_index];
    switch (token.tag) {
        .string_literal => {
            const text = doc.ast_context.getTokenText(token);
            if (text.len > 2) {
                return workspace.gotoDefinitionString(arena, doc, text[1 .. text.len - 1]);
            } else {
                return null;
            }
        },
        .identifier => {
            // continue;
        },
        else => {
            return null;
        },
    }

    const idx = doc.ast_context.tokens_node[token_index];
    const tag = doc.ast_context.tree.nodes.items(.tag);
    const node_tag = tag[idx];
    var lookup = SymbolLookup.init(arena.allocator());
    defer lookup.deinit();
    switch (node_tag) {
        .field_access => {
            if (lookup.getSymbolFieldAccess(arena, workspace, doc, token_index)) |decl| {
                return decl.gotoDefinitionSymbol(workspace, arena, resolve_alias);
            } else {
                return null;
            }
        },
        // .label => {
        //     if (try DeclWithHandle.lookupLabel(doc, token.loc.start)) |decl| {
        //         return decl.gotoDefinitionSymbol(workspace, arena, false);
        //     } else {
        //         return null;
        //     }
        // },
        else => {
            if (lookup.lookupSymbolGlobalTokenIndex(arena, workspace, doc, token_index)) |decl| {
                return decl.gotoDefinitionSymbol(workspace, arena, resolve_alias);
            } else {
                return null;
            }
        },
    }
}

pub fn getRenferences(
    arena: *std.heap.ArenaAllocator,
    workspace: *Workspace,
    doc: *Document,
    token_index: u32,
    include_decl: bool,
    config: *Config,
) !?[]UriBytePosition {
    const token = doc.ast_context.tokens.items[token_index];
    if (token.tag != .identifier) {
        return null;
    }

    const idx = doc.ast_context.tokens_node[token_index];
    const tag = doc.ast_context.tree.nodes.items(.tag);
    const node_tag = tag[idx];
    var lookup = SymbolLookup.init(arena.allocator());
    defer lookup.deinit();
    switch (node_tag) {
        .field_access => {
            if (lookup.getSymbolFieldAccess(arena, workspace, doc, token_index)) |decl| {
                var locs = std.ArrayList(UriBytePosition).init(arena.allocator());
                try decl.symbolReferences(arena, workspace, include_decl, &locs, config.skip_std_references);
                return locs.items;
            } else {
                return null;
            }
        },
        // .label => {
        //     if ((try DeclWithHandle.lookupLabel(doc, token.loc.start))) |decl| {
        //         var locs = std.ArrayList(UriBytePosition).init(arena.allocator());
        //         try decl.labelReferences(include_decl, &locs);
        //         return locs.items;
        //     } else {
        //         return null;
        //     }
        // },
        else => {
            if (lookup.lookupSymbolGlobalTokenIndex(arena, workspace, doc, token_index)) |decl| {
                var locs = std.ArrayList(UriBytePosition).init(arena.allocator());
                try decl.symbolReferences(arena, workspace, include_decl, &locs, config.skip_std_references);
                return locs.items;
            } else {
                return null;
            }
        },
    }
}
