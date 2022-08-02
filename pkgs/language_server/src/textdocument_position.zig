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

// .fn_proto_multi => {
//     const signature = ast.getFunctionSignature(doc.ast_context.tree, doc.ast_context.tree.fnProtoMulti(idx));
//     return try std.fmt.allocPrint(
//         allocator,
//         "# function: {s}\n\n```zig\n{s}\n```",
//         .{ name, signature },
//     );
// },
//     .label => {
//         logger.debug("[hover][label_access]", .{});
//         if (try offsets.getLabelGlobal(doc_position.absolute_index, doc)) |decl| {
//             return try hoverSymbol(arena, workspace, id, decl, client_capabilities);
//         }
//     },

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

    const idx = doc.ast_context.tokens_node[token_index];
    const tag = doc.ast_context.tree.nodes.items(.tag);
    const node_tag = tag[idx];
    var lookup = SymbolLookup.init(arena.allocator());
    defer lookup.deinit();
    return switch (node_tag) {
        .field_access => if (lookup.getSymbolFieldAccess(arena, workspace, doc, token_index)) |decl|
            try decl.renameSymbol(arena, workspace)
        else
            null,
        // .label => if (try DeclWithHandle.lookupLabel(doc, byte_position)) |decl|
        //     try decl.renameLabel(arena)
        // else
        //     null,
        else => if (lookup.lookupSymbolGlobalTokenIndex(arena, workspace, doc, token_index)) |decl|
            try decl.renameSymbol(arena, workspace)
        else
            null,
    };
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
        // .string_literal => {
        //     return doc.gotoDefinitionString(arena, token.loc.start, workspace.zigenv);
        // },
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

// fn nodeContainsSourceIndex(tree: Ast, node: Ast.Node.Index, source_index: usize) bool {
//     const first_token = ast.tokenLocation(tree, tree.firstToken(node)).start;
//     const last_token = ast.tokenLocation(tree, ast.lastToken(tree, node)).end;
//     return source_index >= first_token and source_index <= last_token;
// }

// fn importStr(tree: std.zig.Ast, node: usize) ?[]const u8 {
//     const node_tags = tree.nodes.items(.tag);
//     const data = tree.nodes.items(.data)[node];
//     const params = switch (node_tags[node]) {
//         .builtin_call, .builtin_call_comma => tree.extra_data[data.lhs..data.rhs],
//         .builtin_call_two, .builtin_call_two_comma => if (data.lhs == 0)
//             &[_]Ast.Node.Index{}
//         else if (data.rhs == 0)
//             &[_]Ast.Node.Index{data.lhs}
//         else
//             &[_]Ast.Node.Index{ data.lhs, data.rhs },
//         else => unreachable,
//     };

//     if (params.len != 1) return null;

//     const import_str = tree.tokenSlice(tree.nodes.items(.main_token)[params[0]]);
//     return import_str[1 .. import_str.len - 1];
// }

// pub fn gotoDefinitionString(
//     self: *Self,
//     arena: *std.heap.ArenaAllocator,
//     pos_index: usize,
//     zigenv: ZigEnv,
// ) !?UriBytePosition {
//     const tree = self.ast_context.tree;
//     var it = ImportStrIterator.init(tree);
//     while (it.next()) |node| {
//         if (nodeContainsSourceIndex(tree, node, pos_index)) {
//             if (importStr(tree, node)) |import_str| {
//                 if (try self.uriFromImportStrAlloc(arena.allocator(), import_str, zigenv)) |uri| {
//                     // logger.debug("gotoDefinitionString: {s}", .{uri});
//                     return UriBytePosition{ .uri = uri, .loc = .{ .start = 0, .end = 0 } };
//                 }
//             }
//         }
//     }
//     return null;
// }
