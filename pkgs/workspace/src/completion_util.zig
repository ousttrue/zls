const std = @import("std");
const astutil = @import("astutil");
const lsp = @import("lsp");
const Document = astutil.Document;
const Line = astutil.Line;
const AstToken = astutil.AstToken;
const ImportSolver = astutil.ImportSolver;
const Workspace = @import("./Workspace.zig");
const builtin_completions = @import("./builtin_completions.zig");
const logger = std.log.scoped(.Completion);

fn tokenToRange(doc: *Document, loc: std.zig.Token.Loc, encoding: Line.Encoding) !lsp.Range {
    const start = try doc.utf8_buffer.getPositionFromBytePosition(loc.start, encoding);
    const end = try doc.utf8_buffer.getPositionFromBytePosition(loc.end, encoding);
    return lsp.Range{
        .start = .{
            .line = start.line,
            .character = start.x,
        },
        .end = .{
            .line = end.line,
            .character = end.x,
        },
    };
}

fn completeImport(
    arena: *std.heap.ArenaAllocator,
    import_solver: ImportSolver,
    doc: *Document,
    token: AstToken,
    encoding: Line.Encoding,
) ![]lsp.completion.CompletionItem {
    var items = std.ArrayList(lsp.completion.CompletionItem).init(arena.allocator());
    const loc = token.getLoc();
    const text = token.getText();
    var range = try tokenToRange(doc, .{ .start = loc.start + 1, .end = loc.end - 1 }, encoding);
    _ = range;

    try items.append(.{
        .label = "std",
        .kind = .Text,
    });
    try items.append(.{
        .label = "builtin",
        .kind = .Text,
    });

    {
        // pckages
        var it = import_solver.pkg_path_map.keyIterator();
        while (it.next()) |key| {
            const copy = try std.fmt.allocPrint(arena.allocator(), "{s}", .{key.*});
            // logger.debug("pkg: {s} => {s}", .{ token.getText(), copy });
            try items.append(.{
                .label = copy,
                .kind = .Module,
                // .textEdit = .{
                //     .range = range,
                //     .newText = copy,
                // },
                // .filterText=,
                // .insertText = try std.fmt.allocPrint(arena.allocator(), "\"{s}\"", .{key.*}),
                // .insertTextFormat=,
                // .detail=,
                // .documentation=,
            });
        }
    }

    if (std.mem.startsWith(u8, text, "\"./")) {
        // current path
        const dir = doc.path.parent().?;
        var it = try dir.iterateChildren();
        defer it.deinit();
        while (try it.next()) |entry| {
            switch (entry.kind) {
                .File => {
                    if (std.mem.endsWith(u8, entry.name, ".zig")) {
                        const copy = try std.fmt.allocPrint(arena.allocator(), "{s}", .{entry.name});
                        // logger.debug("path: {s} => {s}", .{ token.getText(), copy });
                        try items.append(.{
                            .label = copy,
                            .kind = .File,
                            // .textEdit = .{
                            //     .range = range,
                            //     .newText = copy,
                            // },
                            // .filterText=,
                            // .insertText = copy[partial.len..], //
                            // .insertTextFormat=,
                            // .detail=,
                            // .documentation=,
                        });
                    }
                },
                else => {},
            }
        }
    }

    return items.toOwnedSlice();
}

pub fn process(
    arena: *std.heap.ArenaAllocator,
    workspace: *Workspace,
    doc: *Document,
    trigger_character: ?[]const u8,
    token: AstToken,
    encoding: Line.Encoding,
) ![]const lsp.completion.CompletionItem {
    if (trigger_character) |trigger| {
        // if (std.mem.eql(u8, trigger, ".")) {
        //     logger.debug("trigger '.' => field_access", .{});
        //     return try completeFieldAccess(arena, workspace, doc, token, config, doc_kind);
        // } else
        if (std.mem.eql(u8, trigger, "@")) {
            // logger.debug("trigger '@' => builtin", .{});
            return builtin_completions.completeBuiltin();
        } else {
            logger.debug("trigger '{s}'", .{trigger});
            return &[_]lsp.completion.CompletionItem{};
        }
    } else {
        switch (token.getTag()) {
            // .period => {
            //     // completeFieldAccess(arena, workspace, doc, token_with_index.index, config, doc_kind);
            //     // const idx = doc.ast_context.tokens_node[token_with_index.index];
            //     // const tag = doc.ast_context.tree.nodes.items(.tag);
            //     // const node_tag = tag[idx];
            //     // switch(node_tag)
            //     // {
            //     //     .field_access =>{
            //     //         return try completeFieldAccess(arena, workspace, doc, token_with_index.index, config, doc_kind);
            //     //     },
            //     //     else =>{
            //     //         return try completeGlobal(arena, workspace, byte_position, doc, config, doc_kind);
            //     //     },
            //     // }
            //     return try completeFieldAccess(arena, workspace, doc, token, config, doc_kind);
            // },
            .string_literal => {
                const prev = token.getPrev(); // lparen
                const prev_prev = prev.getPrev(); //
                if (std.mem.eql(u8, prev_prev.getText(), "@import")) {
                    return try completeImport(arena, workspace.import_solver, doc, token, encoding);
                }
                // return try completeGlobal(arena, workspace, token.getStart(), doc, config, doc_kind);
            },
            else => {
                // const node = AstNode.fromTokenIndex(doc.ast_context, token.index);
                // return try completeGlobal(arena, workspace, token.getStart(), doc, config, doc_kind);
            },
        }
    }
    return &[_]lsp.completion.CompletionItem{};
}
