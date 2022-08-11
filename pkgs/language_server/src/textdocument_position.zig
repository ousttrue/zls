const std = @import("std");
const astutil = @import("astutil");
const lsp = @import("lsp");
const Project = astutil.Project;
const Document = astutil.Document;
const PathPosition = astutil.PathPosition;
const FixedPath = astutil.FixedPath;
const AstToken = astutil.AstToken;
const AstNode = astutil.AstNode;
const Declaration = astutil.Declaration;
const Line = astutil.Line;
const FunctionSignature = astutil.FunctionSignature;
const ImportSolver = astutil.ImportSolver;
const builtin_completions = @import("./builtin_completions.zig");
const logger = std.log.scoped(.textdocument_position);

pub const Hover = struct {
    text: []const u8,
    loc: ?std.zig.Token.Loc = null,
};

pub fn getHover(
    arena: *std.heap.ArenaAllocator,
    project: ?Project,
    doc: *Document,
    token: AstToken,
) !?Hover {
    _ = project;
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
                    if (Declaration.find(node)) |decl| {
                        const text = try decl.allocPrint(allocator);
                        try w.print("{s}", .{text});
                        switch (decl) {
                            .local => |local| {
                                return Hover{
                                    .text = text_buffer.items,
                                    .loc = local.name_token.getLoc(),
                                };
                            },
                            .container => |container| {
                                return Hover{
                                    .text = text_buffer.items,
                                    .loc = container.name_token.getLoc(),
                                };
                            },
                            .primitive => {},
                        }
                    } else {
                        logger.debug("identifier: {s}: decl not found", .{token.getText()});
                    }
                },
                else => {
                    // const var_type = try VarType.init(project, node);
                    // const text = try var_type.allocPrint(allocator);
                    // try w.print("var_type: {s}", .{text});
                    // return Hover{
                    //     .text = text_buffer.items,
                    // };
                },
            }
        },
        else => {},
    }

    return Hover{
        .text = text_buffer.items,
    };
}

// pub fn getRename(
//     arena: *std.heap.ArenaAllocator,
//     workspace: *Workspace,
//     doc: *Document,
//     token: AstToken,
// ) !?[]const PathPosition {
//     if (token.getTag() != .identifier) {
//         return null;
//     }

//     var lookup = SymbolLookup.init(arena.allocator());
//     defer lookup.deinit();
//     const decl = lookup.lookupIdentifier(arena, workspace, doc, token) orelse {
//         return null;
//     };

//     return try decl.renameSymbol(arena, workspace);
// }

fn gotoImport(project: ?Project, import_from: FixedPath, text: []const u8) ?PathPosition {
    if (project) |p| {
        if (text.len > 2) {
            if (p.import_solver.solve(import_from, text)) |path| {
                return PathPosition{ .path = path, .loc = .{ .start = 0, .end = 0 } };
            }
        }
    }
    return null;
}

pub fn getGoto(
    arena: *std.heap.ArenaAllocator,
    project: Project,
    doc: *Document,
    token: AstToken,
) !?PathPosition {
    _ = arena;
    const node = AstNode.fromTokenIndex(doc.ast_context, token.index);

    switch (token.getTag()) {
        .string_literal => {
            // goto import file
            return gotoImport(project, doc.path, token.getText());
        },
        .builtin => {
            if (std.mem.eql(u8, token.getText(), "@import")) {
                var buf: [2]u32 = undefined;
                switch (node.getChildren(&buf)) {
                    .builtin_call => |full| {
                        const param_node = AstNode.init(node.context, full.ast.params[0]);
                        return gotoImport(project, doc.path, param_node.getMainToken().getText());
                    },
                    else => {},
                }
            }
            return null;
        },
        .identifier => {
            var buf: [2]u32 = undefined;
            switch (node.getChildren(&buf)) {
                .var_decl => |var_decl| {
                    // to rhs
                    const init_node = AstNode.init(node.context, var_decl.ast.init_node);
                    // TODO: GetType
                    return PathPosition{ .path = doc.path, .loc = init_node.getMainToken().getLoc() };
                },
                .fn_proto => {
                    return null;
                },
                else => {
                    switch (node.getTag()) {
                        .identifier => {
                            if (Declaration.find(node)) |decl| {
                                const text = try decl.allocPrint(arena.allocator());
                                logger.debug("{s}", .{text});
                                switch (decl) {
                                    .local => |local| {
                                        return PathPosition{ .path = doc.path, .loc = local.name_token.getLoc() };
                                    },
                                    .container => |container| {
                                        return PathPosition{ .path = doc.path, .loc = container.name_token.getLoc() };
                                    },
                                    .primitive => {
                                        return error.Primitive;
                                    },
                                }
                            } else {
                                return error.DeclNotFound;
                            }
                        },
                        .field_access => {
                            const type_node = try project.resolveFieldAccess(node);
                            return type_node.getPosition();
                        },
                        .fn_decl => {
                            return null;
                        },
                        else => {
                            logger.debug("getGoto: unknown node tag: {s}", .{@tagName(node.getTag())});
                            return null;
                        },
                    }
                },
            }
        },
        else => {
            return null;
        },
    }
}

// pub fn getRenferences(
//     arena: *std.heap.ArenaAllocator,
//     workspace: *Workspace,
//     doc: *Document,
//     token: AstToken,
//     include_decl: bool,
// ) !?[]PathPosition {
//     if (token.getTag() != .identifier) {
//         return null;
//     }

//     var lookup = SymbolLookup.init(arena.allocator());
//     defer lookup.deinit();
//     const decl = lookup.lookupIdentifier(arena, workspace, doc, token) orelse {
//         return null;
//     };

//     var locs = std.ArrayList(PathPosition).init(arena.allocator());
//     try decl.symbolReferences(arena, workspace, include_decl, &locs);
//     return locs.items;
// }

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

pub fn completeContainerMember(
    arena: *std.heap.ArenaAllocator,
    project: Project,
    doc: *Document,
    token: AstToken,
) ![]const lsp.completion.CompletionItem {
    _ = project;
    _ = doc;
    _ = token;
    var items = std.ArrayList(lsp.completion.CompletionItem).init(arena.allocator());

    // const node = AstNode.fromTokenIndex(doc.ast_context, token.index);
    // const var_type = try VarType.init(project, node);

    // if (try var_type.getContainerNode(project)) |container_node| {
    //     var buf: [2]u32 = undefined;
    //     if (container_node.containerIterator(&buf)) |*it| {
    //         while (it.next()) |member| {
    //             if (member.getMemberNameToken()) |name_token| {
    //                 var buf2: [2]u32 = undefined;

    //                 try items.append(.{
    //                     .label = name_token.getText(),
    //                     .kind = switch (member.getChildren(&buf2)) {
    //                         .var_decl => .Value,
    //                         .container_field => .Property,
    //                         else => .Method,
    //                     },
    //                 });
    //             } else {}
    //         }
    //     }
    // } else {
    //     logger.err("no container: {s}", .{try token.allocPrint(arena.allocator())});
    // }

    return items.toOwnedSlice();
}

pub fn getCompletion(
    arena: *std.heap.ArenaAllocator,
    project: Project,
    doc: *Document,
    trigger_character: ?[]const u8,
    byte_position: u32,
    encoding: Line.Encoding,
) ![]const lsp.completion.CompletionItem {
    // TODO:
    // * trigger
    // * not trigger input head(no token under cursor)
    // * not trigger input not head(token under cursor)

    // get token for completion context
    //
    // std.
    //     ^ cursor is here. no token. get previous token
    const token_with_index = doc.ast_context.prevTokenFromBytePos(byte_position) orelse {
        // token not found. return no hover.
        return error.NoTokenUnderCursor;
    };
    const token = AstToken.init(&doc.ast_context.tree, token_with_index.index);

    if (trigger_character) |trigger| {
        if (std.mem.eql(u8, trigger, ".")) {
            logger.debug("trigger '.' => field_access", .{});
            if (token.getPrev()) |prev| {
                return try completeContainerMember(arena, project, doc, prev);
            }
        } else if (std.mem.eql(u8, trigger, "@")) {
            // logger.debug("trigger '@' => builtin", .{});
            return builtin_completions.completeBuiltin();
        } else {
            logger.debug("trigger '{s}'", .{trigger});
            return &[_]lsp.completion.CompletionItem{};
        }
    } else {
        switch (token.getTag()) {
            .period => {
                if (token.getPrev()) |prev| {
                    return try completeContainerMember(arena, project, doc, prev);
                }
            },
            .string_literal => {
                const prev = token.getPrev().?; // lparen
                const prev_prev = prev.getPrev().?; //
                if (std.mem.eql(u8, prev_prev.getText(), "@import")) {
                    return try completeImport(arena, project.import_solver, doc, token, encoding);
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

/// triggerd
///
/// @import()
///         ^ r_paren
pub fn getSignature(
    arena: *std.heap.ArenaAllocator,
    project: Project,
    doc: *Document,
    token: AstToken,
) !?FunctionSignature {
    _ = project;

    const node = AstNode.fromTokenIndex(doc.ast_context, token.index);
    var buf: [2]u32 = undefined;
    switch (node.getChildren(&buf)) {
        .call => {
            logger.debug("call", .{});
        },
        .builtin_call => |full| {
            const name = node.getMainToken().getText();
            for (builtin_completions.data()) |b| {
                if (std.mem.eql(u8, b.name, name)) {
                    var fs = FunctionSignature.init(
                        arena.allocator(),
                        b.signature,
                        b.documentation,
                        @intCast(u32, full.ast.params.len),
                    );
                    for (b.arguments) |arg| {
                        if (std.mem.indexOf(u8, arg, ":")) |found| {
                            try fs.args.append(.{
                                .name = arg[0..found],
                                .document = arg[found + 1 ..],
                            });
                        } else {
                            try fs.args.append(.{
                                .name = arg,
                                .document = arg,
                            });
                        }
                    }
                    return fs;
                }
            }
            logger.err("builtin {s} not found", .{name});
        },
        else => {
            logger.debug("getSignature: not function call: {s}", .{try node.allocPrint(arena.allocator())});
            return null;
        },
    }

    return null;
}
