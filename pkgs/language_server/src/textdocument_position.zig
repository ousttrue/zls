const std = @import("std");
const astutil = @import("astutil");
const ws = @import("workspace");
const Config = ws.Config;
const Workspace = ws.Workspace;
const Document = astutil.Document;
const PathPosition = astutil.PathPosition;
const FixedPath = astutil.FixedPath;
const AstToken = astutil.AstToken;
const AstNode = astutil.AstNode;
const Declaration = astutil.Declaration;
const VarType = astutil.VarType;
const Project = astutil.Project;
const ast = ws.ast;
const builtin_completions = ws.builtin_completions;
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
                        const text = try decl.allocPrint(allocator, project);
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
                    const var_type = try VarType.init(project, node);
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
            if (std.mem.endsWith(u8, text, ".zig")) {
                if (p.import_solver.solve(import_from, .{ .file = text })) |path| {
                    return PathPosition{ .path = path, .loc = .{ .start = 0, .end = 0 } };
                }
            } else {
                if (p.import_solver.solve(import_from, .{ .pkg = text })) |path| {
                    return PathPosition{ .path = path, .loc = .{ .start = 0, .end = 0 } };
                }
            }
        }
    }
    return null;
}

pub fn getGoto(
    arena: *std.heap.ArenaAllocator,
    project: ?Project,
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
                            if (Declaration.findFromBlock(node)) |decl| {
                                const text = try decl.allocPrint(arena.allocator(), project);
                                logger.debug("local => {s}", .{text});
                                // local variable
                                return PathPosition{ .path = doc.path, .loc = decl.token.getLoc() };
                            } else if (Declaration.findFromContainer(node)) |decl| {
                                // container variable
                                return PathPosition{ .path = doc.path, .loc = decl.token.getLoc() };
                            } else {
                                return error.DeclNotFound;
                            }
                        },
                        .field_access => {
                            var data = node.getData();
                            var lhs = AstNode.init(node.context, data.lhs);
                            var var_type = try VarType.init(project, lhs);
                            // rhs
                            var rhs = AstToken.init(&node.context.tree, data.rhs);
                            if (try var_type.getMember(project, rhs.getText())) |member| {
                                return PathPosition{ .path = doc.path, .loc = member.getMainToken().getLoc() };
                            } else {
                                logger.debug("not member", .{});
                                return null;
                            }
                        },
                        .fn_decl => {
                            return null;
                        },
                        else => {
                            logger.debug("unknown node tag: {s}", .{@tagName(node.getTag())});
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
