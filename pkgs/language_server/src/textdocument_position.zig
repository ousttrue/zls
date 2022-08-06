const std = @import("std");
const astutil = @import("astutil");
const ws = @import("workspace");
const Config = ws.Config;
const Workspace = ws.Workspace;
const Document = ws.Document;
const UriBytePosition = ws.UriBytePosition;
const DeclWithHandle = ws.DeclWithHandle;
const SymbolLookup = ws.SymbolLookup;
const FixedPath = ws.FixedPath;
const AstToken = astutil.AstToken;
const AstSemantic = astutil.AstSemantic;
const AstIdentifier = astutil.AstIdentifier;
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
    const name = token.getText();
    switch (token.getTag()) {
        .builtin => {
            if (builtin_completions.find(name)) |builtin| {
                return Hover{
                    .text = try std.fmt.allocPrint(
                        allocator,
                        "# builtin: {s}\n\n## hover\n\n```zig\n{s}\n```\n\n{s}",
                        .{ name, builtin.signature, builtin.documentation },
                    ),
                };
            } else {
                return Hover{
                    .text = try std.fmt.allocPrint(
                        allocator,
                        "# builtin: {s}\n\n",
                        .{name},
                    ),
                };
            }
        },
        .identifier => {
            if (AstIdentifier.init(doc.ast_context, token)) |identifier| {
                switch (identifier) {
                    .reference => |reference| {
                        const decl = reference.decl;
                        const text = try decl.allocPrint(allocator);
                        return Hover{
                            .text = text,
                            .loc = decl.token.getRange(),
                        };
                    },
                    .decl => |decl| {
                        const text = try decl.allocPrint(allocator);
                        return Hover{
                            .text = text,
                            .loc = decl.token.getRange(),
                        };
                    },
                }
            } else {
                return null;
            }

            // var lookup = SymbolLookup.init(arena.allocator());
            // defer lookup.deinit();
            // if (lookup.lookupIdentifier(arena, workspace, doc, token)) |decl| {
            //     const hover = try decl.hoverSymbol(arena, workspace, hover_kind);
            //     return try std.fmt.allocPrint(
            //         allocator,
            //         "# {s}\n\n{s}\n\n## hover\n\n{?s}",
            //         .{ name, context_info, hover },
            //     );
            // } else {
            //     return try std.fmt.allocPrint(
            //         allocator,
            //         "# {s}\n\n{s}\n\n",
            //         .{ name, context_info },
            //     );
            // }
        },
        else => {
            return Hover{
                .text = try std.fmt.allocPrint(
                    allocator,
                    "# {s}: {s}\n\n",
                    .{ @tagName(token.getTag()), name },
                ),
            };
        },
    }
}

pub fn getRename(
    arena: *std.heap.ArenaAllocator,
    workspace: *Workspace,
    doc: *Document,
    token: AstToken,
) !?[]const UriBytePosition {
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
    resolve_alias: bool,
) !?UriBytePosition {
    switch (token.getTag()) {
        .string_literal => {
            const text = token.getText();
            if (text.len > 2) {
                const path = try workspace.resolveImportPath(doc, text[1 .. text.len - 1]);
                return UriBytePosition{ .path = path, .loc = .{ .start = 0, .end = 0 } };
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

    var lookup = SymbolLookup.init(arena.allocator());
    defer lookup.deinit();
    const decl = lookup.lookupIdentifier(arena, workspace, doc, token) orelse {
        return null;
    };

    return decl.gotoDefinitionSymbol(workspace, arena, resolve_alias);
}

pub fn getRenferences(
    arena: *std.heap.ArenaAllocator,
    workspace: *Workspace,
    doc: *Document,
    token: AstToken,
    include_decl: bool,
    config: *Config,
) !?[]UriBytePosition {
    if (token.getTag() != .identifier) {
        return null;
    }

    var lookup = SymbolLookup.init(arena.allocator());
    defer lookup.deinit();
    const decl = lookup.lookupIdentifier(arena, workspace, doc, token) orelse {
        return null;
    };

    var locs = std.ArrayList(UriBytePosition).init(arena.allocator());
    try decl.symbolReferences(arena, workspace, include_decl, &locs, config.skip_std_references);
    return locs.items;
}
