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
                        "",
                    );
                    fs.param_count = @intCast(u32, full.ast.params.len);
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
