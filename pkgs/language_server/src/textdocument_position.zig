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




