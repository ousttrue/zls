const std = @import("std");
const ws = @import("workspace");
const Workspace = ws.Workspace;
const Document = ws.Document;
const DeclWithHandle = ws.DeclWithHandle;
const UriBytePosition = ws.UriBytePosition;

pub fn gotoHandler(
    arena: *std.heap.ArenaAllocator,
    workspace: *Workspace,
    doc: *Document,
    byte_position: u32,
    resolve_alias: bool,
) !?UriBytePosition {
    const token_with_index = doc.ast_context.tokenFromBytePos(byte_position) orelse {
        // token not found. return no hover.
        return null;
    };

    const pos_context = doc.ast_context.getPositionContext(byte_position);
    switch (pos_context) {
        .var_access => {
            if (try DeclWithHandle.lookupSymbolGlobalTokenIndex(arena, workspace, doc, token_with_index.index)) |decl| {
                return decl.gotoDefinitionSymbol(workspace, arena, resolve_alias);
            } else {
                return null;
            }
        },
        .field_access => |_| {
            const decl = try DeclWithHandle.getSymbolFieldAccess(arena, workspace, doc, byte_position);
            return decl.gotoDefinitionSymbol(workspace, arena, resolve_alias);
        },
        .string_literal => {
            return doc.gotoDefinitionString(arena, byte_position, workspace.zigenv);
        },
        .label => {
            // return self.gotoDefinitionLabel(arena, doc, byte_position);
            if (try DeclWithHandle.lookupLabel(doc, byte_position)) |decl| {
                return decl.gotoDefinitionSymbol(workspace, arena, false);
            } else {
                return null;
            }
        },
        else => {
            // logger.debug("PositionContext.{s} is not implemented", .{@tagName(pos_context)});
            return null;
        },
    }
}
