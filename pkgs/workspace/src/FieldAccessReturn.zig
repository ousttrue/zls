const std = @import("std");
const Ast = std.zig.Ast;
const Workspace = @import("./Workspace.zig");
const Document = @import("./Document.zig");
const TypeWithHandle = @import("./TypeWithHandle.zig");
const DeclWithHandle = @import("./DeclWithHandle.zig");
const SymbolLookup = @import("./SymbolLookup.zig");
const ast = @import("./ast.zig");
const logger = std.log.scoped(.FieldAccessReturn);

const Self = @This();
original: TypeWithHandle,
unwrapped: ?TypeWithHandle = null,
pub fn getFieldAccessType(
    arena: *std.heap.ArenaAllocator,
    workspace: *Workspace,
    doc: *Document,
    token_idx: u32,
) !?Self {
    var current_type = TypeWithHandle.typeVal(doc, 0);
    var bound_type_params = TypeWithHandle.BoundTypeParams.init(arena.allocator());
    const idx = doc.ast_context.tokens_node[token_idx];
    const token_begin = doc.ast_context.tree.firstToken(doc.ast_context.getRootIdentifier(idx));
    var current = token_begin;
    while (current <= token_idx) : (current += 1) {
        const tok = doc.ast_context.tokens.items[current];
        switch (tok.tag) {
            .identifier => {
                if (try DeclWithHandle.lookupSymbolGlobalTokenIndex(
                    arena,
                    workspace,
                    current_type.handle,
                    current,
                )) |child| {
                    if (try child.resolveType(arena, workspace, &bound_type_params)) |child_type| {
                        current_type = child_type;
                    } else {
                        logger.warn("fail to child.resolveType: {}", .{child.decl});
                        return null;
                    }
                } else {
                    logger.warn("fail to lookupSymbolGlobal: {}", .{tok});
                    return null;
                }
            },
            .period => {
                current += 1;
                const after_period = current;
                const after_period_token = doc.ast_context.tokens.items[after_period];
                switch (after_period_token.tag) {
                    .eof => {
                        // function labels cannot be dot accessed
                        if (current_type.isFunc()) return null;
                        return Self{
                            .original = current_type,
                            .unwrapped = try TypeWithHandle.resolveDerefType(arena, workspace, current_type, &bound_type_params),
                        };
                    },
                    .identifier => {
                        if (after_period == token_idx) {
                            return Self{
                                .original = current_type,
                                .unwrapped = try TypeWithHandle.resolveDerefType(arena, workspace, current_type, &bound_type_params),
                            };
                        }

                        current_type = try TypeWithHandle.resolveFieldAccessLhsType(arena, workspace, current_type, &bound_type_params);
                        const current_type_node = switch (current_type.type.data) {
                            .other => |n| n,
                            else => return null,
                        };

                        var lookup = SymbolLookup.init(arena.allocator());
                        defer lookup.deinit();
                        if (try lookup.lookupSymbolContainer(
                            arena,
                            workspace,
                            current_type.handle,
                            current_type_node,
                            doc.ast_context.getTokenText(doc.ast_context.tokenFromBytePos(after_period_token.loc.start).?.token),
                            !current_type.type.is_type_val,
                        )) |child| {
                            current_type = (try child.resolveType(
                                arena,
                                workspace,
                                &bound_type_params,
                            )) orelse return null;
                        } else return null;
                    },
                    .question_mark => {
                        current_type = (try TypeWithHandle.resolveUnwrapOptionalType(
                            arena,
                            workspace,
                            current_type,
                            &bound_type_params,
                        )) orelse return null;
                    },
                    else => {
                        logger.debug("Unrecognized token {} after period.", .{after_period_token.tag});
                        return null;
                    },
                }
            },
            .period_asterisk => {
                current_type = (try TypeWithHandle.resolveDerefType(
                    arena,
                    workspace,
                    current_type,
                    &bound_type_params,
                )) orelse return null;
            },
            .l_paren => {
                const current_type_node = switch (current_type.type.data) {
                    .other => |n| n,
                    else => return null,
                };

                // Can't call a function type, we need a function type instance.
                if (current_type.type.is_type_val) return null;
                const cur_tree = current_type.handle.ast_context.tree;
                var buf: [1]Ast.Node.Index = undefined;
                if (ast.fnProto(cur_tree, current_type_node, &buf)) |func| {
                    // Check if the function has a body and if so, pass it
                    // so the type can be resolved if it's a generic function returning
                    // an anonymous struct
                    const has_body = cur_tree.nodes.items(.tag)[current_type_node] == .fn_decl;
                    const body = cur_tree.nodes.items(.data)[current_type_node].rhs;

                    // TODO Actually bind params here when calling functions instead of just skipping args.
                    if (try TypeWithHandle.resolveReturnType(arena, workspace, func, current_type.handle, &bound_type_params, if (has_body) body else null)) |ret| {
                        current_type = ret;
                        // Skip to the right paren
                        var paren_count: usize = 1;
                        current += 1;
                        var next = doc.ast_context.tokens.items[current];
                        while (current <= token_idx) : (current += 1) {
                            if (next.tag == .r_paren) {
                                paren_count -= 1;
                                if (paren_count == 0) break;
                            } else if (next.tag == .l_paren) {
                                paren_count += 1;
                            }
                        } else return null;
                    } else return null;
                } else return null;
            },
            .l_bracket => {
                var brack_count: usize = 1;
                current += 1;
                var next = doc.ast_context.tokens.items[current];
                var is_range = false;
                while (next.tag != .eof) : (current += 1) {
                    if (next.tag == .r_bracket) {
                        brack_count -= 1;
                        if (brack_count == 0) break;
                    } else if (next.tag == .l_bracket) {
                        brack_count += 1;
                    } else if (next.tag == .ellipsis2 and brack_count == 1) {
                        is_range = true;
                    }
                } else return null;

                current_type = (try TypeWithHandle.resolveBracketAccessType(arena, workspace, current_type, if (is_range) .Range else .Single, &bound_type_params)) orelse return null;
            },
            else => {
                logger.debug("Unimplemented token: {}", .{tok.tag});
                return null;
            },
        }
    }

    return Self{
        .original = current_type,
        .unwrapped = try TypeWithHandle.resolveDerefType(arena, workspace, current_type, &bound_type_params),
    };
}
