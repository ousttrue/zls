const std = @import("std");
const Ast = std.zig.Ast;
const Workspace = @import("./Workspace.zig");
const Document = @import("./Document.zig");
const TypeWithHandle = @import("./TypeWithHandle.zig");
const DeclWithHandle = @import("./DeclWithHandle.zig");
const ast = @import("./ast.zig");
const logger = std.log.scoped(.FieldAccessReturn);
const Self = @This();

original: TypeWithHandle,
unwrapped: ?TypeWithHandle = null,

pub fn getFieldAccessType(
    arena: *std.heap.ArenaAllocator,
    workspace: *Workspace,
    handle: *Document,
    source_index: usize,
    tokenizer: *std.zig.Tokenizer,
) !?Self {
    var current_type = TypeWithHandle.typeVal(handle, undefined);

    var bound_type_params = TypeWithHandle.BoundTypeParams.init(arena.allocator());

    while (true) {
        const tok = tokenizer.next();
        switch (tok.tag) {
            .eof => return Self{
                .original = current_type,
                .unwrapped = try TypeWithHandle.resolveDerefType(arena, workspace, current_type, &bound_type_params),
            },
            .identifier => {
                if (try DeclWithHandle.lookupSymbolGlobal(
                    arena,
                    workspace,
                    current_type.handle,
                    tokenizer.buffer[tok.loc.start..tok.loc.end],
                    source_index,
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
                const after_period = tokenizer.next();
                switch (after_period.tag) {
                    .eof => {
                        // function labels cannot be dot accessed
                        if (current_type.isFunc()) return null;
                        return Self{
                            .original = current_type,
                            .unwrapped = try TypeWithHandle.resolveDerefType(arena, workspace, current_type, &bound_type_params),
                        };
                    },
                    .identifier => {
                        if (after_period.loc.end == tokenizer.buffer.len) {
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

                        if (try DeclWithHandle.lookupSymbolContainer(
                            arena,
                            workspace,
                            current_type.handle,
                            current_type_node,
                            tokenizer.buffer[after_period.loc.start..after_period.loc.end],
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
                        logger.debug("Unrecognized token {} after period.", .{after_period.tag});
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
                const cur_tree = current_type.handle.tree;
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
                        var next = tokenizer.next();
                        while (next.tag != .eof) : (next = tokenizer.next()) {
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
                var next = tokenizer.next();
                var is_range = false;
                while (next.tag != .eof) : (next = tokenizer.next()) {
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
        .unwrapped = try TypeWithHandle.resolveDerefType(arena, current_type, &bound_type_params),
    };
}
