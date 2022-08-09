const std = @import("std");
const astutil = @import("astutil");
const Ast = std.zig.Ast;
const Workspace = @import("./Workspace.zig");
const Document = astutil.Document;
const ast = astutil.ast;
const AstToken = astutil.AstToken;
pub const BoundTypeParams = std.AutoHashMap(Ast.full.FnProto.Param, Self);
const DeclWithHandle = @import("./DeclWithHandle.zig");
const SymbolLookup = @import("./SymbolLookup.zig");
const logger = std.log.scoped(.TypeWithHandle);

pub fn getDeclName(tree: *const Ast, node: Ast.Node.Index) ?[]const u8 {
    const name = tree.tokenSlice(ast.getDeclNameToken(tree, node) orelse return null);
    return switch (tree.nodes.items(.tag)[node]) {
        .test_decl => name[1 .. name.len - 1],
        else => name,
    };
}

fn isBlock(tree: *const Ast, node: Ast.Node.Index) bool {
    return switch (tree.nodes.items(.tag)[node]) {
        .block,
        .block_semicolon,
        .block_two,
        .block_two_semicolon,
        => true,
        else => false,
    };
}

fn findReturnStatementInternal(tree: *const Ast, fn_decl: Ast.full.FnProto, body: Ast.Node.Index, already_found: *bool) ?Ast.Node.Index {
    var result: ?Ast.Node.Index = null;

    const node_tags = tree.nodes.items(.tag);
    const datas = tree.nodes.items(.data);

    if (!isBlock(tree, body)) return null;

    const statements: []const Ast.Node.Index = switch (node_tags[body]) {
        .block, .block_semicolon => tree.extra_data[datas[body].lhs..datas[body].rhs],
        .block_two, .block_two_semicolon => blk: {
            const statements = &[_]Ast.Node.Index{ datas[body].lhs, datas[body].rhs };
            const len: usize = if (datas[body].lhs == 0)
                @as(usize, 0)
            else if (datas[body].rhs == 0)
                @as(usize, 1)
            else
                @as(usize, 2);
            break :blk statements[0..len];
        },
        else => unreachable,
    };

    for (statements) |child_idx| {
        if (node_tags[child_idx] == .@"return") {
            if (datas[child_idx].lhs != 0) {
                const lhs = datas[child_idx].lhs;
                if (ast.isCall(tree, lhs)) {
                    const call_name = getDeclName(tree, datas[lhs].lhs);
                    if (call_name) |name| {
                        if (std.mem.eql(u8, name, tree.tokenSlice(fn_decl.name_token.?))) {
                            continue;
                        }
                    }
                }
            }

            if (already_found.*) return null;
            already_found.* = true;
            result = child_idx;
            continue;
        }

        result = findReturnStatementInternal(tree, fn_decl, child_idx, already_found);
    }

    return result;
}

pub fn findReturnStatement(tree: *const Ast, fn_decl: Ast.full.FnProto, body: Ast.Node.Index) ?Ast.Node.Index {
    var already_found = false;
    return findReturnStatementInternal(tree, fn_decl, body, &already_found);
}

/// The node is the meta-type `type`
pub fn isMetaType(tree: *const Ast, node: Ast.Node.Index) bool {
    if (tree.nodes.items(.tag)[node] == .identifier) {
        return std.mem.eql(u8, tree.tokenSlice(tree.nodes.items(.main_token)[node]), "type");
    }
    return false;
}

pub fn isTypeFunction(tree: *const Ast, func: Ast.full.FnProto) bool {
    return isMetaType(tree, func.ast.return_type);
}

pub fn isGenericFunction(tree: *const Ast, func: Ast.full.FnProto) bool {
    var it = func.iterate(&tree);
    while (it.next()) |param| {
        if (param.anytype_ellipsis3 != null or param.comptime_noalias != null) {
            return true;
        }
    }
    return false;
}

fn allDigits(str: []const u8) bool {
    for (str) |c| {
        if (!std.ascii.isDigit(c)) return false;
    }
    return true;
}

pub fn isTypeIdent(tree: Ast, token_idx: Ast.TokenIndex) bool {
    const PrimitiveTypes = std.ComptimeStringMap(void, .{
        .{"isize"},          .{"usize"},
        .{"c_short"},        .{"c_ushort"},
        .{"c_int"},          .{"c_uint"},
        .{"c_long"},         .{"c_ulong"},
        .{"c_longlong"},     .{"c_ulonglong"},
        .{"c_longdouble"},   .{"anyopaque"},
        .{"f16"},            .{"f32"},
        .{"f64"},            .{"f128"},
        .{"bool"},           .{"void"},
        .{"noreturn"},       .{"type"},
        .{"anyerror"},       .{"comptime_int"},
        .{"comptime_float"}, .{"anyframe"},
        .{"anytype"},
    });

    const text = tree.tokenSlice(token_idx);
    if (PrimitiveTypes.has(text)) return true;
    if (text.len == 1) return false;
    if (!(text[0] == 'u' or text[0] == 'i')) return false;
    if (!allDigits(text[1..])) return false;
    _ = std.fmt.parseUnsigned(u16, text[1..], 10) catch return false;
    return true;
}

pub fn isPtrType(tree: *const Ast, node: Ast.Node.Index) bool {
    return switch (tree.nodes.items(.tag)[node]) {
        .ptr_type,
        .ptr_type_aligned,
        .ptr_type_bit_range,
        .ptr_type_sentinel,
        => true,
        else => false,
    };
}

const Self = @This();
pub const Type = struct {
    data: union(enum) {
        pointer: Ast.Node.Index,
        slice: Ast.Node.Index,
        error_union: Ast.Node.Index,
        other: Ast.Node.Index,
        primitive,
    },
    /// If true, the type `type`, the attached data is the value of the type value.
    is_type_val: bool,
};

type: Type,
handle: *Document,

pub fn typeVal(handle: *Document, node: Ast.Node.Index) Self {
    const tree = handle.ast_context.tree;
    std.debug.assert(node < tree.nodes.len);

    return .{
        .type = .{
            .data = .{ .other = node },
            .is_type_val = true,
        },
        .handle = handle,
    };
}

pub fn notTypeVal(handle: *Document, node: Ast.Node.Index) Self {
    const tree = handle.ast_context.tree;
    std.debug.assert(node < tree.nodes.len);

    return .{
        .type = .{
            .data = .{ .other = node },
            .is_type_val = false,
        },
        .handle = handle,
    };
}

pub fn instanceTypeVal(self: Self) ?Self {
    if (!self.type.is_type_val) return null;
    return Self{
        .type = .{ .data = self.type.data, .is_type_val = false },
        .handle = self.handle,
    };
}

const NodeWithHandle = std.meta.Tuple(&.{ *Document, Ast.Node.Index });
var resolve_trail: std.ArrayList(NodeWithHandle) = undefined;
pub fn init(allocator: std.mem.Allocator) void {
    resolve_trail = std.ArrayList(NodeWithHandle).init(allocator);
}
pub fn deinit() void {
    resolve_trail.deinit();
}

pub fn innermostContainer(workspace: *Workspace, handle: *Document, source_index: usize) Self {
    var current = workspace.handles.get(handle).?.scopes.items[0].data.container;
    if (workspace.handles.get(handle).?.scopes.items.len == 1) return Self.typeVal(handle, current);

    for (workspace.handles.get(handle).?.scopes.items[1..]) |scope| {
        if (source_index >= scope.range.start and source_index <= scope.range.end) {
            switch (scope.data) {
                .container => |node| current = node,
                else => {},
            }
        }
        if (scope.range.start > source_index) break;
    }
    return Self.typeVal(handle, current);
}

pub fn resolveTypeOfNode(
    arena: *std.heap.ArenaAllocator,
    workspace: *Workspace,
    handle: *Document,
    node: Ast.Node.Index,
) ?Self {
    var bound_type_params = BoundTypeParams.init(arena.allocator());
    return resolveTypeOfNodeInternal(arena, workspace, handle, node, &bound_type_params);
}

pub fn hasSelfParam(arena: *std.heap.ArenaAllocator, workspace: *Workspace, handle: *Document, func: Ast.full.FnProto) !bool {
    // Non-decl prototypes cannot have a self parameter.
    if (func.name_token == null) return false;
    if (func.ast.params.len == 0) return false;

    const tree = &handle.ast_context.tree;
    var it = func.iterate(tree);
    const param = it.next().?;
    if (param.type_expr == 0) return false;

    const token_starts = tree.tokens.items(.start);
    const token_data = tree.nodes.items(.data);
    const in_container = innermostContainer(workspace, handle, token_starts[func.ast.fn_token]);

    if (resolveTypeOfNode(arena, workspace, handle, param.type_expr)) |resolved_type| {
        if (std.meta.eql(in_container, resolved_type))
            return true;
    }

    if (isPtrType(tree, param.type_expr)) {
        if (resolveTypeOfNode(
            arena,
            workspace,
            handle,
            token_data[param.type_expr].rhs,
        )) |resolved_prefix_op| {
            if (std.meta.eql(in_container, resolved_prefix_op))
                return true;
        }
    }
    return false;
}

pub fn resolveReturnType(arena: *std.heap.ArenaAllocator, workspace: *Workspace, fn_decl: Ast.full.FnProto, handle: *Document, bound_type_params: *BoundTypeParams, fn_body: ?Ast.Node.Index) !?Self {
    const tree = &handle.ast_context.tree;
    if (Self.isTypeFunction(tree, fn_decl) and fn_body != null) {
        // If this is a type function and it only contains a single return statement that returns
        // a container declaration, we will return that declaration.
        const ret = findReturnStatement(tree, fn_decl, fn_body.?) orelse return null;
        const data = tree.nodes.items(.data)[ret];
        if (data.lhs != 0) {
            return resolveTypeOfNodeInternal(arena, workspace, handle, data.lhs, bound_type_params);
        }

        return null;
    }

    if (fn_decl.ast.return_type == 0) return null;
    const return_type = fn_decl.ast.return_type;
    const child_type = resolveTypeOfNodeInternal(arena, workspace, handle, return_type, bound_type_params) orelse
        return null;

    const is_inferred_error = tree.tokens.items(.tag)[tree.firstToken(return_type) - 1] == .bang;
    if (is_inferred_error) {
        const child_type_node = switch (child_type.type.data) {
            .other => |n| n,
            else => return null,
        };
        return Self{
            .type = .{ .data = .{ .error_union = child_type_node }, .is_type_val = false },
            .handle = child_type.handle,
        };
    } else return child_type.instanceTypeVal();
}

/// Resolves slicing and array access
pub fn resolveBracketAccessType(arena: *std.heap.ArenaAllocator, workspace: *Workspace, lhs: Self, rhs: enum { Single, Range }, bound_type_params: *BoundTypeParams) !?Self {
    const lhs_node = switch (lhs.type.data) {
        .other => |n| n,
        else => return null,
    };

    const tree = &lhs.handle.ast_context.tree;
    const tags = tree.nodes.items(.tag);
    const tag = tags[lhs_node];
    const data = tree.nodes.items(.data)[lhs_node];

    if (tag == .array_type or tag == .array_type_sentinel) {
        if (rhs == .Single)
            return (resolveTypeOfNodeInternal(
                arena,
                workspace,
                lhs.handle,
                data.rhs,
                bound_type_params,
            ) orelse return null).instanceTypeVal();
        return Self{
            .type = .{ .data = .{ .slice = data.rhs }, .is_type_val = false },
            .handle = lhs.handle,
        };
    } else if (ast.ptrType(tree, lhs_node)) |ptr_type| {
        if (ptr_type.size == .Slice) {
            if (rhs == .Single) {
                return (resolveTypeOfNodeInternal(
                    arena,
                    workspace,
                    lhs.handle,
                    ptr_type.ast.child_type,
                    bound_type_params,
                ) orelse return null).instanceTypeVal();
            }
            return lhs;
        }
    }

    return null;
}

/// Resolves the child type of an optional type
pub fn resolveUnwrapOptionalType(
    arena: *std.heap.ArenaAllocator,
    workspace: *Workspace,
    opt: Self,
    bound_type_params: *BoundTypeParams,
) ?Self {
    const opt_node = switch (opt.type.data) {
        .other => |n| n,
        else => return null,
    };

    if (opt.handle.ast_context.tree.nodes.items(.tag)[opt_node] == .optional_type) {
        return ((resolveTypeOfNodeInternal(
            arena,
            workspace,
            opt.handle,
            opt.handle.ast_context.tree.nodes.items(.data)[opt_node].lhs,
            bound_type_params,
        )) orelse return null).instanceTypeVal();
    }

    return null;
}

pub fn resolveUnwrapErrorType(arena: *std.heap.ArenaAllocator, workspace: *Workspace, rhs: Self, bound_type_params: *BoundTypeParams) !?Self {
    const rhs_node = switch (rhs.type.data) {
        .other => |n| n,
        .error_union => |n| return Self{
            .type = .{ .data = .{ .other = n }, .is_type_val = rhs.type.is_type_val },
            .handle = rhs.handle,
        },
        .primitive, .slice, .pointer => return null,
    };

    if (rhs.handle.ast_context.tree.nodes.items(.tag)[rhs_node] == .error_union) {
        return (resolveTypeOfNodeInternal(
            arena,
            workspace,
            rhs.handle,
            rhs.handle.ast_context.tree.nodes.items(.data)[rhs_node].rhs,
            bound_type_params,
        ) orelse return null).instanceTypeVal();
    }

    return null;
}

/// Called to remove one level of pointerness before a field access
pub fn resolveFieldAccessLhsType(arena: *std.heap.ArenaAllocator, workspace: *Workspace, lhs: Self, bound_type_params: *BoundTypeParams) !Self {
    return (try resolveDerefType(arena, workspace, lhs, bound_type_params)) orelse lhs;
}

/// Resolves the type of a node
pub fn resolveTypeOfNodeInternal(
    arena: *std.heap.ArenaAllocator,
    workspace: *Workspace,
    doc: *Document,
    node: Ast.Node.Index,
    bound_type_params: *BoundTypeParams,
) ?Self {
    // If we were asked to resolve this node before,
    // it is self-referential and we cannot resolve it.
    for (resolve_trail.items) |i| {
        if (std.meta.eql(i, NodeWithHandle{ doc, node }))
            return null;
    }
    resolve_trail.append(.{ doc, node }) catch unreachable;
    defer _ = resolve_trail.pop();

    const tree = &doc.ast_context.tree;

    const main_tokens = tree.nodes.items(.main_token);
    const node_tags = tree.nodes.items(.tag);
    const datas = tree.nodes.items(.data);
    const token_tags = tree.tokens.items(.tag);
    const starts = tree.tokens.items(.start);

    switch (node_tags[node]) {
        .global_var_decl,
        .local_var_decl,
        .simple_var_decl,
        .aligned_var_decl,
        => {
            const var_decl = ast.varDecl(tree, node).?;
            if (var_decl.ast.type_node != 0) {
                if (resolveTypeOfNodeInternal(
                    arena,
                    workspace,
                    doc,
                    var_decl.ast.type_node,
                    bound_type_params,
                )) |typ|
                    return typ.instanceTypeVal();
            }
            if (var_decl.ast.init_node == 0)
                return null;

            return resolveTypeOfNodeInternal(
                arena,
                workspace,
                doc,
                var_decl.ast.init_node,
                bound_type_params,
            );
        },
        .identifier => {
            if (isTypeIdent(doc.ast_context.tree, main_tokens[node])) {
                return Self{
                    .type = .{ .data = .primitive, .is_type_val = true },
                    .handle = doc,
                };
            }

            var lookup = SymbolLookup.init(arena.allocator());
            defer lookup.deinit();
            if (lookup.lookupSymbolGlobalTokenIndex(
                arena,
                workspace,
                doc,
                AstToken.init(&doc.ast_context.tree, main_tokens[node]),
            )) |child| {
                switch (child.decl.*) {
                    .ast_node => |n| {
                        if (n == node) return null;
                        if (ast.varDecl(&child.handle.ast_context.tree, n)) |var_decl| {
                            if (var_decl.ast.init_node == node)
                                return null;
                        }
                    },
                    else => {},
                }
                return try child.resolveType(arena, workspace, bound_type_params);
            }
            return null;
        },
        .call,
        .call_comma,
        .async_call,
        .async_call_comma,
        .call_one,
        .call_one_comma,
        .async_call_one,
        .async_call_one_comma,
        => {
            var params: [1]Ast.Node.Index = undefined;
            const call = ast.callFull(tree, node, &params) orelse unreachable;

            const decl = resolveTypeOfNodeInternal(arena, workspace, doc, call.ast.fn_expr, bound_type_params) orelse
                return null;

            if (decl.type.is_type_val) return null;
            const decl_node = switch (decl.type.data) {
                .other => |n| n,
                else => return null,
            };
            var buf: [1]Ast.Node.Index = undefined;
            const func_maybe = ast.fnProto(&decl.handle.ast_context.tree, decl_node, &buf);

            if (func_maybe) |fn_decl| {
                var expected_params = fn_decl.ast.params.len;
                // If we call as method, the first parameter should be skipped
                // TODO: Back-parse to extract the self argument?
                var it = fn_decl.iterate(&decl.handle.ast_context.tree);
                if (token_tags[call.ast.lparen - 2] == .period) {
                    if (try hasSelfParam(arena, workspace, decl.handle, fn_decl)) {
                        _ = it.next();
                        expected_params -= 1;
                    }
                }

                // Bind type params to the arguments passed in the call.
                const param_len = std.math.min(call.ast.params.len, expected_params);
                var i: usize = 0;
                while (it.next()) |decl_param| : (i += 1) {
                    if (i >= param_len) break;
                    if (!Self.isMetaType(&decl.handle.ast_context.tree, decl_param.type_expr))
                        continue;

                    const argument_type = resolveTypeOfNodeInternal(
                        arena,
                        workspace,
                        doc,
                        call.ast.params[i],
                        bound_type_params,
                    ) orelse {
                        continue;
                    };
                    if (!argument_type.type.is_type_val) continue;

                    _ = bound_type_params.put(decl_param, argument_type) catch unreachable;
                }

                const has_body = decl.handle.ast_context.tree.nodes.items(.tag)[decl_node] == .fn_decl;
                const body = decl.handle.ast_context.tree.nodes.items(.data)[decl_node].rhs;
                return try resolveReturnType(arena, workspace, fn_decl, decl.handle, bound_type_params, if (has_body) body else null);
            }
            return null;
        },
        .@"comptime",
        .@"nosuspend",
        .grouped_expression,
        .container_field,
        .container_field_init,
        .container_field_align,
        .struct_init,
        .struct_init_comma,
        .struct_init_one,
        .struct_init_one_comma,
        .slice,
        .slice_sentinel,
        .slice_open,
        .deref,
        .unwrap_optional,
        .array_access,
        .@"orelse",
        .@"catch",
        .@"try",
        .address_of,
        => {
            const base_type = resolveTypeOfNodeInternal(arena, workspace, doc, datas[node].lhs, bound_type_params) orelse
                return null;
            return switch (node_tags[node]) {
                .@"comptime",
                .@"nosuspend",
                .grouped_expression,
                => base_type,
                .container_field,
                .container_field_init,
                .container_field_align,
                .struct_init,
                .struct_init_comma,
                .struct_init_one,
                .struct_init_one_comma,
                => base_type.instanceTypeVal(),
                .slice,
                .slice_sentinel,
                .slice_open,
                => try resolveBracketAccessType(arena, workspace, base_type, .Range, bound_type_params),
                .deref => try resolveDerefType(arena, workspace, base_type, bound_type_params),
                .unwrap_optional => resolveUnwrapOptionalType(arena, workspace, base_type, bound_type_params),
                .array_access => try resolveBracketAccessType(arena, workspace, base_type, .Single, bound_type_params),
                .@"orelse" => resolveUnwrapOptionalType(arena, workspace, base_type, bound_type_params),
                .@"catch" => try resolveUnwrapErrorType(arena, workspace, base_type, bound_type_params),
                .@"try" => try resolveUnwrapErrorType(arena, workspace, base_type, bound_type_params),
                .address_of => {
                    const lhs_node = switch (base_type.type.data) {
                        .other => |n| n,
                        else => return null,
                    };
                    return Self{
                        .type = .{ .data = .{ .pointer = lhs_node }, .is_type_val = base_type.type.is_type_val },
                        .handle = base_type.handle,
                    };
                },
                else => unreachable,
            };
        },
        .field_access => {
            if (datas[node].rhs == 0) return null;
            const rhs_str = tree.tokenSlice(datas[node].rhs);
            // If we are accessing a pointer type, remove one pointerness level :)
            const left_type = try resolveFieldAccessLhsType(
                arena,
                workspace,
                resolveTypeOfNodeInternal(arena, workspace, doc, datas[node].lhs, bound_type_params) orelse return null,
                bound_type_params,
            );

            const left_type_node = switch (left_type.type.data) {
                .other => |n| n,
                else => return null,
            };
            var lookup = SymbolLookup.init(arena.allocator());
            defer lookup.deinit();
            if (lookup.lookupSymbolContainer(
                arena,
                workspace,
                left_type.handle,
                left_type_node,
                rhs_str,
                !left_type.type.is_type_val,
            )) |child| {
                return try child.resolveType(arena, workspace, bound_type_params);
            } else return null;
        },
        .array_type,
        .array_type_sentinel,
        .optional_type,
        .ptr_type_aligned,
        .ptr_type,
        .ptr_type_bit_range,
        .error_union,
        .error_set_decl,
        .container_decl,
        .container_decl_arg,
        .container_decl_arg_trailing,
        .container_decl_trailing,
        .container_decl_two,
        .container_decl_two_trailing,
        .tagged_union,
        .tagged_union_trailing,
        .tagged_union_two,
        .tagged_union_two_trailing,
        .tagged_union_enum_tag,
        .tagged_union_enum_tag_trailing,
        => return Self.typeVal(doc, node),
        .builtin_call,
        .builtin_call_comma,
        .builtin_call_two,
        .builtin_call_two_comma,
        => {
            const data = datas[node];
            const params = switch (node_tags[node]) {
                .builtin_call, .builtin_call_comma => tree.extra_data[data.lhs..data.rhs],
                .builtin_call_two, .builtin_call_two_comma => if (data.lhs == 0)
                    &[_]Ast.Node.Index{}
                else if (data.rhs == 0)
                    &[_]Ast.Node.Index{data.lhs}
                else
                    &[_]Ast.Node.Index{ data.lhs, data.rhs },
                else => unreachable,
            };

            const call_name = tree.tokenSlice(main_tokens[node]);
            if (std.mem.eql(u8, call_name, "@This")) {
                if (params.len != 0) return null;
                return innermostContainer(workspace, doc, starts[tree.firstToken(node)]);
            }

            const cast_map = std.ComptimeStringMap(void, .{
                .{"@as"},
                .{"@bitCast"},
                .{"@fieldParentPtr"},
                .{"@floatCast"},
                .{"@floatToInt"},
                .{"@intCast"},
                .{"@intToEnum"},
                .{"@intToFloat"},
                .{"@intToPtr"},
                .{"@truncate"},
                .{"@ptrCast"},
            });
            if (cast_map.has(call_name)) {
                if (params.len < 1) return null;
                return (resolveTypeOfNodeInternal(arena, workspace, doc, params[0], bound_type_params) orelse return null).instanceTypeVal();
            }

            // Almost the same as the above, return a type value though.
            // TODO Do peer type resolution, we just keep the first for now.
            if (std.mem.eql(u8, call_name, "@TypeOf")) {
                if (params.len < 1) return null;
                var resolved_type = resolveTypeOfNodeInternal(arena, workspace, doc, params[0], bound_type_params) orelse return null;

                if (resolved_type.type.is_type_val) return null;
                resolved_type.type.is_type_val = true;
                return resolved_type;
            }

            if (!std.mem.eql(u8, call_name, "@import")) return null;
            if (params.len == 0) return null;

            const import_param = params[0];
            if (node_tags[import_param] != .string_literal) return null;

            const import_str = tree.tokenSlice(main_tokens[import_param]);
            const new_handle = (workspace.resolveImport(doc, import_str[1 .. import_str.len - 1]) catch unreachable) orelse return null;

            // reference to node '0' which is root
            return Self.typeVal(new_handle, 0);
        },
        .fn_proto,
        .fn_proto_multi,
        .fn_proto_one,
        .fn_proto_simple,
        .fn_decl,
        => {
            var buf: [1]Ast.Node.Index = undefined;
            // This is a function type
            if (ast.fnProto(tree, node, &buf).?.name_token == null) {
                return typeVal(doc, node);
            }

            return notTypeVal(doc, node);
        },
        .multiline_string_literal,
        .string_literal,
        => return notTypeVal(doc, node),
        else => {},
    }
    return null;
}

/// Resolves the child type of a deref type
pub fn resolveDerefType(
    arena: *std.heap.ArenaAllocator,
    workspace: *Workspace,
    deref: Self,
    bound_type_params: *BoundTypeParams,
) !?Self {
    const deref_node = switch (deref.type.data) {
        .other => |n| n,
        .pointer => |n| return notTypeVal(deref.handle, n),
        else => return null,
    };
    const tree = &deref.handle.ast_context.tree;
    const main_token = tree.nodes.items(.main_token)[deref_node];
    const token_tag = tree.tokens.items(.tag)[main_token];

    if (isPtrType(tree, deref_node)) {
        const ptr_type = ast.ptrType(tree, deref_node).?;
        switch (token_tag) {
            .asterisk => {
                return (resolveTypeOfNodeInternal(
                    arena,
                    workspace,
                    deref.handle,
                    ptr_type.ast.child_type,
                    bound_type_params,
                ) orelse return null).instanceTypeVal();
            },
            .l_bracket, .asterisk_asterisk => return null,
            else => unreachable,
        }
    }
    return null;
}

fn isRoot(self: Self) bool {
    switch (self.type.data) {
        // root is always index 0
        .other => |n| return n == 0,
        else => return false,
    }
}

fn isContainerKind(self: Self, container_kind_tok: std.zig.Token.Tag) bool {
    const tree = self.handle.ast_context.tree;
    const main_tokens = tree.nodes.items(.main_token);
    const tags = tree.tokens.items(.tag);
    switch (self.type.data) {
        .other => |n| return tags[main_tokens[n]] == container_kind_tok,
        else => return false,
    }
}

pub fn isStructType(self: Self) bool {
    return self.isContainerKind(.keyword_struct) or self.isRoot();
}

pub fn isNamespace(self: Self) bool {
    if (!self.isStructType()) return false;
    const tree = self.handle.ast_context.tree;
    const node = self.type.data.other;
    const tags = tree.nodes.items(.tag);
    if (ast.isContainer(tree, node)) {
        var buf: [2]Ast.Node.Index = undefined;
        for (ast.declMembers(tree, node, &buf)) |child| {
            if (tags[child].isContainerField()) return false;
        }
    }
    return true;
}

pub fn isEnumType(self: Self) bool {
    return self.isContainerKind(.keyword_enum);
}

pub fn isUnionType(self: Self) bool {
    return self.isContainerKind(.keyword_union);
}

pub fn isOpaqueType(self: Self) bool {
    return self.isContainerKind(.keyword_opaque);
}

pub fn isTypeFunc(self: Self) bool {
    var buf: [1]Ast.Node.Index = undefined;
    const tree = self.handle.ast_context.tree;
    return switch (self.type.data) {
        .other => |n| if (ast.fnProto(tree, n, &buf)) |fn_proto| blk: {
            break :blk isTypeFunction(tree, fn_proto);
        } else false,
        else => false,
    };
}

pub fn isGenericFunc(self: Self) bool {
    var buf: [1]Ast.Node.Index = undefined;
    const tree = self.handle.ast_context.tree;
    return switch (self.type.data) {
        .other => |n| if (ast.fnProto(tree, n, &buf)) |fn_proto| blk: {
            break :blk isGenericFunction(tree, fn_proto);
        } else false,
        else => false,
    };
}

pub fn isFunc(self: Self) bool {
    const tree = self.handle.ast_context.tree;
    const tags = tree.nodes.items(.tag);
    return switch (self.type.data) {
        .other => |n| switch (tags[n]) {
            .fn_proto,
            .fn_proto_multi,
            .fn_proto_one,
            .fn_proto_simple,
            .fn_decl,
            => true,
            else => false,
        },
        else => false,
    };
}
