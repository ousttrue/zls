const std = @import("std");
const Ast = std.zig.Ast;
const Workspace = @import("./Workspace.zig");
const Document = @import("./Document.zig");
const UriBytePosition = @import("./UriBytePosition.zig");
const FieldAccessReturn = @import("./FieldAccessReturn.zig");
const Scope = @import("./Scope.zig");
const Declaration = Scope.Declaration;
const ast = @import("./ast.zig");
const TypeWithHandle = @import("./TypeWithHandle.zig");
const BoundTypeParams = std.AutoHashMap(Ast.full.FnProto.Param, TypeWithHandle);
const logger = std.log.scoped(.DeclWithHandle);
const Self = @This();

decl: *Declaration,
handle: *Document,

fn isContainerDecl(self: Self) bool {
    return switch (self.decl.*) {
        .ast_node => |inner_node| ast.isContainer(self.handle.tree.nodes.items(.tag)[inner_node]),
        else => false,
    };
}

pub fn nameToken(self: Self) Ast.TokenIndex {
    const tree = self.handle.tree;
    return switch (self.decl.*) {
        .ast_node => |n| ast.getDeclNameToken(tree, n).?,
        .param_decl => |p| p.name_token.?,
        .pointer_payload => |pp| pp.name,
        .array_payload => |ap| ap.identifier,
        .array_index => |ai| ai,
        .switch_payload => |sp| sp.node,
        .label_decl => |ld| ld,
    };
}

pub fn bytePosition(self: Self) usize {
    const tree = self.handle.tree;
    return tree.tokens.items(.start)[self.nameToken()];
}

pub fn isNodePublic(tree: Ast, node: Ast.Node.Index) bool {
    var buf: [1]Ast.Node.Index = undefined;
    return switch (tree.nodes.items(.tag)[node]) {
        .global_var_decl,
        .local_var_decl,
        .simple_var_decl,
        .aligned_var_decl,
        => ast.varDecl(tree, node).?.visib_token != null,
        .fn_proto,
        .fn_proto_multi,
        .fn_proto_one,
        .fn_proto_simple,
        .fn_decl,
        => ast.fnProto(tree, node, &buf).?.visib_token != null,
        else => true,
    };
}

pub fn isPublic(self: Self) bool {
    return switch (self.decl.*) {
        .ast_node => |node| isNodePublic(self.handle.tree, node),
        else => true,
    };
}

pub fn resolveType(self: Self, arena: *std.heap.ArenaAllocator, workspace: *Workspace, bound_type_params: *BoundTypeParams) !?TypeWithHandle {
    const tree = self.handle.tree;
    const node_tags = tree.nodes.items(.tag);
    const main_tokens = tree.nodes.items(.main_token);
    return switch (self.decl.*) {
        .ast_node => |node| try TypeWithHandle.resolveTypeOfNodeInternal(
            arena,
            workspace,
            self.handle,
            node,
            bound_type_params,
        ),
        .param_decl => |param_decl| {
            if (TypeWithHandle.isMetaType(self.handle.tree, param_decl.type_expr)) {
                var bound_param_it = bound_type_params.iterator();
                while (bound_param_it.next()) |entry| {
                    if (std.meta.eql(entry.key_ptr.*, param_decl)) return entry.value_ptr.*;
                }
                return null;
            } else if (node_tags[param_decl.type_expr] == .identifier) {
                if (param_decl.name_token) |name_tok| {
                    if (std.mem.eql(u8, tree.tokenSlice(main_tokens[param_decl.type_expr]), tree.tokenSlice(name_tok)))
                        return null;
                }
            }
            return ((try TypeWithHandle.resolveTypeOfNodeInternal(
                arena,
                workspace,
                self.handle,
                param_decl.type_expr,
                bound_type_params,
            )) orelse return null).instanceTypeVal();
        },
        .pointer_payload => |pay| try TypeWithHandle.resolveUnwrapOptionalType(
            arena,
            workspace,
            (try TypeWithHandle.resolveTypeOfNodeInternal(
                arena,
                workspace,
                self.handle,
                pay.condition,
                bound_type_params,
            )) orelse return null,
            bound_type_params,
        ),
        .array_payload => |pay| try TypeWithHandle.resolveBracketAccessType(
            arena,
            workspace,
            (try TypeWithHandle.resolveTypeOfNodeInternal(
                arena,
                workspace,
                self.handle,
                pay.array_expr,
                bound_type_params,
            )) orelse return null,
            .Single,
            bound_type_params,
        ),
        .array_index => TypeWithHandle{
            .type = .{ .data = .primitive, .is_type_val = false },
            .handle = self.handle,
        },
        .label_decl => return null,
        .switch_payload => |pay| {
            if (pay.items.len == 0) return null;
            // TODO Peer type resolution, we just use the first item for now.
            const switch_expr_type = (try TypeWithHandle.resolveTypeOfNodeInternal(arena, workspace, self.handle, pay.switch_expr, bound_type_params)) orelse return null;
            if (!switch_expr_type.isUnionType())
                return null;

            if (node_tags[pay.items[0]] == .enum_literal) {
                const scope = Scope.findContainerScope(switch_expr_type.handle, switch_expr_type.type.data.other) orelse return null;
                if (scope.decls.getEntry(tree.tokenSlice(main_tokens[pay.items[0]]))) |candidate| {
                    switch (candidate.value_ptr.*) {
                        .ast_node => |node| {
                            if (ast.containerField(switch_expr_type.handle.tree, node)) |container_field| {
                                if (container_field.ast.type_expr != 0) {
                                    return ((try TypeWithHandle.resolveTypeOfNodeInternal(
                                        arena,
                                        workspace,
                                        switch_expr_type.handle,
                                        container_field.ast.type_expr,
                                        bound_type_params,
                                    )) orelse return null).instanceTypeVal();
                                }
                            }
                        },
                        else => {},
                    }
                    return null;
                }
            }
            return null;
        },
    };
}

var using_trail: std.ArrayList([*]const u8) = undefined;
pub fn init(allocator: std.mem.Allocator) void {
    using_trail = std.ArrayList([*]const u8).init(allocator);
}
pub fn deinit() void {
    using_trail.deinit();
}

pub fn resolveUse(
    arena: *std.heap.ArenaAllocator,
    workspace: *Workspace,
    uses: []const *const Ast.Node.Index,
    symbol: []const u8,
    handle: *Document,
) error{OutOfMemory}!?Self {
    // If we were asked to resolve this symbol before,
    // it is self-referential and we cannot resolve it.
    if (std.mem.indexOfScalar([*]const u8, using_trail.items, symbol.ptr) != null)
        return null;
    try using_trail.append(symbol.ptr);
    defer _ = using_trail.pop();

    for (uses) |use| {
        const index = use.*;

        if (handle.tree.nodes.items(.data).len <= index) continue;

        const expr_type_node = (try TypeWithHandle.resolveTypeOfNode(
            arena,
            workspace,
            handle,
            handle.tree.nodes.items(.data)[index].lhs,
        )) orelse
            continue;

        const node = switch (expr_type_node.type.data) {
            .other => |n| n,
            else => continue,
        };

        if (try lookupSymbolContainer(
            arena,
            workspace,
            expr_type_node.handle,
            node,
            symbol,
            false,
        )) |candidate| {
            if (candidate.handle == handle or candidate.isPublic()) {
                return candidate;
            }
        }
    }
    return null;
}

pub fn lookupSymbolContainer(
    arena: *std.heap.ArenaAllocator,
    workspace: *Workspace,
    handle: *Document,
    container: Ast.Node.Index,
    symbol: []const u8,
    /// If true, we are looking up the symbol like we are accessing through a field access
    /// of an instance of the type, otherwise as a field access of the type value itself.
    instance_access: bool,
) error{OutOfMemory}!?Self {
    const tree = handle.tree;
    const node_tags = tree.nodes.items(.tag);
    const token_tags = tree.tokens.items(.tag);
    const main_token = tree.nodes.items(.main_token)[container];

    const is_enum = token_tags[main_token] == .keyword_enum;

    if (Scope.findContainerScope(handle, container)) |container_scope| {
        if (container_scope.decls.getEntry(symbol)) |candidate| {
            switch (candidate.value_ptr.*) {
                .ast_node => |node| {
                    if (node_tags[node].isContainerField()) {
                        if (!instance_access and !is_enum) return null;
                        if (instance_access and is_enum) return null;
                    }
                },
                .label_decl => unreachable,
                else => {},
            }
            return Self{ .decl = candidate.value_ptr, .handle = handle };
        }

        if (try resolveUse(arena, workspace, container_scope.uses, symbol, handle)) |result| return result;
        return null;
    }

    return null;
}

fn resolveVarDeclAliasInternal(
    arena: *std.heap.ArenaAllocator,
    workspace: *Workspace,
    handle: *Document,
    node: Ast.Node.Index,
    root: bool,
) error{OutOfMemory}!?Self {
    _ = root;
    const tree = handle.tree;
    const node_tags = tree.nodes.items(.tag);
    const main_tokens = tree.nodes.items(.main_token);
    const datas = tree.nodes.items(.data);

    if (node_tags[node] == .identifier) {
        const token = main_tokens[node];
        return try lookupSymbolGlobal(
            arena,
            workspace,
            handle,
            tree.tokenSlice(token),
            tree.tokens.items(.start)[token],
        );
    }

    if (node_tags[node] == .field_access) {
        const lhs = datas[node].lhs;

        var container_node: Ast.Node.Index = undefined;
        var container_handle: *Document = undefined;
        if (ast.isBuiltinCall(tree, lhs)) {
            if (!std.mem.eql(u8, tree.tokenSlice(main_tokens[lhs]), "@import"))
                return null;

            const inner_node = (try TypeWithHandle.resolveTypeOfNode(arena, workspace, handle, lhs)) orelse return null;
            // assert root node
            std.debug.assert(inner_node.type.data.other == 0);
            container_handle = inner_node.handle;
            container_node = inner_node.type.data.other;
        } else if (try resolveVarDeclAliasInternal(arena, workspace, handle, lhs, false)) |decl_handle| {
            if (decl_handle.decl.* != .ast_node) return null;
            const resolved = (try TypeWithHandle.resolveTypeOfNode(arena, workspace, decl_handle.handle, decl_handle.decl.ast_node)) orelse return null;
            const resolved_node = switch (resolved.type.data) {
                .other => |n| n,
                else => return null,
            };
            if (!ast.isContainer(resolved.handle.tree, resolved_node)) return null;
            container_handle = resolved.handle;
            container_node = resolved_node;
        } else {
            return null;
        }

        return try lookupSymbolContainer(
            arena,
            workspace,
            container_handle,
            container_node,
            tree.tokenSlice(datas[node].rhs),
            false,
        );
    }
    return null;
}

/// Resolves variable declarations consisting of chains of imports and field accesses of containers, ending with the same name as the variable decl's name
/// Examples:
///```zig
/// const decl = @import("decl-file.zig").decl;
/// const other = decl.middle.other;
///```
pub fn resolveVarDeclAlias(
    arena: *std.heap.ArenaAllocator,
    workspace: *Workspace,
    handle: *Document,
    decl: Ast.Node.Index,
) !?Self {
    const tree = handle.tree;
    const token_tags = tree.tokens.items(.tag);
    const node_tags = tree.nodes.items(.tag);

    if (ast.varDecl(handle.tree, decl)) |var_decl| {
        if (var_decl.ast.init_node == 0) return null;
        const base_exp = var_decl.ast.init_node;
        if (token_tags[var_decl.ast.mut_token] != .keyword_const) return null;

        if (node_tags[base_exp] == .field_access) {
            const name = tree.tokenSlice(tree.nodes.items(.data)[base_exp].rhs);
            if (!std.mem.eql(u8, tree.tokenSlice(var_decl.ast.mut_token + 1), name))
                return null;

            return try resolveVarDeclAliasInternal(arena, workspace, handle, base_exp, true);
        }
    }

    return null;
}

/// Token location inside source
pub fn getSymbolFieldAccess(
    arena: *std.heap.ArenaAllocator,
    workspace: *Workspace,
    doc: *Document,
    byte_position: usize,
) !Self {
    const token_with_index = doc.ast_context.tokenFromBytePos(byte_position) orelse return error.NoToken;
    const idx = doc.ast_context.tokens_node[token_with_index.index];
    const tag = doc.tree.nodes.items(.tag);
    std.debug.assert(tag[idx] == .field_access);
    const first = doc.ast_context.tokens.items[doc.tree.firstToken(idx)];
    const last = doc.ast_context.tokens.items[doc.tree.lastToken(idx)];
    const range = std.zig.Token.Loc{ .start = first.loc.start, .end = last.loc.end };

    const allocator = arena.allocator();
    var copy = try allocator.dupeZ(u8, doc.utf8_buffer.text[range.start..range.end]);
    defer allocator.free(copy);
    var tokenizer = std.zig.Tokenizer.init(copy);
    const result = (try FieldAccessReturn.getFieldAccessType(
        arena,
        workspace,
        doc,
        byte_position,
        &tokenizer,
    )) orelse return error.NoFieldAccessType;
    const container_handle = result.unwrapped orelse result.original;
    const container_handle_node = switch (container_handle.type.data) {
        .other => |n| n,
        else => return error.NodeNotFound,
    };
    const name = doc.identifierFromPosition(byte_position) orelse return error.NoIdentifier;
    return (try Self.lookupSymbolContainer(
        arena,
        workspace,
        container_handle.handle,
        container_handle_node,
        name,
        true,
    )) orelse return error.ContainerSymbolNotFound;
}

pub fn gotoDefinitionSymbol(
    self: Self,
    workspace: *Workspace,
    arena: *std.heap.ArenaAllocator,
    resolve_alias: bool,
) !?UriBytePosition {
    var handle = self.handle;

    const byte_position = switch (self.decl.*) {
        .ast_node => |node| block: {
            if (resolve_alias) {
                if (try resolveVarDeclAlias(arena, workspace, handle, node)) |result| {
                    handle = result.handle;
                    break :block result.bytePosition();
                }
            }

            const name_token = ast.getDeclNameToken(handle.tree, node) orelse
                return null;
            break :block handle.tree.tokens.items(.start)[name_token];
        },
        else => self.bytePosition(),
    };

    return UriBytePosition{
        .uri = handle.utf8_buffer.uri,
        .loc = .{ .start = byte_position, .end = byte_position },
    };
}

pub fn getSymbolGlobal(arena: *std.heap.ArenaAllocator, workspace: *Workspace, doc: *Document, pos_index: usize) !?Self {
    if (doc.identifierFromPosition(pos_index)) |name| {
        return lookupSymbolGlobal(arena, workspace, doc, name, pos_index);
    } else {
        return null;
    }
}

pub fn lookupSymbolGlobal(
    arena: *std.heap.ArenaAllocator,
    workspace: *Workspace,
    handle: *Document,
    symbol: []const u8,
    source_index: usize,
) error{OutOfMemory}!?Self {
    const innermost_scope_idx = handle.innermostBlockScopeIndex(source_index);

    var curr = innermost_scope_idx;
    while (curr >= 0) : (curr -= 1) {
        const scope = &handle.document_scope.scopes[curr];
        if (source_index >= scope.range.start and source_index <= scope.range.end) blk: {
            if (scope.decls.getEntry(symbol)) |candidate| {
                switch (candidate.value_ptr.*) {
                    .ast_node => |node| {
                        if (handle.tree.nodes.items(.tag)[node].isContainerField()) break :blk;
                    },
                    .label_decl => break :blk,
                    else => {},
                }
                return Self{
                    .decl = candidate.value_ptr,
                    .handle = handle,
                };
            }
            if (try resolveUse(arena, workspace, scope.uses, symbol, handle)) |result| return result;
        }
        if (curr == 0) break;
    }
    return null;
}

pub fn renameSymbol(
    self: Self,
    arena: *std.heap.ArenaAllocator,
    workspace: *Workspace,
) ![]UriBytePosition {
    std.debug.assert(self.decl.* != .label_decl);
    var locations = std.ArrayList(UriBytePosition).init(arena.allocator());
    try self.symbolReferences(arena, workspace, true, &locations, true);
    return locations.items;
}

pub fn renameLabel(
    self: Self,
    arena: *std.heap.ArenaAllocator,
) ![]UriBytePosition {
    std.debug.assert(self.decl.* == .label_decl);
    var locations = std.ArrayList(UriBytePosition).init(arena.allocator());
    try self.labelReferences(true, &locations);
    return locations.items;
}

pub fn labelReferences(decl: Self, include_decl: bool, locations: *std.ArrayList(UriBytePosition)) !void {
    std.debug.assert(decl.decl.* == .label_decl);
    const handle = decl.handle;
    const tree = handle.tree;
    const token_tags = tree.tokens.items(.tag);

    // Find while / for / block from label -> iterate over children nodes, find break and continues, change their labels if they match.
    // This case can be implemented just by scanning tokens.
    const first_tok = tree.firstToken(decl.decl.label_decl);
    const last_tok = tree.firstToken(decl.decl.label_decl);

    if (include_decl) {
        // The first token is always going to be the label
        try locations.append(handle.tokenReference(first_tok));
    }

    var curr_tok = first_tok + 1;
    while (curr_tok < last_tok - 2) : (curr_tok += 1) {
        const curr_id = token_tags[curr_tok];
        if ((curr_id == .keyword_break or curr_id == .keyword_continue) and token_tags[curr_tok + 1] == .colon and
            token_tags[curr_tok + 2] == .identifier)
        {
            if (std.mem.eql(u8, tree.tokenSlice(curr_tok + 2), tree.tokenSlice(first_tok))) {
                try locations.append(handle.tokenReference(first_tok));
            }
        }
    }
}

fn symbolReferencesInternal(
    self: Self,
    arena: *std.heap.ArenaAllocator,
    workspace: *Workspace,
    doc: *Document,
    node: Ast.Node.Index,
    locations: *std.ArrayList(UriBytePosition),
) error{OutOfMemory}!void {
    const tree = doc.tree;
    if (node > tree.nodes.len) return;
    const node_tags = tree.nodes.items(.tag);
    const datas = tree.nodes.items(.data);
    const main_tokens = tree.nodes.items(.main_token);
    const starts = tree.tokens.items(.start);

    switch (node_tags[node]) {
        .block, .block_semicolon, .block_two, .block_two_semicolon => {
            const statements: []const Ast.Node.Index = switch (node_tags[node]) {
                .block, .block_semicolon => tree.extra_data[datas[node].lhs..datas[node].rhs],
                .block_two, .block_two_semicolon => blk: {
                    const statements = &[_]Ast.Node.Index{ datas[node].lhs, datas[node].rhs };
                    const len: usize = if (datas[node].lhs == 0)
                        @as(usize, 0)
                    else if (datas[node].rhs == 0)
                        @as(usize, 1)
                    else
                        @as(usize, 2);
                    break :blk statements[0..len];
                },
                else => unreachable,
            };
            for (statements) |stmt|
                try self.symbolReferencesInternal(
                    arena,
                    workspace,
                    doc,
                    stmt,
                    locations,
                );
        },
        .container_decl,
        .container_decl_trailing,
        .container_decl_arg,
        .container_decl_arg_trailing,
        .container_decl_two,
        .container_decl_two_trailing,
        .tagged_union,
        .tagged_union_trailing,
        .tagged_union_two,
        .tagged_union_two_trailing,
        .tagged_union_enum_tag,
        .tagged_union_enum_tag_trailing,
        .root,
        .error_set_decl,
        => {
            var buf: [2]Ast.Node.Index = undefined;
            for (ast.declMembers(tree, node, &buf)) |member|
                try self.symbolReferencesInternal(
                    arena,
                    workspace,
                    doc,
                    member,
                    locations,
                );
        },
        .global_var_decl,
        .local_var_decl,
        .simple_var_decl,
        .aligned_var_decl,
        => {
            const var_decl = ast.varDecl(tree, node).?;
            if (var_decl.ast.type_node != 0) {
                try self.symbolReferencesInternal(
                    arena,
                    workspace,
                    doc,
                    var_decl.ast.type_node,
                    locations,
                );
            }
            if (var_decl.ast.init_node != 0) {
                try self.symbolReferencesInternal(
                    arena,
                    workspace,
                    doc,
                    var_decl.ast.init_node,
                    locations,
                );
            }
        },
        .@"usingnamespace" => {
            try self.symbolReferencesInternal(
                arena,
                workspace,
                doc,
                datas[node].lhs,
                locations,
            );
        },
        .container_field,
        .container_field_align,
        .container_field_init,
        => {
            const field = ast.containerField(tree, node).?;
            if (field.ast.type_expr != 0) {
                try self.symbolReferencesInternal(
                    arena,
                    workspace,
                    doc,
                    field.ast.type_expr,
                    locations,
                );
            }
            if (field.ast.value_expr != 0) {
                try self.symbolReferencesInternal(
                    arena,
                    workspace,
                    doc,
                    field.ast.value_expr,
                    locations,
                );
            }
        },
        .identifier => {
            if (try lookupSymbolGlobal(arena, workspace, doc, tree.getNodeSource(node), starts[main_tokens[node]])) |child| {
                if (std.meta.eql(self, child)) {
                    try locations.append(doc.tokenReference(main_tokens[node]));
                }
            }
        },
        .fn_proto,
        .fn_proto_multi,
        .fn_proto_one,
        .fn_proto_simple,
        .fn_decl,
        => {
            var buf: [1]Ast.Node.Index = undefined;
            const fn_proto = ast.fnProto(tree, node, &buf).?;
            var it = fn_proto.iterate(&tree);
            while (it.next()) |param| {
                if (param.type_expr != 0)
                    try self.symbolReferencesInternal(arena, workspace, doc, param.type_expr, locations);
            }

            if (fn_proto.ast.return_type != 0) {
                try self.symbolReferencesInternal(arena, workspace, doc, fn_proto.ast.return_type, locations);
            }
            if (fn_proto.ast.align_expr != 0) {
                try self.symbolReferencesInternal(arena, workspace, doc, fn_proto.ast.align_expr, locations);
            }
            if (fn_proto.ast.section_expr != 0) {
                try self.symbolReferencesInternal(arena, workspace, doc, fn_proto.ast.section_expr, locations);
            }
            if (fn_proto.ast.callconv_expr != 0) {
                try self.symbolReferencesInternal(arena, workspace, doc, fn_proto.ast.callconv_expr, locations);
            }
            if (node_tags[node] == .fn_decl) {
                try self.symbolReferencesInternal(arena, workspace, doc, datas[node].rhs, locations);
            }
        },
        .anyframe_type => {
            try self.symbolReferencesInternal(arena, workspace, doc, datas[node].rhs, locations);
        },
        .@"defer" => {
            try self.symbolReferencesInternal(arena, workspace, doc, datas[node].rhs, locations);
        },
        .@"comptime" => {
            try self.symbolReferencesInternal(arena, workspace, doc, datas[node].lhs, locations);
        },
        .@"nosuspend" => {
            try self.symbolReferencesInternal(arena, workspace, doc, datas[node].lhs, locations);
        },
        .@"switch",
        .switch_comma,
        => {
            // TODO When renaming a union(enum) field, also rename switch items that refer to it.
            try self.symbolReferencesInternal(arena, workspace, doc, datas[node].lhs, locations);
            const extra = tree.extraData(datas[node].rhs, Ast.Node.SubRange);
            const cases = tree.extra_data[extra.start..extra.end];
            for (cases) |case| {
                try self.symbolReferencesInternal(arena, workspace, doc, case, locations);
            }
        },
        .switch_case_one => {
            const case_one = tree.switchCaseOne(node);
            if (case_one.ast.target_expr != 0)
                try self.symbolReferencesInternal(arena, workspace, doc, case_one.ast.target_expr, locations);
            for (case_one.ast.values) |val|
                try self.symbolReferencesInternal(arena, workspace, doc, val, locations);
        },
        .switch_case => {
            const case = tree.switchCase(node);
            if (case.ast.target_expr != 0)
                try self.symbolReferencesInternal(arena, workspace, doc, case.ast.target_expr, locations);
            for (case.ast.values) |val|
                try self.symbolReferencesInternal(arena, workspace, doc, val, locations);
        },
        .@"while",
        .while_simple,
        .while_cont,
        .for_simple,
        .@"for",
        => {
            const loop = ast.whileAst(tree, node).?;
            try self.symbolReferencesInternal(arena, workspace, doc, loop.ast.cond_expr, locations);
            if (loop.ast.cont_expr != 0) {
                try self.symbolReferencesInternal(arena, workspace, doc, loop.ast.cont_expr, locations);
            }
            try self.symbolReferencesInternal(arena, workspace, doc, loop.ast.then_expr, locations);
            if (loop.ast.else_expr != 0) {
                try self.symbolReferencesInternal(arena, workspace, doc, loop.ast.else_expr, locations);
            }
        },
        .@"if",
        .if_simple,
        => {
            const if_node = ast.ifFull(tree, node);

            try self.symbolReferencesInternal(arena, workspace, doc, if_node.ast.cond_expr, locations);
            try self.symbolReferencesInternal(arena, workspace, doc, if_node.ast.then_expr, locations);
            if (if_node.ast.else_expr != 0) {
                try self.symbolReferencesInternal(arena, workspace, doc, if_node.ast.else_expr, locations);
            }
        },
        .array_type,
        .array_type_sentinel,
        => {
            try self.symbolReferencesInternal(arena, workspace, doc, datas[node].lhs, locations);
            try self.symbolReferencesInternal(arena, workspace, doc, datas[node].rhs, locations);
        },
        .ptr_type,
        .ptr_type_aligned,
        .ptr_type_bit_range,
        .ptr_type_sentinel,
        => {
            const ptr_type = ast.ptrType(tree, node).?;

            if (ptr_type.ast.align_node != 0) {
                try self.symbolReferencesInternal(arena, workspace, doc, ptr_type.ast.align_node, locations);
                if (node_tags[node] == .ptr_type_bit_range) {
                    try self.symbolReferencesInternal(arena, workspace, doc, ptr_type.ast.bit_range_start, locations);
                    try self.symbolReferencesInternal(arena, workspace, doc, ptr_type.ast.bit_range_end, locations);
                }
            }
            if (ptr_type.ast.sentinel != 0) {
                try self.symbolReferencesInternal(arena, workspace, doc, ptr_type.ast.sentinel, locations);
            }

            try self.symbolReferencesInternal(arena, workspace, doc, ptr_type.ast.child_type, locations);
        },
        .address_of, .@"await", .bit_not, .bool_not, .optional_type, .negation, .negation_wrap, .@"resume", .@"try" => {
            try self.symbolReferencesInternal(arena, workspace, doc, datas[node].lhs, locations);
        },
        .array_init,
        .array_init_comma,
        .array_init_dot,
        .array_init_dot_comma,
        .array_init_one,
        .array_init_one_comma,
        .array_init_dot_two,
        .array_init_dot_two_comma,
        => |n| {
            var buf: [2]Ast.Node.Index = undefined;
            const array_init = switch (n) {
                .array_init, .array_init_comma => tree.arrayInit(node),
                .array_init_dot, .array_init_dot_comma => tree.arrayInitDot(node),
                .array_init_one, .array_init_one_comma => tree.arrayInitOne(buf[0..1], node),
                .array_init_dot_two, .array_init_dot_two_comma => tree.arrayInitDotTwo(&buf, node),
                else => unreachable,
            };
            if (array_init.ast.type_expr != 0)
                try self.symbolReferencesInternal(arena, workspace, doc, array_init.ast.type_expr, locations);
            for (array_init.ast.elements) |e|
                try self.symbolReferencesInternal(arena, workspace, doc, e, locations);
        },
        .struct_init,
        .struct_init_comma,
        .struct_init_dot,
        .struct_init_dot_comma,
        .struct_init_dot_two,
        .struct_init_dot_two_comma,
        .struct_init_one,
        .struct_init_one_comma,
        => |n| {
            var buf: [2]Ast.Node.Index = undefined;
            const struct_init: Ast.full.StructInit = switch (n) {
                .struct_init, .struct_init_comma => tree.structInit(node),
                .struct_init_dot, .struct_init_dot_comma => tree.structInitDot(node),
                .struct_init_one, .struct_init_one_comma => tree.structInitOne(buf[0..1], node),
                .struct_init_dot_two, .struct_init_dot_two_comma => tree.structInitDotTwo(&buf, node),
                else => unreachable,
            };
            if (struct_init.ast.type_expr != 0)
                try self.symbolReferencesInternal(arena, workspace, doc, struct_init.ast.type_expr, locations);
            for (struct_init.ast.fields) |field|
                try self.symbolReferencesInternal(arena, workspace, doc, field, locations);
        },
        .call,
        .call_comma,
        .call_one,
        .call_one_comma,
        .async_call,
        .async_call_comma,
        .async_call_one,
        .async_call_one_comma,
        => |c| {
            var buf: [1]Ast.Node.Index = undefined;
            const call: Ast.full.Call = switch (c) {
                .call, .call_comma, .async_call, .async_call_comma => tree.callFull(node),
                .call_one, .call_one_comma, .async_call_one, .async_call_one_comma => tree.callOne(&buf, node),
                else => unreachable,
            };
            if (call.ast.fn_expr != 0)
                try self.symbolReferencesInternal(arena, workspace, doc, call.ast.fn_expr, locations);

            for (call.ast.params) |param| {
                try self.symbolReferencesInternal(arena, workspace, doc, param, locations);
            }
        },
        .slice,
        .slice_sentinel,
        .slice_open,
        => |s| {
            const slice: Ast.full.Slice = switch (s) {
                .slice => tree.slice(node),
                .slice_open => tree.sliceOpen(node),
                .slice_sentinel => tree.sliceSentinel(node),
                else => unreachable,
            };

            try self.symbolReferencesInternal(arena, workspace, doc, slice.ast.sliced, locations);
            try self.symbolReferencesInternal(arena, workspace, doc, slice.ast.start, locations);
            if (slice.ast.end != 0)
                try self.symbolReferencesInternal(arena, workspace, doc, slice.ast.end, locations);
            if (slice.ast.sentinel != 0)
                try self.symbolReferencesInternal(arena, workspace, doc, slice.ast.sentinel, locations);
        },
        .array_access => {
            try self.symbolReferencesInternal(arena, workspace, doc, datas[node].lhs, locations);
            try self.symbolReferencesInternal(arena, workspace, doc, datas[node].rhs, locations);
        },
        .deref,
        .unwrap_optional,
        => {
            try self.symbolReferencesInternal(arena, workspace, doc, datas[node].lhs, locations);
        },
        .grouped_expression => {
            try self.symbolReferencesInternal(arena, workspace, doc, datas[node].lhs, locations);
        },
        .@"return",
        .@"break",
        .@"continue",
        => {
            if (datas[node].lhs != 0) {
                try self.symbolReferencesInternal(arena, workspace, doc, datas[node].lhs, locations);
            }
        },
        .@"suspend" => {
            if (datas[node].lhs != 0) {
                try self.symbolReferencesInternal(arena, workspace, doc, datas[node].lhs, locations);
            }
        },
        .builtin_call,
        .builtin_call_comma,
        .builtin_call_two,
        .builtin_call_two_comma,
        => |builtin_tag| {
            const data = datas[node];
            const params = switch (builtin_tag) {
                .builtin_call, .builtin_call_comma => tree.extra_data[data.lhs..data.rhs],
                .builtin_call_two, .builtin_call_two_comma => if (data.lhs == 0)
                    &[_]Ast.Node.Index{}
                else if (data.rhs == 0)
                    &[_]Ast.Node.Index{data.lhs}
                else
                    &[_]Ast.Node.Index{ data.lhs, data.rhs },
                else => unreachable,
            };

            for (params) |param|
                try self.symbolReferencesInternal(arena, workspace, doc, param, locations);
        },
        .@"asm",
        .asm_simple,
        => |a| {
            const _asm: Ast.full.Asm = if (a == .@"asm") tree.asmFull(node) else tree.asmSimple(node);
            if (_asm.ast.items.len == 0)
                try self.symbolReferencesInternal(arena, workspace, doc, _asm.ast.template, locations);

            for (_asm.inputs) |input|
                try self.symbolReferencesInternal(arena, workspace, doc, input, locations);

            for (_asm.outputs) |output|
                try self.symbolReferencesInternal(arena, workspace, doc, output, locations);
        },
        .test_decl => {
            try self.symbolReferencesInternal(arena, workspace, doc, datas[node].rhs, locations);
        },
        .field_access => {
            try self.symbolReferencesInternal(arena, workspace, doc, datas[node].lhs, locations);

            const rhs_str = tree.tokenSlice(datas[node].rhs);
            var bound_type_params = TypeWithHandle.BoundTypeParams.init(arena.allocator());
            const left_type = try TypeWithHandle.resolveFieldAccessLhsType(
                arena,
                workspace,
                (try TypeWithHandle.resolveTypeOfNodeInternal(arena, workspace, doc, datas[node].lhs, &bound_type_params)) orelse return,
                &bound_type_params,
            );

            const left_type_node = switch (left_type.type.data) {
                .other => |n| n,
                else => return,
            };

            if (try Self.lookupSymbolContainer(
                arena,
                workspace,
                left_type.handle,
                left_type_node,
                rhs_str,
                !left_type.type.is_type_val,
            )) |child| {
                if (std.meta.eql(child, self)) {
                    try locations.append(doc.tokenReference(datas[node].rhs));
                }
            }
        },
        .add,
        .add_wrap,
        .array_cat,
        .array_mult,
        .assign,
        .assign_bit_and,
        .assign_bit_or,
        .assign_shl,
        .assign_shr,
        .assign_bit_xor,
        .assign_div,
        .assign_sub,
        .assign_sub_wrap,
        .assign_mod,
        .assign_add,
        .assign_add_wrap,
        .assign_mul,
        .assign_mul_wrap,
        .bang_equal,
        .bit_and,
        .bit_or,
        .shl,
        .shr,
        .bit_xor,
        .bool_or,
        .div,
        .equal_equal,
        .error_union,
        .greater_or_equal,
        .greater_than,
        .less_or_equal,
        .less_than,
        .merge_error_sets,
        .mod,
        .mul,
        .mul_wrap,
        .switch_range,
        .sub,
        .sub_wrap,
        .@"orelse",
        => {
            try self.symbolReferencesInternal(arena, workspace, doc, datas[node].lhs, locations);
            try self.symbolReferencesInternal(arena, workspace, doc, datas[node].rhs, locations);
        },
        else => {},
    }
}

pub fn symbolReferences(
    self: Self,
    arena: *std.heap.ArenaAllocator,
    workspace: *Workspace,
    include_decl: bool,
    locations: *std.ArrayList(UriBytePosition),
    skip_std_references: bool,
) !void {
    std.debug.assert(self.decl.* != .label_decl);
    const curr_handle = self.handle;
    if (include_decl) {
        try locations.append(curr_handle.tokenReference(self.nameToken()));
    }

    switch (self.decl.*) {
        .ast_node => {
            try self.symbolReferencesInternal(
                arena,
                workspace,
                curr_handle,
                0,
                locations,
            );

            var imports = std.ArrayList(*Document).init(arena.allocator());

            var handle_it = workspace.handles.iterator();
            while (handle_it.next()) |entry| {
                if (skip_std_references and std.mem.indexOf(u8, entry.key_ptr.*, "std") != null) {
                    if (!include_decl or entry.value_ptr.* != curr_handle)
                        continue;
                }

                // Check entry's transitive imports
                try imports.append(entry.value_ptr.*);
                var i: usize = 0;
                blk: while (i < imports.items.len) : (i += 1) {
                    const import = imports.items[i];
                    for (import.imports_used.items) |uri| {
                        const h = workspace.getDocument(uri) orelse break;

                        if (h == curr_handle) {
                            // entry does import curr_handle
                            try self.symbolReferencesInternal(
                                arena,
                                workspace,
                                entry.value_ptr.*,
                                0,
                                locations,
                            );
                            break :blk;
                        }

                        select: {
                            for (imports.items) |item| {
                                if (item == h) {
                                    // already checked this import
                                    break :select;
                                }
                            }
                            try imports.append(h);
                        }
                    }
                }
                try imports.resize(0);
            }
        },
        .param_decl => |param| {
            // Rename the param tok.
            const fn_node: Ast.full.FnProto = loop: for (curr_handle.document_scope.scopes) |scope| {
                switch (scope.data) {
                    .function => |proto| {
                        var buf: [1]Ast.Node.Index = undefined;
                        const fn_proto = ast.fnProto(curr_handle.tree, proto, &buf).?;
                        var it = fn_proto.iterate(&curr_handle.tree);
                        while (it.next()) |candidate| {
                            if (std.meta.eql(candidate, param)) {
                                if (curr_handle.tree.nodes.items(.tag)[proto] == .fn_decl) {
                                    try self.symbolReferencesInternal(
                                        arena,
                                        workspace,
                                        curr_handle,
                                        curr_handle.tree.nodes.items(.data)[proto].rhs,
                                        locations,
                                    );
                                }
                                break :loop fn_proto;
                            }
                        }
                    },
                    else => {},
                }
            } else {
                logger.warn("Could not find param decl's function", .{});
                return;
            };
            _ = fn_node;
        },
        .pointer_payload, .switch_payload, .array_payload, .array_index => {
            try self.symbolReferencesInternal(arena, workspace, curr_handle, 0, locations);
        },
        .label_decl => unreachable,
    }
}

pub fn hoverSymbol(
    self: Self,
    arena: *std.heap.ArenaAllocator,
    workspace: *Workspace,
    hover_kind: ast.MarkupFormat,
) (std.os.WriteError || error{OutOfMemory})!?[]const u8 {
    const handle = self.handle;
    const tree = handle.tree;
    var doc_str: ?[]const u8 = null;

    const def_str = switch (self.decl.*) {
        .ast_node => |node| def: {
            if (try Self.resolveVarDeclAlias(arena, workspace, handle, node)) |result| {
                return try result.hoverSymbol(arena, workspace, hover_kind);
            }
            doc_str = try ast.getDocComments(arena.allocator(), tree, node, hover_kind);

            var buf: [1]Ast.Node.Index = undefined;

            if (ast.varDecl(tree, node)) |var_decl| {
                break :def ast.getVariableSignature(tree, var_decl);
            } else if (ast.fnProto(tree, node, &buf)) |fn_proto| {
                break :def ast.getFunctionSignature(tree, fn_proto);
            } else if (ast.containerField(tree, node)) |field| {
                break :def ast.getContainerFieldSignature(tree, field);
            } else {
                if (ast.nodeToString(tree, node)) |text| {
                    break :def text;
                }
                return null;
            }
        },
        .param_decl => |param| def: {
            if (param.first_doc_comment) |doc_comments| {
                doc_str = try ast.collectDocComments(arena.allocator(), handle.tree, doc_comments, hover_kind, false);
            }

            const first_token = param.first_doc_comment orelse
                param.comptime_noalias orelse
                param.name_token orelse
                tree.firstToken(param.type_expr); // extern fn
            const last_token = param.anytype_ellipsis3 orelse tree.lastToken(param.type_expr);

            const start = ast.tokenLocation(tree, first_token).start;
            const end = ast.tokenLocation(tree, last_token).end;
            break :def tree.source[start..end];
        },
        .pointer_payload => |payload| tree.tokenSlice(payload.name),
        .array_payload => |payload| handle.tree.tokenSlice(payload.identifier),
        .array_index => |payload| handle.tree.tokenSlice(payload),
        .switch_payload => |payload| tree.tokenSlice(payload.node),
        .label_decl => |label_decl| tree.tokenSlice(label_decl),
    };

    var hover_text: []const u8 = undefined;
    if (hover_kind == .Markdown) {
        hover_text = if (doc_str) |doc|
            try std.fmt.allocPrint(arena.allocator(), "```zig\n{s}\n```\n{s}", .{ def_str, doc })
        else
            try std.fmt.allocPrint(arena.allocator(), "```zig\n{s}\n```", .{def_str});
    } else {
        hover_text = if (doc_str) |doc|
            try std.fmt.allocPrint(arena.allocator(), "{s}\n{s}", .{ def_str, doc })
        else
            def_str;
    }
    return hover_text;
}
