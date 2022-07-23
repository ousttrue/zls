const std = @import("std");
const Ast = std.zig.Ast;
const Workspace = @import("./Workspace.zig");
const Document = @import("./Document.zig");
const DeclWithHandle = @import("./DeclWithHandle.zig");
const TypeWithHandle = @import("./TypeWithHandle.zig");
const lsp = @import("lsp");
const offsets = @import("./offsets.zig");
const log = std.log.scoped(.references);
const ast = @import("./ast.zig");

fn tokenReference(document: *Document, tok: Ast.TokenIndex, context: anytype, comptime handler: anytype) !void {
    const loc = offsets.tokenRelativeLocation(document.tree, 0, document.tree.tokens.items(.start)[tok], .utf8) catch return;
    try handler(context, lsp.Location{
        .uri = document.utf8_buffer.uri,
        .range = .{
            .start = .{
                .line = @intCast(i64, loc.line),
                .character = @intCast(i64, loc.column),
            },
            .end = .{
                .line = @intCast(i64, loc.line),
                .character = @intCast(i64, loc.column + offsets.tokenLength(document.tree, tok, .utf8)),
            },
        },
    });
}

pub fn labelReferences(decl: DeclWithHandle, include_decl: bool, context: anytype, comptime handler: anytype) !void {
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
        try tokenReference(handle, first_tok, context, handler);
    }

    var curr_tok = first_tok + 1;
    while (curr_tok < last_tok - 2) : (curr_tok += 1) {
        const curr_id = token_tags[curr_tok];
        if ((curr_id == .keyword_break or curr_id == .keyword_continue) and token_tags[curr_tok + 1] == .colon and
            token_tags[curr_tok + 2] == .identifier)
        {
            if (std.mem.eql(u8, tree.tokenSlice(curr_tok + 2), tree.tokenSlice(first_tok))) {
                try tokenReference(handle, first_tok, context, handler);
            }
        }
    }
}

fn symbolReferencesInternal(
    arena: *std.heap.ArenaAllocator,
    workspace: *Workspace,
    handle: *Document,
    node: Ast.Node.Index,
    decl: DeclWithHandle,
    context: anytype,
    comptime handler: anytype,
) error{OutOfMemory}!void {
    const tree = handle.tree;
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
                try symbolReferencesInternal(
                    arena,
                    workspace,
                    handle,
                    stmt,
                    decl,
                    context,
                    handler,
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
                try symbolReferencesInternal(
                    arena,
                    workspace,
                    handle,
                    member,
                    decl,
                    context,
                    handler,
                );
        },
        .global_var_decl,
        .local_var_decl,
        .simple_var_decl,
        .aligned_var_decl,
        => {
            const var_decl = ast.varDecl(tree, node).?;
            if (var_decl.ast.type_node != 0) {
                try symbolReferencesInternal(
                    arena,
                    workspace,
                    handle,
                    var_decl.ast.type_node,
                    decl,
                    context,
                    handler,
                );
            }
            if (var_decl.ast.init_node != 0) {
                try symbolReferencesInternal(
                    arena,
                    workspace,
                    handle,
                    var_decl.ast.init_node,
                    decl,
                    context,
                    handler,
                );
            }
        },
        .@"usingnamespace" => {
            try symbolReferencesInternal(
                arena,
                workspace,
                handle,
                datas[node].lhs,
                decl,
                context,
                handler,
            );
        },
        .container_field,
        .container_field_align,
        .container_field_init,
        => {
            const field = ast.containerField(tree, node).?;
            if (field.ast.type_expr != 0) {
                try symbolReferencesInternal(
                    arena,
                    workspace,
                    handle,
                    field.ast.type_expr,
                    decl,
                    context,
                    handler,
                );
            }
            if (field.ast.value_expr != 0) {
                try symbolReferencesInternal(
                    arena,
                    workspace,
                    handle,
                    field.ast.value_expr,
                    decl,
                    context,
                    handler,
                );
            }
        },
        .identifier => {
            if (try workspace.lookupSymbolGlobal(arena, handle, tree.getNodeSource(node), starts[main_tokens[node]])) |child| {
                if (std.meta.eql(decl, child)) {
                    try tokenReference(handle, main_tokens[node], context, handler);
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
                    try symbolReferencesInternal(arena, workspace, handle, param.type_expr, decl, context, handler);
            }

            if (fn_proto.ast.return_type != 0) {
                try symbolReferencesInternal(arena, workspace, handle, fn_proto.ast.return_type, decl, context, handler);
            }
            if (fn_proto.ast.align_expr != 0) {
                try symbolReferencesInternal(arena, workspace, handle, fn_proto.ast.align_expr, decl, context, handler);
            }
            if (fn_proto.ast.section_expr != 0) {
                try symbolReferencesInternal(arena, workspace, handle, fn_proto.ast.section_expr, decl, context, handler);
            }
            if (fn_proto.ast.callconv_expr != 0) {
                try symbolReferencesInternal(arena, workspace, handle, fn_proto.ast.callconv_expr, decl, context, handler);
            }
            if (node_tags[node] == .fn_decl) {
                try symbolReferencesInternal(arena, workspace, handle, datas[node].rhs, decl, context, handler);
            }
        },
        .anyframe_type => {
            try symbolReferencesInternal(arena, workspace, handle, datas[node].rhs, decl, context, handler);
        },
        .@"defer" => {
            try symbolReferencesInternal(arena, workspace, handle, datas[node].rhs, decl, context, handler);
        },
        .@"comptime" => {
            try symbolReferencesInternal(arena, workspace, handle, datas[node].lhs, decl, context, handler);
        },
        .@"nosuspend" => {
            try symbolReferencesInternal(arena, workspace, handle, datas[node].lhs, decl, context, handler);
        },
        .@"switch",
        .switch_comma,
        => {
            // TODO When renaming a union(enum) field, also rename switch items that refer to it.
            try symbolReferencesInternal(arena, workspace, handle, datas[node].lhs, decl, context, handler);
            const extra = tree.extraData(datas[node].rhs, Ast.Node.SubRange);
            const cases = tree.extra_data[extra.start..extra.end];
            for (cases) |case| {
                try symbolReferencesInternal(arena, workspace, handle, case, decl, context, handler);
            }
        },
        .switch_case_one => {
            const case_one = tree.switchCaseOne(node);
            if (case_one.ast.target_expr != 0)
                try symbolReferencesInternal(arena, workspace, handle, case_one.ast.target_expr, decl, context, handler);
            for (case_one.ast.values) |val|
                try symbolReferencesInternal(arena, workspace, handle, val, decl, context, handler);
        },
        .switch_case => {
            const case = tree.switchCase(node);
            if (case.ast.target_expr != 0)
                try symbolReferencesInternal(arena, workspace, handle, case.ast.target_expr, decl, context, handler);
            for (case.ast.values) |val|
                try symbolReferencesInternal(arena, workspace, handle, val, decl, context, handler);
        },
        .@"while",
        .while_simple,
        .while_cont,
        .for_simple,
        .@"for",
        => {
            const loop = ast.whileAst(tree, node).?;
            try symbolReferencesInternal(arena, workspace, handle, loop.ast.cond_expr, decl, context, handler);
            if (loop.ast.cont_expr != 0) {
                try symbolReferencesInternal(arena, workspace, handle, loop.ast.cont_expr, decl, context, handler);
            }
            try symbolReferencesInternal(arena, workspace, handle, loop.ast.then_expr, decl, context, handler);
            if (loop.ast.else_expr != 0) {
                try symbolReferencesInternal(arena, workspace, handle, loop.ast.else_expr, decl, context, handler);
            }
        },
        .@"if",
        .if_simple,
        => {
            const if_node = ast.ifFull(tree, node);

            try symbolReferencesInternal(arena, workspace, handle, if_node.ast.cond_expr, decl, context, handler);
            try symbolReferencesInternal(arena, workspace, handle, if_node.ast.then_expr, decl, context, handler);
            if (if_node.ast.else_expr != 0) {
                try symbolReferencesInternal(arena, workspace, handle, if_node.ast.else_expr, decl, context, handler);
            }
        },
        .array_type,
        .array_type_sentinel,
        => {
            try symbolReferencesInternal(arena, workspace, handle, datas[node].lhs, decl, context, handler);
            try symbolReferencesInternal(arena, workspace, handle, datas[node].rhs, decl, context, handler);
        },
        .ptr_type,
        .ptr_type_aligned,
        .ptr_type_bit_range,
        .ptr_type_sentinel,
        => {
            const ptr_type = ast.ptrType(tree, node).?;

            if (ptr_type.ast.align_node != 0) {
                try symbolReferencesInternal(arena, workspace, handle, ptr_type.ast.align_node, decl, context, handler);
                if (node_tags[node] == .ptr_type_bit_range) {
                    try symbolReferencesInternal(arena, workspace, handle, ptr_type.ast.bit_range_start, decl, context, handler);
                    try symbolReferencesInternal(arena, workspace, handle, ptr_type.ast.bit_range_end, decl, context, handler);
                }
            }
            if (ptr_type.ast.sentinel != 0) {
                try symbolReferencesInternal(arena, workspace, handle, ptr_type.ast.sentinel, decl, context, handler);
            }

            try symbolReferencesInternal(arena, workspace, handle, ptr_type.ast.child_type, decl, context, handler);
        },
        .address_of, .@"await", .bit_not, .bool_not, .optional_type, .negation, .negation_wrap, .@"resume", .@"try" => {
            try symbolReferencesInternal(arena, workspace, handle, datas[node].lhs, decl, context, handler);
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
                try symbolReferencesInternal(arena, workspace, handle, array_init.ast.type_expr, decl, context, handler);
            for (array_init.ast.elements) |e|
                try symbolReferencesInternal(arena, workspace, handle, e, decl, context, handler);
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
                try symbolReferencesInternal(arena, workspace, handle, struct_init.ast.type_expr, decl, context, handler);
            for (struct_init.ast.fields) |field|
                try symbolReferencesInternal(arena, workspace, handle, field, decl, context, handler);
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
                try symbolReferencesInternal(arena, workspace, handle, call.ast.fn_expr, decl, context, handler);

            for (call.ast.params) |param| {
                try symbolReferencesInternal(arena, workspace, handle, param, decl, context, handler);
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

            try symbolReferencesInternal(arena, workspace, handle, slice.ast.sliced, decl, context, handler);
            try symbolReferencesInternal(arena, workspace, handle, slice.ast.start, decl, context, handler);
            if (slice.ast.end != 0)
                try symbolReferencesInternal(arena, workspace, handle, slice.ast.end, decl, context, handler);
            if (slice.ast.sentinel != 0)
                try symbolReferencesInternal(arena, workspace, handle, slice.ast.sentinel, decl, context, handler);
        },
        .array_access => {
            try symbolReferencesInternal(arena, workspace, handle, datas[node].lhs, decl, context, handler);
            try symbolReferencesInternal(arena, workspace, handle, datas[node].rhs, decl, context, handler);
        },
        .deref,
        .unwrap_optional,
        => {
            try symbolReferencesInternal(arena, workspace, handle, datas[node].lhs, decl, context, handler);
        },
        .grouped_expression => {
            try symbolReferencesInternal(arena, workspace, handle, datas[node].lhs, decl, context, handler);
        },
        .@"return",
        .@"break",
        .@"continue",
        => {
            if (datas[node].lhs != 0) {
                try symbolReferencesInternal(arena, workspace, handle, datas[node].lhs, decl, context, handler);
            }
        },
        .@"suspend" => {
            if (datas[node].lhs != 0) {
                try symbolReferencesInternal(arena, workspace, handle, datas[node].lhs, decl, context, handler);
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
                try symbolReferencesInternal(arena, workspace, handle, param, decl, context, handler);
        },
        .@"asm",
        .asm_simple,
        => |a| {
            const _asm: Ast.full.Asm = if (a == .@"asm") tree.asmFull(node) else tree.asmSimple(node);
            if (_asm.ast.items.len == 0)
                try symbolReferencesInternal(arena, workspace, handle, _asm.ast.template, decl, context, handler);

            for (_asm.inputs) |input|
                try symbolReferencesInternal(arena, workspace, handle, input, decl, context, handler);

            for (_asm.outputs) |output|
                try symbolReferencesInternal(arena, workspace, handle, output, decl, context, handler);
        },
        .test_decl => {
            try symbolReferencesInternal(arena, workspace, handle, datas[node].rhs, decl, context, handler);
        },
        .field_access => {
            try symbolReferencesInternal(arena, workspace, handle, datas[node].lhs, decl, context, handler);

            const rhs_str = tree.tokenSlice(datas[node].rhs);
            var bound_type_params = TypeWithHandle.BoundTypeParams.init(arena.allocator());
            const left_type = try TypeWithHandle.resolveFieldAccessLhsType(
                arena,
                workspace,
                (try TypeWithHandle.resolveTypeOfNodeInternal(arena, workspace, handle, datas[node].lhs, &bound_type_params)) orelse return,
                &bound_type_params,
            );

            const left_type_node = switch (left_type.type.data) {
                .other => |n| n,
                else => return,
            };

            if (try DeclWithHandle.lookupSymbolContainer(
                arena,
                workspace,
                left_type.handle,
                left_type_node,
                rhs_str,
                !left_type.type.is_type_val,
            )) |child| {
                if (std.meta.eql(child, decl)) {
                    try tokenReference(handle, datas[node].rhs, context, handler);
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
            try symbolReferencesInternal(arena, workspace, handle, datas[node].lhs, decl, context, handler);
            try symbolReferencesInternal(arena, workspace, handle, datas[node].rhs, decl, context, handler);
        },
        else => {},
    }
}

pub fn symbolReferences(
    arena: *std.heap.ArenaAllocator,
    workspace: *Workspace,
    decl_handle: DeclWithHandle,
    include_decl: bool,
    context: anytype,
    comptime handler: anytype,
    skip_std_references: bool,
) !void {
    std.debug.assert(decl_handle.decl.* != .label_decl);
    const curr_handle = decl_handle.handle;
    if (include_decl) {
        try tokenReference(curr_handle, decl_handle.nameToken(), context, handler);
    }

    switch (decl_handle.decl.*) {
        .ast_node => {
            try symbolReferencesInternal(
                arena,
                workspace,
                curr_handle,
                0,
                decl_handle,
                context,
                handler,
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
                        const h = workspace.getDocument(uri) catch break;

                        if (h == curr_handle) {
                            // entry does import curr_handle
                            try symbolReferencesInternal(
                                arena,
                                workspace,
                                entry.value_ptr.*,
                                0,
                                decl_handle,
                                context,
                                handler,
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
                                    try symbolReferencesInternal(
                                        arena,
                                        workspace,
                                        curr_handle,
                                        curr_handle.tree.nodes.items(.data)[proto].rhs,
                                        decl_handle,
                                        context,
                                        handler,
                                    );
                                }
                                break :loop fn_proto;
                            }
                        }
                    },
                    else => {},
                }
            } else {
                log.warn("Could not find param decl's function", .{});
                return;
            };
            _ = fn_node;
        },
        .pointer_payload, .switch_payload, .array_payload, .array_index => {
            try symbolReferencesInternal(arena, workspace, curr_handle, 0, decl_handle, context, handler);
        },
        .label_decl => unreachable,
    }
}
