const std = @import("std");

pub fn getChildren(children: *std.ArrayList(u32), tree: *const std.zig.Ast, idx: u32) void {
    const tag = tree.nodes.items(.tag);
    const node_tag = tag[idx];
    const data = tree.nodes.items(.data);
    const node_data = data[idx];

    switch (node_tag) {
        .simple_var_decl => {
            const var_decl = tree.simpleVarDecl(idx);
            if (var_decl.ast.type_node != 0) {
                children.append(var_decl.ast.type_node) catch unreachable;
            }
            if (var_decl.ast.init_node != 0) {
                children.append(var_decl.ast.init_node) catch unreachable;
            }
        },
        .fn_decl => {
            // fn_proto
            children.append(node_data.lhs) catch unreachable;

            // body
            children.append(node_data.rhs) catch unreachable;
        },
        .builtin_call_two => {
            if (node_data.lhs != 0) {
                children.append(node_data.lhs) catch unreachable;
            }
            if (node_data.rhs != 0) {
                children.append(node_data.rhs) catch unreachable;
            }
        },
        .field_access => {
            children.append(node_data.lhs) catch unreachable;
        },
        .string_literal => {
            // leaf. no children
        },
        .block, .block_semicolon => {
            for (tree.extra_data[node_data.lhs..node_data.rhs]) |child| {
                children.append(child) catch unreachable;
            }
        },
        .block_two, .block_two_semicolon => {
            if (node_data.lhs != 0) {
                children.append(node_data.lhs) catch unreachable;
            }
            if (node_data.rhs != 0) {
                children.append(node_data.rhs) catch unreachable;
            }
        },
        .if_simple,
        => {
            children.append(node_data.lhs) catch unreachable;
            children.append(node_data.rhs) catch unreachable;
        },
        .@"if" =>{
            // const extra = tree.extraData(node_data.rhs, Node.If);
            const if_full = tree.ifFull(idx);
            children.append(if_full.ast.cond_expr) catch unreachable;
            children.append(if_full.ast.then_expr) catch unreachable;
            children.append(if_full.ast.else_expr) catch unreachable;
        },
        .while_simple => {
            children.append(node_data.lhs) catch unreachable;
            children.append(node_data.rhs) catch unreachable;
        },
        // .for_simple,
        .@"switch",
        .switch_comma,
        => {
            children.append(node_data.lhs) catch unreachable;
            const extra = tree.extraData(node_data.rhs, std.zig.Ast.Node.SubRange);
            for(tree.extra_data[extra.start..extra.end])|child|{
                children.append(child) catch unreachable;
            }
        },
        .switch_case
        => {
            const switch_case = tree.switchCase(idx);
            for(switch_case.ast.values)|child|
            {
                children.append(child) catch unreachable;
            }
            children.append(switch_case.ast.target_expr) catch unreachable;
        },
        .switch_case_one
        =>{
            const switch_case = tree.switchCaseOne(idx);
            for(switch_case.ast.values)|child|
            {
                children.append(child) catch unreachable;
            }
            children.append(switch_case.ast.target_expr) catch unreachable;
        },
        .struct_init_dot_comma =>{
            const struct_init = tree.structInitDot(idx);            
            for(struct_init.ast.fields)|child|
            {
                children.append(child) catch unreachable;
            }
        },
        else => {
            // std.debug.print("unknown node: {s}\n", .{@tagName(node_tag)});
        },
    }
}
