//! Collection of functions from std.zig.ast that we need
//! and may hit undefined in the standard library implementation
//! when there are parser errors.

const std = @import("std");
const Ast = std.zig.Ast;
const Node = Ast.Node;
const full = Ast.full;
const logger = std.log.scoped(.ast_helper);

fn fullPtrType(tree: Ast, info: full.PtrType.Components) full.PtrType {
    const token_tags = tree.tokens.items(.tag);
    // TODO: looks like stage1 isn't quite smart enough to handle enum
    // literals in some places here
    const Size = std.builtin.TypeInfo.Pointer.Size;
    const size: Size = switch (token_tags[info.main_token]) {
        .asterisk,
        .asterisk_asterisk,
        => switch (token_tags[info.main_token + 1]) {
            .r_bracket, .colon => .Many,
            .identifier => if (token_tags[info.main_token - 1] == .l_bracket) Size.C else .One,
            else => .One,
        },
        .l_bracket => Size.Slice,
        else => unreachable,
    };
    var result: full.PtrType = .{
        .size = size,
        .allowzero_token = null,
        .const_token = null,
        .volatile_token = null,
        .ast = info,
    };
    // We need to be careful that we don't iterate over any sub-expressions
    // here while looking for modifiers as that could result in false
    // positives. Therefore, start after a sentinel if there is one and
    // skip over any align node and bit range nodes.
    var i = if (info.sentinel != 0) lastToken(tree, info.sentinel) + 1 else info.main_token;
    const end = tree.firstToken(info.child_type);
    while (i < end) : (i += 1) {
        switch (token_tags[i]) {
            .keyword_allowzero => result.allowzero_token = i,
            .keyword_const => result.const_token = i,
            .keyword_volatile => result.volatile_token = i,
            .keyword_align => {
                std.debug.assert(info.align_node != 0);
                if (info.bit_range_end != 0) {
                    std.debug.assert(info.bit_range_start != 0);
                    i = lastToken(tree, info.bit_range_end) + 1;
                } else {
                    i = lastToken(tree, info.align_node) + 1;
                }
            },
            else => {},
        }
    }
    return result;
}

pub fn ptrTypeSimple(tree: Ast, node: Node.Index) full.PtrType {
    std.debug.assert(tree.nodes.items(.tag)[node] == .ptr_type);
    const data = tree.nodes.items(.data)[node];
    const extra = tree.extraData(data.lhs, Node.PtrType);
    return fullPtrType(tree, .{
        .main_token = tree.nodes.items(.main_token)[node],
        .align_node = extra.align_node,
        .addrspace_node = extra.addrspace_node,
        .sentinel = extra.sentinel,
        .bit_range_start = 0,
        .bit_range_end = 0,
        .child_type = data.rhs,
    });
}

pub fn ptrTypeSentinel(tree: Ast, node: Node.Index) full.PtrType {
    std.debug.assert(tree.nodes.items(.tag)[node] == .ptr_type_sentinel);
    const data = tree.nodes.items(.data)[node];
    return fullPtrType(tree, .{
        .main_token = tree.nodes.items(.main_token)[node],
        .align_node = 0,
        .addrspace_node = 0,
        .sentinel = data.lhs,
        .bit_range_start = 0,
        .bit_range_end = 0,
        .child_type = data.rhs,
    });
}

pub fn ptrTypeAligned(tree: Ast, node: Node.Index) full.PtrType {
    std.debug.assert(tree.nodes.items(.tag)[node] == .ptr_type_aligned);
    const data = tree.nodes.items(.data)[node];
    return fullPtrType(tree, .{
        .main_token = tree.nodes.items(.main_token)[node],
        .align_node = data.lhs,
        .addrspace_node = 0,
        .sentinel = 0,
        .bit_range_start = 0,
        .bit_range_end = 0,
        .child_type = data.rhs,
    });
}

pub fn ptrTypeBitRange(tree: Ast, node: Node.Index) full.PtrType {
    std.debug.assert(tree.nodes.items(.tag)[node] == .ptr_type_bit_range);
    const data = tree.nodes.items(.data)[node];
    const extra = tree.extraData(data.lhs, Node.PtrTypeBitRange);
    return fullPtrType(tree, .{
        .main_token = tree.nodes.items(.main_token)[node],
        .align_node = extra.align_node,
        .addrspace_node = extra.addrspace_node,
        .sentinel = extra.sentinel,
        .bit_range_start = extra.bit_range_start,
        .bit_range_end = extra.bit_range_end,
        .child_type = data.rhs,
    });
}

fn fullIf(tree: Ast, info: full.If.Components) full.If {
    const token_tags = tree.tokens.items(.tag);
    var result: full.If = .{
        .ast = info,
        .payload_token = null,
        .error_token = null,
        .else_token = undefined,
    };
    // if (cond_expr) |x|
    //              ^ ^
    const payload_pipe = lastToken(tree, info.cond_expr) + 2;
    if (token_tags[payload_pipe] == .pipe) {
        result.payload_token = payload_pipe + 1;
    }
    if (info.else_expr != 0) {
        // then_expr else |x|
        //           ^    ^
        result.else_token = lastToken(tree, info.then_expr) + 1;
        if (token_tags[result.else_token + 1] == .pipe) {
            result.error_token = result.else_token + 2;
        }
    }
    return result;
}

pub fn ifFull(tree: Ast, node: Node.Index) full.If {
    const data = tree.nodes.items(.data)[node];
    if (tree.nodes.items(.tag)[node] == .@"if") {
        const extra = tree.extraData(data.rhs, Node.If);
        return fullIf(tree, .{
            .cond_expr = data.lhs,
            .then_expr = extra.then_expr,
            .else_expr = extra.else_expr,
            .if_token = tree.nodes.items(.main_token)[node],
        });
    } else {
        std.debug.assert(tree.nodes.items(.tag)[node] == .if_simple);
        return fullIf(tree, .{
            .cond_expr = data.lhs,
            .then_expr = data.rhs,
            .else_expr = 0,
            .if_token = tree.nodes.items(.main_token)[node],
        });
    }
}

fn fullWhile(tree: Ast, info: full.While.Components) full.While {
    const token_tags = tree.tokens.items(.tag);
    var result: full.While = .{
        .ast = info,
        .inline_token = null,
        .label_token = null,
        .payload_token = null,
        .else_token = undefined,
        .error_token = null,
    };
    var tok_i = info.while_token - 1;
    if (token_tags[tok_i] == .keyword_inline) {
        result.inline_token = tok_i;
        tok_i -= 1;
    }
    if (token_tags[tok_i] == .colon and
        token_tags[tok_i - 1] == .identifier)
    {
        result.label_token = tok_i - 1;
    }
    const last_cond_token = lastToken(tree, info.cond_expr);
    if (token_tags[last_cond_token + 2] == .pipe) {
        result.payload_token = last_cond_token + 3;
    }
    if (info.else_expr != 0) {
        // then_expr else |x|
        //           ^    ^
        result.else_token = lastToken(tree, info.then_expr) + 1;
        if (token_tags[result.else_token + 1] == .pipe) {
            result.error_token = result.else_token + 2;
        }
    }
    return result;
}

pub fn whileSimple(tree: Ast, node: Node.Index) full.While {
    const data = tree.nodes.items(.data)[node];
    return fullWhile(tree, .{
        .while_token = tree.nodes.items(.main_token)[node],
        .cond_expr = data.lhs,
        .cont_expr = 0,
        .then_expr = data.rhs,
        .else_expr = 0,
    });
}

pub fn whileCont(tree: Ast, node: Node.Index) full.While {
    const data = tree.nodes.items(.data)[node];
    const extra = tree.extraData(data.rhs, Node.WhileCont);
    return fullWhile(tree, .{
        .while_token = tree.nodes.items(.main_token)[node],
        .cond_expr = data.lhs,
        .cont_expr = extra.cont_expr,
        .then_expr = extra.then_expr,
        .else_expr = 0,
    });
}

pub fn whileFull(tree: Ast, node: Node.Index) full.While {
    const data = tree.nodes.items(.data)[node];
    const extra = tree.extraData(data.rhs, Node.While);
    return fullWhile(tree, .{
        .while_token = tree.nodes.items(.main_token)[node],
        .cond_expr = data.lhs,
        .cont_expr = extra.cont_expr,
        .then_expr = extra.then_expr,
        .else_expr = extra.else_expr,
    });
}

pub fn forSimple(tree: Ast, node: Node.Index) full.While {
    const data = tree.nodes.items(.data)[node];
    return fullWhile(tree, .{
        .while_token = tree.nodes.items(.main_token)[node],
        .cond_expr = data.lhs,
        .cont_expr = 0,
        .then_expr = data.rhs,
        .else_expr = 0,
    });
}

pub fn forFull(tree: Ast, node: Node.Index) full.While {
    const data = tree.nodes.items(.data)[node];
    const extra = tree.extraData(data.rhs, Node.If);
    return fullWhile(tree, .{
        .while_token = tree.nodes.items(.main_token)[node],
        .cond_expr = data.lhs,
        .cont_expr = 0,
        .then_expr = extra.then_expr,
        .else_expr = extra.else_expr,
    });
}

pub fn lastToken(tree: Ast, node: Ast.Node.Index) Ast.TokenIndex {
    const TokenIndex = Ast.TokenIndex;
    const tags = tree.nodes.items(.tag);
    const datas = tree.nodes.items(.data);
    const main_tokens = tree.nodes.items(.main_token);
    const token_starts = tree.tokens.items(.start);
    const token_tags = tree.tokens.items(.tag);
    var n = node;
    var end_offset: TokenIndex = 0;
    while (true) switch (tags[n]) {
        .root => return @intCast(TokenIndex, tree.tokens.len - 1),
        .@"usingnamespace" => {
            // lhs is the expression
            if (datas[n].lhs == 0) {
                return main_tokens[n] + end_offset;
            } else {
                n = datas[n].lhs;
            }
        },
        .test_decl => {
            // rhs is the block
            // lhs is the name
            if (datas[n].rhs != 0) {
                n = datas[n].rhs;
            } else if (datas[n].lhs != 0) {
                n = datas[n].lhs;
            } else {
                return main_tokens[n] + end_offset;
            }
        },
        .global_var_decl => {
            // rhs is init node
            if (datas[n].rhs != 0) {
                n = datas[n].rhs;
            } else {
                const extra = tree.extraData(datas[n].lhs, Node.GlobalVarDecl);
                if (extra.section_node != 0) {
                    end_offset += 1; // for the rparen
                    n = extra.section_node;
                } else if (extra.align_node != 0) {
                    end_offset += 1; // for the rparen
                    n = extra.align_node;
                } else if (extra.type_node != 0) {
                    n = extra.type_node;
                } else {
                    end_offset += 1; // from mut token to name
                    return main_tokens[n] + end_offset;
                }
            }
        },
        .local_var_decl => {
            // rhs is init node
            if (datas[n].rhs != 0) {
                n = datas[n].rhs;
            } else {
                const extra = tree.extraData(datas[n].lhs, Node.LocalVarDecl);
                if (extra.align_node != 0) {
                    end_offset += 1; // for the rparen
                    n = extra.align_node;
                } else if (extra.type_node != 0) {
                    n = extra.type_node;
                } else {
                    end_offset += 1; // from mut token to name
                    return main_tokens[n] + end_offset;
                }
            }
        },
        .simple_var_decl => {
            // rhs is init node
            if (datas[n].rhs != 0) {
                n = datas[n].rhs;
            } else if (datas[n].lhs != 0) {
                n = datas[n].lhs;
            } else {
                end_offset += 1; // from mut token to name
                return main_tokens[n] + end_offset;
            }
        },
        .aligned_var_decl => {
            // rhs is init node, lhs is align node
            if (datas[n].rhs != 0) {
                n = datas[n].rhs;
            } else if (datas[n].lhs != 0) {
                end_offset += 1; // for the rparen
                n = datas[n].lhs;
            } else {
                end_offset += 1; // from mut token to name
                return main_tokens[n] + end_offset;
            }
        },
        .@"errdefer" => {
            // lhs is the token payload, rhs is the expression
            if (datas[n].rhs != 0) {
                n = datas[n].rhs;
            } else if (datas[n].lhs != 0) {
                // right pipe
                end_offset += 1;
                n = datas[n].lhs;
            } else {
                return main_tokens[n] + end_offset;
            }
        },
        .@"defer" => {
            // rhs is the defered expr
            if (datas[n].rhs != 0) {
                n = datas[n].rhs;
            } else {
                return main_tokens[n] + end_offset;
            }
        },

        .bool_not,
        .negation,
        .bit_not,
        .negation_wrap,
        .address_of,
        .@"try",
        .@"await",
        .optional_type,
        .@"resume",
        .@"nosuspend",
        .@"comptime",
        => n = datas[n].lhs,

        .@"catch",
        .equal_equal,
        .bang_equal,
        .less_than,
        .greater_than,
        .less_or_equal,
        .greater_or_equal,
        .assign_mul,
        .assign_div,
        .assign_mod,
        .assign_add,
        .assign_sub,
        .assign_shl,
        .assign_shr,
        .assign_bit_and,
        .assign_bit_xor,
        .assign_bit_or,
        .assign_mul_wrap,
        .assign_add_wrap,
        .assign_sub_wrap,
        .assign_mul_sat,
        .assign_add_sat,
        .assign_sub_sat,
        .assign_shl_sat,
        .assign,
        .merge_error_sets,
        .mul,
        .div,
        .mod,
        .array_mult,
        .mul_wrap,
        .mul_sat,
        .add,
        .sub,
        .array_cat,
        .add_wrap,
        .sub_wrap,
        .add_sat,
        .sub_sat,
        .shl,
        .shl_sat,
        .shr,
        .bit_and,
        .bit_xor,
        .bit_or,
        .@"orelse",
        .bool_and,
        .bool_or,
        .anyframe_type,
        .error_union,
        .if_simple,
        .while_simple,
        .for_simple,
        .ptr_type_aligned,
        .ptr_type_sentinel,
        .ptr_type,
        .ptr_type_bit_range,
        .array_type,
        .switch_case_one,
        .switch_case,
        .switch_range,
        => n = datas[n].rhs,

        .field_access,
        .unwrap_optional,
        .grouped_expression,
        .multiline_string_literal,
        .error_set_decl,
        .asm_simple,
        .asm_output,
        .asm_input,
        => return datas[n].rhs + end_offset,

        .error_value => {
            if (datas[n].rhs != 0) {
                return datas[n].rhs + end_offset;
            } else if (datas[n].lhs != 0) {
                return datas[n].lhs + end_offset;
            } else {
                return main_tokens[n] + end_offset;
            }
        },

        .anyframe_literal,
        .char_literal,
        .integer_literal,
        .float_literal,
        .unreachable_literal,
        .identifier,
        .deref,
        .enum_literal,
        .string_literal,
        => return main_tokens[n] + end_offset,

        .@"return" => if (datas[n].lhs != 0) {
            n = datas[n].lhs;
        } else {
            return main_tokens[n] + end_offset;
        },

        .call, .async_call => {
            end_offset += 1; // for the rparen
            const params = tree.extraData(datas[n].rhs, Node.SubRange);
            if (params.end - params.start == 0) {
                return main_tokens[n] + end_offset;
            }
            n = tree.extra_data[params.end - 1]; // last parameter
        },
        .tagged_union_enum_tag => {
            const members = tree.extraData(datas[n].rhs, Node.SubRange);
            if (members.end - members.start == 0) {
                end_offset += 4; // for the rparen + rparen + lbrace + rbrace
                n = datas[n].lhs;
            } else {
                end_offset += 1; // for the rbrace
                n = tree.extra_data[members.end - 1]; // last parameter
            }
        },
        .call_comma,
        .async_call_comma,
        .tagged_union_enum_tag_trailing,
        => {
            end_offset += 2; // for the comma/semicolon + rparen/rbrace
            const params = tree.extraData(datas[n].rhs, Node.SubRange);
            std.debug.assert(params.end > params.start);
            n = tree.extra_data[params.end - 1]; // last parameter
        },
        .@"switch" => {
            const cases = tree.extraData(datas[n].rhs, Node.SubRange);
            if (cases.end - cases.start == 0) {
                end_offset += 3; // rparen, lbrace, rbrace
                n = datas[n].lhs; // condition expression
            } else {
                end_offset += 1; // for the rbrace
                n = tree.extra_data[cases.end - 1]; // last case
            }
        },
        .container_decl_arg => {
            const members = tree.extraData(datas[n].rhs, Node.SubRange);
            if (members.end - members.start == 0) {
                end_offset += 3; // for the rparen + lbrace + rbrace
                n = datas[n].lhs;
            } else {
                end_offset += 1; // for the rbrace
                n = tree.extra_data[members.end - 1]; // last parameter
            }
        },
        .@"asm" => {
            const extra = tree.extraData(datas[n].rhs, Node.Asm);
            return extra.rparen + end_offset;
        },
        .array_init,
        .struct_init,
        => {
            const elements = tree.extraData(datas[n].rhs, Node.SubRange);
            std.debug.assert(elements.end - elements.start > 0);
            end_offset += 1; // for the rbrace
            n = tree.extra_data[elements.end - 1]; // last element
        },
        .array_init_comma,
        .struct_init_comma,
        .container_decl_arg_trailing,
        .switch_comma,
        => {
            const members = tree.extraData(datas[n].rhs, Node.SubRange);
            std.debug.assert(members.end - members.start > 0);
            end_offset += 2; // for the comma + rbrace
            n = tree.extra_data[members.end - 1]; // last parameter
        },
        .array_init_dot,
        .struct_init_dot,
        .block,
        .container_decl,
        .tagged_union,
        .builtin_call,
        => {
            std.debug.assert(datas[n].rhs - datas[n].lhs > 0);
            end_offset += 1; // for the rbrace
            n = tree.extra_data[datas[n].rhs - 1]; // last statement
        },
        .array_init_dot_comma,
        .struct_init_dot_comma,
        .block_semicolon,
        .container_decl_trailing,
        .tagged_union_trailing,
        .builtin_call_comma,
        => {
            std.debug.assert(datas[n].rhs - datas[n].lhs > 0);
            end_offset += 2; // for the comma/semicolon + rbrace/rparen
            n = tree.extra_data[datas[n].rhs - 1]; // last member
        },
        .call_one,
        .async_call_one,
        .array_access,
        => {
            end_offset += 1; // for the rparen/rbracket
            if (datas[n].rhs == 0) {
                return main_tokens[n] + end_offset;
            }
            n = datas[n].rhs;
        },
        .array_init_dot_two,
        .block_two,
        .builtin_call_two,
        .struct_init_dot_two,
        .container_decl_two,
        .tagged_union_two,
        => {
            if (datas[n].rhs != 0) {
                end_offset += 1; // for the rparen/rbrace
                n = datas[n].rhs;
            } else if (datas[n].lhs != 0) {
                end_offset += 1; // for the rparen/rbrace
                n = datas[n].lhs;
            } else {
                switch (tags[n]) {
                    .array_init_dot_two,
                    .block_two,
                    .struct_init_dot_two,
                    => end_offset += 1, // rbrace
                    .builtin_call_two => end_offset += 2, // lparen/lbrace + rparen/rbrace
                    .container_decl_two => {
                        var i: u32 = 2; // lbrace + rbrace
                        while (token_tags[main_tokens[n] + i] == .container_doc_comment) i += 1;
                        end_offset += i;
                    },
                    .tagged_union_two => {
                        var i: u32 = 5; // (enum) {}
                        while (token_tags[main_tokens[n] + i] == .container_doc_comment) i += 1;
                        end_offset += i;
                    },
                    else => unreachable,
                }
                return main_tokens[n] + end_offset;
            }
        },
        .array_init_dot_two_comma,
        .builtin_call_two_comma,
        .block_two_semicolon,
        .struct_init_dot_two_comma,
        .container_decl_two_trailing,
        .tagged_union_two_trailing,
        => {
            end_offset += 2; // for the comma/semicolon + rbrace/rparen
            if (datas[n].rhs != 0) {
                n = datas[n].rhs;
            } else if (datas[n].lhs != 0) {
                n = datas[n].lhs;
            } else {
                return main_tokens[n] + end_offset; // returns { }
            }
        },
        .container_field_init => {
            if (datas[n].rhs != 0) {
                n = datas[n].rhs;
            } else if (datas[n].lhs != 0) {
                n = datas[n].lhs;
            } else {
                return main_tokens[n] + end_offset;
            }
        },
        .container_field_align => {
            if (datas[n].rhs != 0) {
                end_offset += 1; // for the rparen
                n = datas[n].rhs;
            } else if (datas[n].lhs != 0) {
                n = datas[n].lhs;
            } else {
                return main_tokens[n] + end_offset;
            }
        },
        .container_field => {
            const extra = tree.extraData(datas[n].rhs, Node.ContainerField);
            if (extra.value_expr != 0) {
                n = extra.value_expr;
            } else if (extra.align_expr != 0) {
                end_offset += 1; // for the rparen
                n = extra.align_expr;
            } else if (datas[n].lhs != 0) {
                n = datas[n].lhs;
            } else {
                return main_tokens[n] + end_offset;
            }
        },

        .array_init_one,
        .struct_init_one,
        => {
            end_offset += 1; // rbrace
            if (datas[n].rhs == 0) {
                return main_tokens[n] + end_offset;
            } else {
                n = datas[n].rhs;
            }
        },
        .slice_open,
        .call_one_comma,
        .async_call_one_comma,
        .array_init_one_comma,
        .struct_init_one_comma,
        => {
            end_offset += 2; // ellipsis2 + rbracket, or comma + rparen
            n = datas[n].rhs;
            std.debug.assert(n != 0);
        },
        .slice => {
            const extra = tree.extraData(datas[n].rhs, Node.Slice);
            std.debug.assert(extra.end != 0); // should have used slice_open
            end_offset += 1; // rbracket
            n = extra.end;
        },
        .slice_sentinel => {
            const extra = tree.extraData(datas[n].rhs, Node.SliceSentinel);
            if (extra.sentinel != 0) {
                end_offset += 1; // right bracket
                n = extra.sentinel;
            } else if (extra.end != 0) {
                end_offset += 2; // colon, right bracket
                n = extra.end;
            } else {
                // Assume both sentinel and end are completely devoid of tokens
                end_offset += 3; // ellipsis, colon, right bracket
                n = extra.start;
            }
        },

        .@"continue" => {
            if (datas[n].lhs != 0) {
                return datas[n].lhs + end_offset;
            } else {
                return main_tokens[n] + end_offset;
            }
        },
        .@"break" => {
            if (datas[n].rhs != 0) {
                n = datas[n].rhs;
            } else if (datas[n].lhs != 0) {
                return datas[n].lhs + end_offset;
            } else {
                return main_tokens[n] + end_offset;
            }
        },
        .fn_decl => {
            if (datas[n].rhs != 0) {
                n = datas[n].rhs;
            } else {
                n = datas[n].lhs;
            }
        },
        .fn_proto_multi => {
            const extra = tree.extraData(datas[n].lhs, Node.SubRange);
            // rhs can be 0 when no return type is provided
            if (datas[n].rhs != 0) {
                n = datas[n].rhs;
            } else {
                // Use the last argument and skip right paren
                n = tree.extra_data[extra.end - 1];
                end_offset += 1;
            }
        },
        .fn_proto_simple => {
            // rhs can be 0 when no return type is provided
            // lhs can be 0 when no parameter is provided
            if (datas[n].rhs != 0) {
                n = datas[n].rhs;
            } else if (datas[n].lhs != 0) {
                n = datas[n].lhs;
                // Skip right paren
                end_offset += 1;
            } else {
                // Skip left and right paren
                return main_tokens[n] + end_offset + 2;
            }
        },
        .fn_proto_one => {
            const extra = tree.extraData(datas[n].lhs, Node.FnProtoOne);
            // addrspace, linksection, callconv, align can appear in any order, so we
            // find the last one here.
            // rhs can be zero if no return type is provided
            var max_node: Node.Index = 0;
            var max_start: u32 = 0;
            if (datas[n].rhs != 0) {
                max_node = datas[n].rhs;
                max_start = token_starts[main_tokens[max_node]];
            }

            var max_offset: TokenIndex = 0;
            if (extra.align_expr != 0) {
                const start = token_starts[main_tokens[extra.align_expr]];
                if (start > max_start) {
                    max_node = extra.align_expr;
                    max_start = start;
                    max_offset = 1; // for the rparen
                }
            }
            if (extra.addrspace_expr != 0) {
                const start = token_starts[main_tokens[extra.addrspace_expr]];
                if (start > max_start) {
                    max_node = extra.addrspace_expr;
                    max_start = start;
                    max_offset = 1; // for the rparen
                }
            }
            if (extra.section_expr != 0) {
                const start = token_starts[main_tokens[extra.section_expr]];
                if (start > max_start) {
                    max_node = extra.section_expr;
                    max_start = start;
                    max_offset = 1; // for the rparen
                }
            }
            if (extra.callconv_expr != 0) {
                const start = token_starts[main_tokens[extra.callconv_expr]];
                if (start > max_start) {
                    max_node = extra.callconv_expr;
                    max_start = start;
                    max_offset = 1; // for the rparen
                }
            }

            if (max_node == 0) {
                std.debug.assert(max_offset == 0);
                // No linksection, callconv, align, return type
                if (extra.param != 0) {
                    n = extra.param;
                    end_offset += 1;
                } else {
                    // Skip left and right parens
                    return main_tokens[n] + end_offset + 2;
                }
            } else {
                n = max_node;
                end_offset += max_offset;
            }
        },
        .fn_proto => {
            const extra = tree.extraData(datas[n].lhs, Node.FnProto);
            // addrspace, linksection, callconv, align can appear in any order, so we
            // find the last one here.
            // rhs can be zero if no return type is provided
            var max_node: Node.Index = 0;
            var max_start: u32 = 0;
            if (datas[n].rhs != 0) {
                max_node = datas[n].rhs;
                max_start = token_starts[main_tokens[max_node]];
            }

            var max_offset: TokenIndex = 0;
            if (extra.align_expr != 0) {
                const start = token_starts[main_tokens[extra.align_expr]];
                if (start > max_start) {
                    max_node = extra.align_expr;
                    max_start = start;
                    max_offset = 1; // for the rparen
                }
            }
            if (extra.addrspace_expr != 0) {
                const start = token_starts[main_tokens[extra.addrspace_expr]];
                if (start > max_start) {
                    max_node = extra.addrspace_expr;
                    max_start = start;
                    max_offset = 1; // for the rparen
                }
            }
            if (extra.section_expr != 0) {
                const start = token_starts[main_tokens[extra.section_expr]];
                if (start > max_start) {
                    max_node = extra.section_expr;
                    max_start = start;
                    max_offset = 1; // for the rparen
                }
            }
            if (extra.callconv_expr != 0) {
                const start = token_starts[main_tokens[extra.callconv_expr]];
                if (start > max_start) {
                    max_node = extra.callconv_expr;
                    max_start = start;
                    max_offset = 1; // for the rparen
                }
            }
            if (max_node == 0) {
                std.debug.assert(max_offset == 0);
                // No linksection, callconv, align, return type
                // Use the last parameter and skip one extra token for the right paren
                n = extra.params_end;
                end_offset += 1;
            } else {
                n = max_node;
                end_offset += max_offset;
            }
        },
        .while_cont => {
            const extra = tree.extraData(datas[n].rhs, Node.WhileCont);
            std.debug.assert(extra.then_expr != 0);
            n = extra.then_expr;
        },
        .@"while" => {
            const extra = tree.extraData(datas[n].rhs, Node.While);
            std.debug.assert(extra.else_expr != 0);
            n = extra.else_expr;
        },
        .@"if", .@"for" => {
            const extra = tree.extraData(datas[n].rhs, Node.If);
            std.debug.assert(extra.else_expr != 0);
            n = extra.else_expr;
        },
        .@"suspend" => {
            if (datas[n].lhs != 0) {
                n = datas[n].lhs;
            } else {
                return main_tokens[n] + end_offset;
            }
        },
        .array_type_sentinel => {
            const extra = tree.extraData(datas[n].rhs, Node.ArrayTypeSentinel);
            n = extra.elem_type;
        },
    };
}

pub fn containerField(tree: Ast, node: Ast.Node.Index) ?Ast.full.ContainerField {
    return switch (tree.nodes.items(.tag)[node]) {
        .container_field => tree.containerField(node),
        .container_field_init => tree.containerFieldInit(node),
        .container_field_align => tree.containerFieldAlign(node),
        else => null,
    };
}

pub fn ptrType(tree: Ast, node: Ast.Node.Index) ?Ast.full.PtrType {
    return switch (tree.nodes.items(.tag)[node]) {
        .ptr_type => ptrTypeSimple(tree, node),
        .ptr_type_aligned => ptrTypeAligned(tree, node),
        .ptr_type_bit_range => ptrTypeBitRange(tree, node),
        .ptr_type_sentinel => ptrTypeSentinel(tree, node),
        else => null,
    };
}

pub fn whileAst(tree: Ast, node: Ast.Node.Index) ?Ast.full.While {
    return switch (tree.nodes.items(.tag)[node]) {
        .@"while" => whileFull(tree, node),
        .while_simple => whileSimple(tree, node),
        .while_cont => whileCont(tree, node),
        .@"for" => forFull(tree, node),
        .for_simple => forSimple(tree, node),
        else => null,
    };
}

pub fn isContainer(tree: Ast, node: Ast.Node.Index) bool {
    return switch (tree.nodes.items(.tag)[node]) {
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
        => true,
        else => false,
    };
}

/// Returns the member indices of a given declaration container.
/// Asserts given `tag` is a container node
pub fn declMembers(tree: Ast, node_idx: Ast.Node.Index, buffer: *[2]Ast.Node.Index) []const Ast.Node.Index {
    std.debug.assert(isContainer(tree, node_idx));
    return switch (tree.nodes.items(.tag)[node_idx]) {
        .container_decl, .container_decl_trailing => tree.containerDecl(node_idx).ast.members,
        .container_decl_arg, .container_decl_arg_trailing => tree.containerDeclArg(node_idx).ast.members,
        .container_decl_two, .container_decl_two_trailing => tree.containerDeclTwo(buffer, node_idx).ast.members,
        .tagged_union, .tagged_union_trailing => tree.taggedUnion(node_idx).ast.members,
        .tagged_union_enum_tag, .tagged_union_enum_tag_trailing => tree.taggedUnionEnumTag(node_idx).ast.members,
        .tagged_union_two, .tagged_union_two_trailing => tree.taggedUnionTwo(buffer, node_idx).ast.members,
        .root => tree.rootDecls(),
        .error_set_decl => &[_]Ast.Node.Index{},
        else => unreachable,
    };
}

/// Returns an `ast.full.VarDecl` for a given node index.
/// Returns null if the tag doesn't match
pub fn varDecl(tree: Ast, node_idx: Ast.Node.Index) ?Ast.full.VarDecl {
    return switch (tree.nodes.items(.tag)[node_idx]) {
        .global_var_decl => tree.globalVarDecl(node_idx),
        .local_var_decl => tree.localVarDecl(node_idx),
        .aligned_var_decl => tree.alignedVarDecl(node_idx),
        .simple_var_decl => tree.simpleVarDecl(node_idx),
        else => null,
    };
}

pub fn isBuiltinCall(tree: Ast, node: Ast.Node.Index) bool {
    return switch (tree.nodes.items(.tag)[node]) {
        .builtin_call,
        .builtin_call_comma,
        .builtin_call_two,
        .builtin_call_two_comma,
        => true,
        else => false,
    };
}

pub fn isCall(tree: Ast, node: Ast.Node.Index) bool {
    return switch (tree.nodes.items(.tag)[node]) {
        .call,
        .call_comma,
        .call_one,
        .call_one_comma,
        .async_call,
        .async_call_comma,
        .async_call_one,
        .async_call_one_comma,
        => true,
        else => false,
    };
}

pub fn fnProto(tree: Ast, node: Ast.Node.Index, buf: *[1]Ast.Node.Index) ?Ast.full.FnProto {
    return switch (tree.nodes.items(.tag)[node]) {
        .fn_proto => tree.fnProto(node),
        .fn_proto_multi => tree.fnProtoMulti(node),
        .fn_proto_one => tree.fnProtoOne(buf, node),
        .fn_proto_simple => tree.fnProtoSimple(buf, node),
        .fn_decl => fnProto(tree, tree.nodes.items(.data)[node].lhs, buf),
        else => null,
    };
}

pub fn callFull(tree: Ast, node: Ast.Node.Index, buf: *[1]Ast.Node.Index) ?Ast.full.Call {
    return switch (tree.nodes.items(.tag)[node]) {
        .call,
        .call_comma,
        .async_call,
        .async_call_comma,
        => tree.callFull(node),
        .call_one,
        .call_one_comma,
        .async_call_one,
        .async_call_one_comma,
        => tree.callOne(buf, node),
        else => null,
    };
}

// STYLE

pub fn isCamelCase(name: []const u8) bool {
    return !std.ascii.isUpper(name[0]) and !isSnakeCase(name);
}

pub fn isPascalCase(name: []const u8) bool {
    return std.ascii.isUpper(name[0]) and !isSnakeCase(name);
}

pub fn isSnakeCase(name: []const u8) bool {
    return std.mem.indexOf(u8, name, "_") != null;
}

pub fn getDeclNameToken(tree: Ast, node: Ast.Node.Index) ?Ast.TokenIndex {
    const tags = tree.nodes.items(.tag);
    const main_token = tree.nodes.items(.main_token)[node];
    return switch (tags[node]) {
        // regular declaration names. + 1 to mut token because name comes after 'const'/'var'
        .local_var_decl => tree.localVarDecl(node).ast.mut_token + 1,
        .global_var_decl => tree.globalVarDecl(node).ast.mut_token + 1,
        .simple_var_decl => tree.simpleVarDecl(node).ast.mut_token + 1,
        .aligned_var_decl => tree.alignedVarDecl(node).ast.mut_token + 1,
        // function declaration names
        .fn_proto,
        .fn_proto_multi,
        .fn_proto_one,
        .fn_proto_simple,
        .fn_decl,
        => blk: {
            var params: [1]Ast.Node.Index = undefined;
            break :blk fnProto(tree, node, &params).?.name_token;
        },

        // containers
        .container_field => tree.containerField(node).ast.name_token,
        .container_field_init => tree.containerFieldInit(node).ast.name_token,
        .container_field_align => tree.containerFieldAlign(node).ast.name_token,

        .identifier => main_token,
        .error_value => main_token + 2, // 'error'.<main_token +2>

        // lhs of main token is name token, so use `node` - 1
        .test_decl => if (tree.tokens.items(.tag)[main_token + 1] == .string_literal)
            return main_token + 1
        else
            null,
        else => null,
    };
}

fn hasComment(tree: Ast.Tree, start_token: Ast.TokenIndex, end_token: Ast.TokenIndex) bool {
    const token_starts = tree.tokens.items(.start);

    const start = token_starts[start_token];
    const end = token_starts[end_token];

    return std.mem.indexOf(u8, tree.source[start..end], "//") != null;
}

pub const MarkupFormat = enum(u1) {
    PlainText = 0,
    Markdown = 1,
};

/// Gets a declaration's doc comments. Caller owns returned memory.
pub fn getDocComments(allocator: std.mem.Allocator, tree: Ast, node: Ast.Node.Index, format: MarkupFormat) !?[]const u8 {
    const base = tree.nodes.items(.main_token)[node];
    const base_kind = tree.nodes.items(.tag)[node];
    const tokens = tree.tokens.items(.tag);

    switch (base_kind) {
        // As far as I know, this does not actually happen yet, but it
        // may come in useful.
        .root => return try collectDocComments(allocator, tree, 0, format, true),
        .fn_proto,
        .fn_proto_one,
        .fn_proto_simple,
        .fn_proto_multi,
        .fn_decl,
        .local_var_decl,
        .global_var_decl,
        .aligned_var_decl,
        .simple_var_decl,
        .container_field_init,
        => {
            if (getDocCommentTokenIndex(tokens, base)) |doc_comment_index|
                return try collectDocComments(allocator, tree, doc_comment_index, format, false);
        },
        else => {},
    }
    return null;
}

/// Get the first doc comment of a declaration.
pub fn getDocCommentTokenIndex(tokens: []std.zig.Token.Tag, base_token: Ast.TokenIndex) ?Ast.TokenIndex {
    var idx = base_token;
    if (idx == 0) return null;
    idx -= 1;
    if (tokens[idx] == .keyword_threadlocal and idx > 0) idx -= 1;
    if (tokens[idx] == .string_literal and idx > 1 and tokens[idx - 1] == .keyword_extern) idx -= 1;
    if (tokens[idx] == .keyword_extern and idx > 0) idx -= 1;
    if (tokens[idx] == .keyword_export and idx > 0) idx -= 1;
    if (tokens[idx] == .keyword_inline and idx > 0) idx -= 1;
    if (tokens[idx] == .keyword_pub and idx > 0) idx -= 1;

    // Find first doc comment token
    if (!(tokens[idx] == .doc_comment))
        return null;
    return while (tokens[idx] == .doc_comment) {
        if (idx == 0) break 0;
        idx -= 1;
    } else idx + 1;
}

pub fn collectDocComments(allocator: std.mem.Allocator, tree: Ast, doc_comments: Ast.TokenIndex, format: MarkupFormat, container_doc: bool) ![]const u8 {
    var lines = std.ArrayList([]const u8).init(allocator);
    defer lines.deinit();
    const tokens = tree.tokens.items(.tag);

    var curr_line_tok = doc_comments;
    while (true) : (curr_line_tok += 1) {
        const comm = tokens[curr_line_tok];
        if ((container_doc and comm == .container_doc_comment) or (!container_doc and comm == .doc_comment)) {
            try lines.append(std.mem.trim(u8, tree.tokenSlice(curr_line_tok)[3..], &std.ascii.spaces));
        } else break;
    }

    return try std.mem.join(allocator, if (format == .Markdown) "  \n" else "\n", lines.items);
}

pub fn nodeToString(tree: Ast, node: Ast.Node.Index) ?[]const u8 {
    const data = tree.nodes.items(.data);
    const main_token = tree.nodes.items(.main_token)[node];
    var buf: [1]Ast.Node.Index = undefined;
    switch (tree.nodes.items(.tag)[node]) {
        .container_field => return tree.tokenSlice(tree.containerField(node).ast.name_token),
        .container_field_init => return tree.tokenSlice(tree.containerFieldInit(node).ast.name_token),
        .container_field_align => return tree.tokenSlice(tree.containerFieldAlign(node).ast.name_token),
        .error_value => return tree.tokenSlice(data[node].rhs),
        .identifier => return tree.tokenSlice(main_token),
        .fn_proto,
        .fn_proto_multi,
        .fn_proto_one,
        .fn_proto_simple,
        .fn_decl,
        => if (fnProto(tree, node, &buf).?.name_token) |name|
            return tree.tokenSlice(name),
        .field_access => return tree.tokenSlice(data[node].rhs),
        .call,
        .call_comma,
        .async_call,
        .async_call_comma,
        => return tree.tokenSlice(tree.callFull(node).ast.lparen - 1),
        .call_one,
        .call_one_comma,
        .async_call_one,
        .async_call_one_comma,
        => return tree.tokenSlice(tree.callOne(&buf, node).ast.lparen - 1),
        .test_decl => if (data[node].lhs != 0)
            return tree.tokenSlice(data[node].lhs),
        else => |tag| logger.debug("INVALID: {}", .{tag}),
    }

    return null;
}

pub const Loc = struct {
    start: usize,
    end: usize,
};

pub fn tokenLocation(tree: Ast, token_index: Ast.TokenIndex) Loc {
    const start = tree.tokens.items(.start)[token_index];
    const tag = tree.tokens.items(.tag)[token_index];

    // For some tokens, re-tokenization is needed to find the end.
    var tokenizer: std.zig.Tokenizer = .{
        .buffer = tree.source,
        .index = start,
        .pending_invalid_token = null,
    };

    const token = tokenizer.next();
    std.debug.assert(token.tag == tag);
    return .{ .start = token.loc.start, .end = token.loc.end };
}

pub fn getVariableSignature(tree: Ast, var_decl: Ast.full.VarDecl) []const u8 {
    const start = tokenLocation(tree, var_decl.ast.mut_token).start;
    const end = tokenLocation(tree, lastToken(tree, var_decl.ast.init_node)).end;
    return tree.source[start..end];
}

pub fn getContainerFieldSignature(tree: Ast, field: Ast.full.ContainerField) []const u8 {
    const start = tokenLocation(tree, field.ast.name_token).start;
    const end_node = if (field.ast.value_expr != 0) field.ast.value_expr else field.ast.type_expr;
    const end = tokenLocation(tree, lastToken(tree, end_node)).end;
    return tree.source[start..end];
}

/// Gets a function's keyword, name, arguments and return value.
pub fn getFunctionSignature(tree: Ast, func: Ast.full.FnProto) []const u8 {
    const start = tokenLocation(tree, func.ast.fn_token);

    const end = if (func.ast.return_type != 0)
        tokenLocation(tree, lastToken(tree, func.ast.return_type))
    else
        start;
    return tree.source[start.start..end.end];
}

/// Asserts the token is comprised of valid utf8
pub fn tokenLength(tree: Ast, token: Ast.TokenIndex) usize {
    const token_loc = tokenLocation(tree, token);
    return token_loc.end - token_loc.start;
}
