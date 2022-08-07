const std = @import("std");
const Ast = std.zig.Ast;
const lsp = @import("lsp");
const ws = @import("workspace");
const astutil = @import("astutil");
const Config = ws.Config;
const Document = ws.Document;
const Line = ws.Line;
const TypeWithHandle = ws.TypeWithHandle;
const AstNode = astutil.AstNode;
const AstNodeIterator = astutil.AstNodeIterator;
const AstToken = astutil.AstToken;
const VarType = astutil.VarType;
const ast = astutil.ast;

// TODO: Is this correct or can we get a better end?
fn astLocationToRange(loc: Ast.Location) lsp.Range {
    return .{
        .start = .{
            .line = @intCast(i64, loc.line),
            .character = @intCast(i64, loc.column),
        },
        .end = .{
            .line = @intCast(i64, loc.line),
            .character = @intCast(i64, loc.column),
        },
    };
}

pub fn createNotifyDiagnostics(arena: *std.heap.ArenaAllocator, doc: *const Document, config: *Config) !lsp.Notification {
    const tree = doc.tree;

    var diagnostics = std.ArrayList(lsp.Diagnostic).init(arena.allocator());

    for (tree.errors) |err| {
        const loc = tree.tokenLocation(0, err.token);

        var mem_buffer: [256]u8 = undefined;
        var fbs = std.io.fixedBufferStream(&mem_buffer);
        try tree.renderError(err, fbs.writer());

        try diagnostics.append(.{
            .range = astLocationToRange(loc),
            .severity = .Error,
            .code = @tagName(err.tag),
            .source = "zls",
            .message = try arena.allocator().dupe(u8, fbs.getWritten()),
            // .relatedInformation = undefined
        });
    }

    // TODO: style warnings for types, values and declarations below root scope
    if (tree.errors.len == 0) {
        for (tree.rootDecls()) |decl_idx| {
            const decl = tree.nodes.items(.tag)[decl_idx];
            switch (decl) {
                .fn_proto,
                .fn_proto_multi,
                .fn_proto_one,
                .fn_proto_simple,
                .fn_decl,
                => blk: {
                    var buf: [1]Ast.Node.Index = undefined;
                    const func = ast.fnProto(tree, decl_idx, &buf).?;
                    if (func.extern_export_inline_token != null) break :blk;

                    if (config.warn_style) {
                        if (func.name_token) |name_token| {
                            const loc = tree.tokenLocation(0, name_token);

                            const is_type_function = TypeWithHandle.isTypeFunction(tree, func);

                            const func_name = tree.tokenSlice(name_token);
                            if (!is_type_function and !ast.isCamelCase(func_name)) {
                                try diagnostics.append(.{
                                    .range = astLocationToRange(loc),
                                    .severity = .Information,
                                    .code = "BadStyle",
                                    .source = "zls",
                                    .message = "Functions should be camelCase",
                                });
                            } else if (is_type_function and !ast.isPascalCase(func_name)) {
                                try diagnostics.append(.{
                                    .range = astLocationToRange(loc),
                                    .severity = .Information,
                                    .code = "BadStyle",
                                    .source = "zls",
                                    .message = "Type functions should be PascalCase",
                                });
                            }
                        }
                    }
                },
                else => {},
            }
        }
    }

    // logger.debug("[Diagnostics] {s}: {}", .{ doc.utf8_buffer.uri, diagnostics.items.len });
    return lsp.Notification{
        .method = "textDocument/publishDiagnostics",
        .params = .{
            .PublishDiagnostics = .{
                .uri = doc.uri,
                .diagnostics = diagnostics.items,
            },
        },
    };
}

fn getRange(doc: *Document, token: AstToken, encoding: Line.Encoding) !lsp.Range {
    const loc = token.getRange();
    var start_loc = try doc.utf8_buffer.getPositionFromBytePosition(loc.start, encoding);
    var end_loc = try doc.utf8_buffer.getPositionFromBytePosition(loc.end, encoding);
    var range = lsp.Range{
        .start = .{
            .line = @intCast(i64, start_loc.line),
            .character = @intCast(i64, start_loc.x),
        },
        .end = .{
            .line = @intCast(i64, end_loc.line),
            .character = @intCast(i64, end_loc.x),
        },
    };
    return range;
}

const SymbolTree = struct {
    const Self = @This();

    root: std.ArrayList(lsp.DocumentSymbol),
    imports: std.ArrayList(lsp.DocumentSymbol),

    fn init(allocator: std.mem.Allocator) Self {
        var self = Self{
            .root = std.ArrayList(lsp.DocumentSymbol).init(allocator),
            .imports = std.ArrayList(lsp.DocumentSymbol).init(allocator),
        };
        var range = lsp.Range{
            .start = .{
                .line = 0,
                .character = 0,
            },
            .end = .{
                .line = 0,
                .character = 0,
            },
        };
        self.root.append(.{
            .name = "imports",
            .kind = .Module,
            .range = range,
            .selectionRange = range,
            .detail = "",
        }) catch unreachable;
        return self;
    }

    fn toOwnedSlice(self: *Self) []lsp.DocumentSymbol {
        self.root.items[0].children = self.imports.items;
        return self.root.toOwnedSlice();
    }

    fn process(self: *Self, arena: *std.heap.ArenaAllocator, doc: *Document, encoding: Line.Encoding) !void {
        for (doc.ast_context.tree.rootDecls()) |decl| {
            if (try self.traverse(arena, doc, AstNode.init(doc.ast_context, decl), encoding)) |child| {
                try self.root.append(child);
            }
        }
    }

    fn traverse(
        self: *Self,
        arena: *std.heap.ArenaAllocator,
        doc: *Document,
        node: AstNode,
        encoding: Line.Encoding,
    ) !?lsp.DocumentSymbol {
        _ = self;
        _ = arena;
        var buf: [2]u32 = undefined;
        switch (node.getChildren(&buf)) {
            .var_decl => |var_decl| {
                const type_var = VarType.fromVarDecl(node.context, var_decl);
                const text = try type_var.allocPrint(arena.allocator());
                const token = node.getMainToken().next();
                const range = try getRange(doc, token, encoding);
                if (type_var.kind == .import) {
                    try self.imports.append(lsp.DocumentSymbol{
                        .name = token.getText(),
                        .kind = .Module,
                        .range = range,
                        .selectionRange = range,
                        .detail = text,
                    });
                } else {
                    // TODO: recursive if container
                    return lsp.DocumentSymbol{
                        .name = token.getText(),
                        .kind = .Variable,
                        .range = range,
                        .selectionRange = range,
                        .detail = text,
                    };
                }
            },
            .container_field => |container_field| {
                const type_var = VarType.fromContainerField(node.context, container_field);
                const text = try type_var.allocPrint(arena.allocator());
                const token = AstToken.init(&node.context.tree, container_field.ast.name_token);
                const range = try getRange(doc, token, encoding);
                    return lsp.DocumentSymbol{
                        .name = token.getText(),
                        .kind = .Property,
                        .range = range,
                        .selectionRange = range,
                        .detail = text,
                    };
            },
            else => {
                switch (node.getTag()) {
                    .fn_decl => {
                        const fn_proto_node = AstNode.init(node.context, node.getData().lhs);
                        var buf2: [2]u32 = undefined;
                        if (fn_proto_node.getFnProto(&buf2)) |fn_proto| {
                            if (fn_proto.name_token) |name_token| {
                                const token = AstToken.init(&node.context.tree, name_token);
                                const range = try getRange(doc, token, encoding);
                                return lsp.DocumentSymbol{
                                    .name = token.getText(),
                                    .kind = .Function,
                                    .range = range,
                                    .selectionRange = range,
                                    .detail = "",
                                    // .children = try to_symbols(allocator, doc, encoding, src, symbol.node),
                                };
                            }
                        }
                    },
                    else => {},
                }
            },
        }
        return null;
    }
};

pub fn to_symbols(arena: *std.heap.ArenaAllocator, doc: *Document, encoding: Line.Encoding) anyerror![]lsp.DocumentSymbol {
    var symbol_tree = SymbolTree.init(arena.allocator());
    try symbol_tree.process(arena, doc, encoding);
    return symbol_tree.toOwnedSlice();
}
