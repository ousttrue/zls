const std = @import("std");
const Ast = std.zig.Ast;
const lsp = @import("lsp");
const astutil = @import("astutil");
const Config = @import("./Config.zig");
const Document = astutil.Document;
const Project = astutil.Project;
const Line = astutil.Line;
const AstNode = astutil.AstNode;
const AstNodeIterator = astutil.AstNodeIterator;
const AstToken = astutil.AstToken;
const VarType = astutil.VarType;
const ImportSolver = astutil.ImportSolver;
const TypeWithHandle = struct {};
const ast = struct {};
const logger = std.log.scoped(.textdocument);

fn getRange(doc: *Document, loc: std.zig.Token.Loc, encoding: Line.Encoding) !lsp.Range {
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

pub fn getDiagnostics(arena: *std.heap.ArenaAllocator, doc: *Document, encoding: Line.Encoding) ![]lsp.diagnostic.Diagnostic {
    const tree = &doc.ast_context.tree;
    var diagnostics = std.ArrayList(lsp.diagnostic.Diagnostic).init(arena.allocator());
    for (tree.errors) |err| {
        var message = std.ArrayList(u8).init(arena.allocator());
        try tree.renderError(err, message.writer());
        try diagnostics.append(.{
            .range = try getRange(doc, AstToken.init(tree, err.token).getLoc(), encoding),
            .severity = .Error,
            .code = @tagName(err.tag),
            .source = "zls",
            .message = message.items,
            // .relatedInformation = undefined
        });
    }
    return diagnostics.toOwnedSlice();
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

    fn process(
        self: *Self,
        arena: *std.heap.ArenaAllocator,
        project: ?Project,
        doc: *Document,
        encoding: Line.Encoding,
    ) !void {
        for (doc.ast_context.tree.rootDecls()) |decl| {
            if (try self.traverse(arena, project, doc, AstNode.init(doc.ast_context, decl), encoding)) |child| {
                try self.root.append(child);
            }
        }
    }

    fn traverse(
        self: *Self,
        arena: *std.heap.ArenaAllocator,
        project: ?Project,
        doc: *Document,
        node: AstNode,
        encoding: Line.Encoding,
    ) anyerror!?lsp.DocumentSymbol {
        _ = self;
        _ = arena;
        var buf: [2]u32 = undefined;
        switch (node.getChildren(&buf)) {
            .var_decl => |var_decl| {
                const type_var = try VarType.fromVarDecl(project, node.context, var_decl);
                const text = try type_var.allocPrint(arena.allocator());
                const token = node.getMainToken().getNext();
                const range = try getRange(doc, token.getLoc(), encoding);
                switch (type_var.kind) {
                    .import => |import| {
                        // .Package or .File
                        try self.imports.append(lsp.DocumentSymbol{
                            .name = token.getText(),
                            .kind = if (std.mem.endsWith(u8, ImportSolver.unquote(import), ".zig")) .File else .Package,
                            .range = range,
                            .selectionRange = range,
                            .detail = text,
                        });
                    },
                    else => {
                        var symbol = lsp.DocumentSymbol{
                            .name = token.getText(),
                            .kind = .Variable,
                            .range = range,
                            .selectionRange = range,
                            .detail = text,
                        };
                        switch (type_var.kind) {
                            .enum_type => {
                                symbol.kind = .Enum;
                            },
                            .container => {
                                symbol.kind = .Struct;
                            },
                            else => {},
                        }

                        // rhs
                        const rhs = AstNode.init(node.context, var_decl.ast.init_node);
                        var buf2: [2]u32 = undefined;
                        switch (rhs.getChildren(&buf2)) {
                            .container_decl => |container_decl| {
                                // traverse children
                                var children = std.ArrayList(lsp.DocumentSymbol).init(arena.allocator());
                                for (container_decl.ast.members) |decl| {
                                    if (try self.traverse(arena, project, doc, AstNode.init(doc.ast_context, decl), encoding)) |child| {
                                        try children.append(child);
                                    }
                                }
                                symbol.children = children.toOwnedSlice();
                            },
                            else => {},
                        }

                        return symbol;
                    },
                }
            },
            .container_field => |container_field| {
                // EnumMember
                const var_type = try VarType.fromContainerField(project, node.context, container_field);
                const text = try var_type.allocPrint(arena.allocator());
                const token = AstToken.init(&node.context.tree, container_field.ast.name_token);
                const range = try getRange(doc, token.getLoc(), encoding);
                return lsp.DocumentSymbol{
                    .name = token.getText(),
                    .kind = if (var_type.kind == .enum_literal) .EnumMember else .Property,
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
                                const type_var = try VarType.fromFnProtoReturn(project, node.context, fn_proto);
                                const text = try type_var.allocPrint(arena.allocator());
                                const token = AstToken.init(&node.context.tree, name_token);
                                const range = try getRange(doc, token.getLoc(), encoding);
                                return lsp.DocumentSymbol{
                                    .name = token.getText(),
                                    .kind = .Function,
                                    .range = range,
                                    .selectionRange = range,
                                    .detail = text,
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

pub fn to_symbols(
    arena: *std.heap.ArenaAllocator,
    project: ?Project,
    doc: *Document,
    encoding: Line.Encoding,
) anyerror![]lsp.DocumentSymbol {
    var symbol_tree = SymbolTree.init(arena.allocator());
    try symbol_tree.process(arena, project, doc, encoding);
    return symbol_tree.toOwnedSlice();
}
