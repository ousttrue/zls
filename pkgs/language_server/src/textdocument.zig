const std = @import("std");
const Ast = std.zig.Ast;
const lsp = @import("lsp");
const astutil = @import("astutil");
const Document = astutil.Document;
const Project = astutil.Project;
const Line = astutil.Line;
const AstNode = astutil.AstNode;
const AstNodeIterator = astutil.AstNodeIterator;
const AstToken = astutil.AstToken;
const ImportSolver = astutil.ImportSolver;
const FieldIterator = @import("./FieldIterator.zig");

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
        const range = try getRange(doc, node.getMainToken().getLoc(), encoding);
        var buf: [2]u32 = undefined;
        var item = lsp.DocumentSymbol{
            .name = node.getMemberNameToken().?.getText(),
            .kind = switch (node.getChildren(&buf)) {
                .var_decl => .Variable,
                .container_field => .Property,
                else => .Method,
            },
            .range = range,
        };
        if (FieldIterator.init(project, node)) |*it| {
            var children = std.ArrayList(lsp.DocumentSymbol).init(arena.allocator());
            while (it.next()) |child_node| {
                if (try self.traverse(arena, project, doc, child_node, encoding)) |child| {
                    try children.append(child);
                }
            }
            item.children = children.items;
        }
        return item;
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
