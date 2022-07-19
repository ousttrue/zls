const std = @import("std");
const Config = @import("./Config.zig");
const Workspace = @import("./Workspace.zig");
const Document = @import("./Document.zig");
const requests = @import("lsp").requests;
const lsp = @import("lsp");
const analysis = @import("./analysis.zig");
const ast = @import("./ast.zig");
const references = @import("./references.zig");
const rename = @import("./rename.zig");
const offsets = @import("./offsets.zig");
const semantic_tokens = @import("./semantic_tokens.zig");
const shared = @import("./shared.zig");
const Ast = std.zig.Ast;
const getSignatureInfo = @import("signature_help.zig").getSignatureInfo;
const builtin_completions = @import("./builtin_completions.zig");
const position_context = @import("./position_context.zig");
const DocumentPosition = @import("./document_position.zig").DocumentPosition;

const Session = struct {};

const logger = std.log.scoped(.main);

const no_signatures_response = lsp.ResponseParams{
    .SignatureHelp = .{
        .signatures = &.{},
        .activeSignature = null,
        .activeParameter = null,
    },
};

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

fn createNotifyDiagnostics(session: *Session, handle: *const Document) !lsp.Notification {
    const tree = handle.tree;

    var diagnostics = std.ArrayList(lsp.Diagnostic).init(session.arena.allocator());

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
            .message = try session.arena.allocator().dupe(u8, fbs.getWritten()),
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

                    if (session.config.warn_style) {
                        if (func.name_token) |name_token| {
                            const loc = tree.tokenLocation(0, name_token);

                            const is_type_function = analysis.isTypeFunction(tree, func);

                            const func_name = tree.tokenSlice(name_token);
                            if (!is_type_function and !analysis.isCamelCase(func_name)) {
                                try diagnostics.append(.{
                                    .range = astLocationToRange(loc),
                                    .severity = .Information,
                                    .code = "BadStyle",
                                    .source = "zls",
                                    .message = "Functions should be camelCase",
                                });
                            } else if (is_type_function and !analysis.isPascalCase(func_name)) {
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

    return lsp.Notification{
        .method = "textDocument/publishDiagnostics",
        .params = .{
            .PublishDiagnostics = .{
                .uri = handle.document.uri,
                .diagnostics = diagnostics.items,
            },
        },
    };

    // try notifyQueue.insert(0, notification);
}

fn referencesDefinitionGlobal(session: *Session, id: i64, handle: *Document, pos_index: usize, include_decl: bool, skip_std_references: bool) !lsp.Response {
    const decl = try offsets.getSymbolGlobal(session, pos_index, handle);
    var locs = std.ArrayList(lsp.Location).init(session.arena.allocator());
    try references.symbolReferences(
        session,
        decl,
        offsets.offset_encoding,
        include_decl,
        &locs,
        std.ArrayList(lsp.Location).append,
        skip_std_references,
    );
    return lsp.Response{
        .id = id,
        .result = .{ .Locations = locs.items },
    };
}

fn referencesDefinitionFieldAccess(session: *Session, id: i64, handle: *Document, position: DocumentPosition, range: analysis.SourceRange, include_decl: bool) !lsp.Response {
    const decl = try offsets.getSymbolFieldAccess(session, handle, position, range);
    var locs = std.ArrayList(lsp.Location).init(session.arena.allocator());
    try references.symbolReferences(session, decl, offsets.offset_encoding, include_decl, &locs, std.ArrayList(lsp.Location).append, session.config.skip_std_references);
    return lsp.Response{
        .id = id,
        .result = .{ .Locations = locs.items },
    };
}

fn referencesDefinitionLabel(session: *Session, id: i64, handle: *Document, pos_index: usize, include_decl: bool) !lsp.Response {
    const decl = (try offsets.getLabelGlobal(pos_index, handle)) orelse return lsp.Response.createNull(id);
    var locs = std.ArrayList(lsp.Location).init(session.arena.allocator());
    try references.labelReferences(decl, offsets.offset_encoding, include_decl, &locs, std.ArrayList(lsp.Location).append);
    return lsp.Response{
        .id = id,
        .result = .{ .Locations = locs.items },
    };
}

fn hasComment(tree: Ast.Tree, start_token: Ast.TokenIndex, end_token: Ast.TokenIndex) bool {
    const token_starts = tree.tokens.items(.start);

    const start = token_starts[start_token];
    const end = token_starts[end_token];

    return std.mem.indexOf(u8, tree.source[start..end], "//") != null;
}

fn getSignature(session: *Session, id: i64, req: requests.SignatureHelp) !lsp.Response {
    if (req.params.position.character == 0) {
        return lsp.Response{ .id = id, .result = no_signatures_response };
    }

    const handle = try session.workspace.getHandle(req.params.textDocument.uri);
    const doc_position = try offsets.documentPosition(handle.document, req.params.position, offsets.offset_encoding);
    if (try getSignatureInfo(
        session,
        handle,
        doc_position.absolute_index,
        builtin_completions.data(),
    )) |sig_info| {
        return lsp.Response{
            .id = id,
            .result = .{
                .SignatureHelp = .{
                    .signatures = &[1]lsp.SignatureInformation{sig_info},
                    .activeSignature = 0,
                    .activeParameter = sig_info.activeParameter,
                },
            },
        };
    }

    return lsp.Response{ .id = id, .result = no_signatures_response };
}

pub fn signatureHelpHandler(session: *Session, id: i64, req: requests.SignatureHelp) !lsp.Response {
    return try getSignature(session, id, req);
}

pub fn gotoDeclarationHandler(session: *Session, id: i64, req: requests.GotoDefinition) !lsp.Response {
    return try offsets.gotoHandler(session, id, req, false);
}

fn getReference(session: *Session, id: i64, req: requests.References) !lsp.Response {
    const handle = try session.workspace.getHandle(req.params.textDocument.uri);
    const doc_position = try offsets.documentPosition(handle.document, req.params.position, offsets.offset_encoding);
    const pos_context = position_context.documentPositionContext(session.arena, doc_position);

    const include_decl = req.params.context.includeDeclaration;
    return switch (pos_context) {
        .var_access => try referencesDefinitionGlobal(session, id, handle, doc_position.absolute_index, include_decl, session.config.skip_std_references),
        .field_access => |range| try referencesDefinitionFieldAccess(session, id, handle, doc_position, range, include_decl),
        .label => try referencesDefinitionLabel(session, id, handle, doc_position.absolute_index, include_decl),
        else => lsp.Response.createNull(id),
    };
}

pub fn referencesHandler(session: *Session, id: i64, req: requests.References) !lsp.Response {
    return try getReference(session, id, req);
}
