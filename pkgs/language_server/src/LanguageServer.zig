const std = @import("std");
const lsp = @import("lsp");
const Ast = std.zig.Ast;
const ws = @import("workspace");
const Config = ws.Config;
const ZigEnv = ws.ZigEnv;
const Workspace = ws.Workspace;
const Document = ws.Document;
const DocumentPosition = ws.DocumentPosition;
const LinePosition = ws.LinePosition;
const ast = ws.ast;
const semantic_tokens = ws.semantic_tokens;
const SemanticTokensBuilder = ws.SemanticTokensBuilder;
const SymbolTree = ws.SymbolTree;
const hover_util = ws.hover_util;
const completion_util = ws.completion_util;
const ClientCapabilities = ws.ClientCapabilities;
const builtin_completions = ws.builtin_completions;
const TextPosition = @import("./TextPosition.zig");
const rename_util = @import("./rename_util.zig");
const references_util = @import("./references_util.zig");
const Self = @This();
const getSignatureInfo = ws.signature_help.getSignatureInfo;

const logger = std.log.scoped(.LanguageServer);
pub var keep_running: bool = true;

fn toLineX(src: lsp.Position) LinePosition.LineX {
    return .{ .line = @intCast(u32, src.line), .x = @intCast(u32, src.character) };
}

fn fromLineX(src: LinePosition.LineX) lsp.Position {
    return .{ .line = src.line, .character = src.x };
}

fn utf8PositionToUtf16(line_position: LinePosition, src: lsp.Position) !lsp.Position {
    return fromLineX(try line_position.utf8PositionToUtf16(toLineX(src)));
}

fn symbolToUtf16(line_position: LinePosition, symbol: *lsp.DocumentSymbol) anyerror!void {
    symbol.range.start = try utf8PositionToUtf16(line_position, symbol.range.start);
    symbol.range.end = try utf8PositionToUtf16(line_position, symbol.range.end);
    symbol.selectionRange.start = try utf8PositionToUtf16(line_position, symbol.selectionRange.start);
    symbol.selectionRange.end = try utf8PositionToUtf16(line_position, symbol.selectionRange.end);
    for (symbol.children) |*child| {
        try symbolToUtf16(line_position, child);
    }
}

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

fn createNotifyDiagnostics(arena: *std.heap.ArenaAllocator, handle: *const Document, config: *Config) !lsp.Notification {
    const tree = handle.tree;

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

                            const is_type_function = ast.isTypeFunction(tree, func);

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

    return lsp.Notification{
        .method = "textDocument/publishDiagnostics",
        .params = .{
            .PublishDiagnostics = .{
                .uri = handle.document.uri,
                .diagnostics = diagnostics.items,
            },
        },
    };
}

pub fn documentRange(text: []const u8, encoding: TextPosition.Encoding) !lsp.Range {
    var line_idx: i64 = 0;
    var curr_line: []const u8 = text;

    var split_iterator = std.mem.split(u8, text, "\n");
    while (split_iterator.next()) |line| : (line_idx += 1) {
        curr_line = line;
    }

    if (encoding == .utf8) {
        return lsp.Range{
            .start = .{
                .line = 0,
                .character = 0,
            },
            .end = .{
                .line = line_idx,
                .character = @intCast(i64, curr_line.len),
            },
        };
    } else {
        var utf16_len: usize = 0;
        var line_utf8_idx: usize = 0;
        while (line_utf8_idx < curr_line.len) {
            const n = try std.unicode.utf8ByteSequenceLength(curr_line[line_utf8_idx]);
            const codepoint = try std.unicode.utf8Decode(curr_line[line_utf8_idx .. line_utf8_idx + n]);
            if (codepoint < 0x10000) {
                utf16_len += 1;
            } else {
                utf16_len += 2;
            }
            line_utf8_idx += n;
        }
        return lsp.Range{
            .start = .{
                .line = 0,
                .character = 0,
            },
            .end = .{
                .line = line_idx,
                .character = @intCast(i64, utf16_len),
            },
        };
    }
}

fn node_tag_to_lsp_symbol_kind(node_tag: std.zig.Ast.Node.Tag) lsp.DocumentSymbol.Kind {
    return switch (node_tag) {
        .fn_proto,
        .fn_proto_simple,
        .fn_proto_multi,
        .fn_proto_one,
        .fn_decl,
        => .Function,
        .local_var_decl,
        .global_var_decl,
        .aligned_var_decl,
        .simple_var_decl,
        => .Variable,
        .container_field,
        .container_field_align,
        .container_field_init,
        .tagged_union_enum_tag,
        .tagged_union_enum_tag_trailing,
        .tagged_union,
        .tagged_union_trailing,
        .tagged_union_two,
        .tagged_union_two_trailing,
        => .Field,
        else => .Variable,
    };
}

fn to_symbols(allocator: std.mem.Allocator, doc: *Document, encoding: TextPosition.Encoding, src: []const SymbolTree.Symbol, parent: u32) anyerror![]lsp.DocumentSymbol {
    var dst = std.ArrayList(lsp.DocumentSymbol).init(allocator);
    const tree = doc.tree;
    const ast_context = doc.ast_context;
    const tags = tree.nodes.items(.tag);
    for (src) |symbol| {
        if (symbol.parent == parent) {
            const first =ast_context.tokens.items[tree.firstToken(symbol.node)];
            var start_loc = try doc.line_position.getPositionFromBytePosition(first.loc.start);
            if (encoding == .utf16) {
                start_loc = try doc.line_position.utf8PositionToUtf16(start_loc);
            }
            const last = ast_context.tokens.items[tree.lastToken(symbol.node)];
            var end_loc = try doc.line_position.getPositionFromBytePosition(last.loc.end);
            if (encoding == .utf16) {
                end_loc = try doc.line_position.utf8PositionToUtf16(end_loc);
            }

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

            try dst.append(lsp.DocumentSymbol{
                .name = symbol.name,
                .kind = node_tag_to_lsp_symbol_kind(tags[symbol.node]),
                .range = range,
                .selectionRange = range,
                .detail = "",
                .children = try to_symbols(allocator, doc, encoding, src, symbol.node),
            });
        }
    }
    return dst.items;
}

config: *Config,
zigenv: ZigEnv,
workspace: Workspace = undefined,
client_capabilities: ClientCapabilities = .{},
offset_encoding: TextPosition.Encoding = TextPosition.Encoding.utf16,
server_capabilities: lsp.initialize.ServerCapabilities = .{
    .signatureHelpProvider = .{
        .triggerCharacters = &.{"("},
        .retriggerCharacters = &.{","},
    },
    .textDocumentSync = .Full,
    .renameProvider = true,
    .completionProvider = .{
        .resolveProvider = false,
        .triggerCharacters = &[_][]const u8{ ".", ":", "@" },
    },
    .documentHighlightProvider = false,
    .hoverProvider = true,
    .codeActionProvider = false,
    .declarationProvider = false,
    .definitionProvider = true,
    .typeDefinitionProvider = false,
    .implementationProvider = false,
    .referencesProvider = true,
    .documentSymbolProvider = true,
    .colorProvider = false,
    .documentFormattingProvider = true,
    .documentRangeFormattingProvider = false,
    .foldingRangeProvider = false,
    .selectionRangeProvider = false,
    .workspaceSymbolProvider = false,
    .rangeProvider = false,
    .documentProvider = true,
    .workspace = .{
        .workspaceFolders = .{
            .supported = false,
            .changeNotifications = false,
        },
    },
    .semanticTokensProvider = .{
        .full = true,
        .range = false,
        .legend = .{
            .tokenTypes = block: {
                const tokTypeFields = std.meta.fields(semantic_tokens.SemanticTokenType);
                var names: [tokTypeFields.len][]const u8 = undefined;
                for (tokTypeFields) |field, i| {
                    names[i] = field.name;
                }
                break :block &names;
            },
            .tokenModifiers = block: {
                const tokModFields = std.meta.fields(semantic_tokens.SemanticTokenModifiers);
                var names: [tokModFields.len][]const u8 = undefined;
                for (tokModFields) |field, i| {
                    names[i] = field.name;
                }
                break :block &names;
            },
        },
    },
},

pub fn init(allocator: std.mem.Allocator, config: *Config, zigenv: ZigEnv) Self {
    return Self{
        .config = config,
        .zigenv = zigenv,
        .workspace = Workspace.init(allocator, zigenv),
    };
}

pub fn deinit(self: *Self) void {
    self.workspace.deinit();
}

pub fn initialize(self: *Self, arena: *std.heap.ArenaAllocator, id: i64, jsonParams: ?std.json.Value) !lsp.Response {
    const params = try lsp.fromDynamicTree(arena, lsp.initialize.InitializeParams, jsonParams.?);
    for (params.capabilities.offsetEncoding.value) |encoding| {
        if (std.mem.eql(u8, encoding, "utf-8")) {
            self.offset_encoding = .utf8;
        }
    }

    if (params.capabilities.textDocument) |textDocument| {
        self.client_capabilities.supports_semantic_tokens = textDocument.semanticTokens.exists;
        if (textDocument.hover) |hover| {
            for (hover.contentFormat.value) |format| {
                if (std.mem.eql(u8, "markdown", format)) {
                    self.client_capabilities.hover_supports_md = true;
                }
            }
        }
        if (textDocument.completion) |completion| {
            if (completion.completionItem) |completionItem| {
                self.client_capabilities.supports_snippets = completionItem.snippetSupport.value;
                for (completionItem.documentationFormat.value) |documentationFormat| {
                    if (std.mem.eql(u8, "markdown", documentationFormat)) {
                        self.client_capabilities.completion_doc_supports_md = true;
                    }
                }
            }
        }
    }

    logger.info("zls initialized", .{});
    logger.info("{}", .{self.client_capabilities});
    logger.info("Using offset encoding: {s}", .{@tagName(self.offset_encoding)});

    return lsp.Response{
        .id = id,
        .result = .{ .InitializeResult = .{
            .offsetEncoding = self.offset_encoding.toString(),
            .serverInfo = .{
                .name = "zls",
                .version = "0.1.0",
            },
            .capabilities = self.server_capabilities,
        } },
    };
}

pub fn shutdown(self: *Self, arena: *std.heap.ArenaAllocator, id: i64, jsonParams: ?std.json.Value) !lsp.Response {
    _ = self;
    _ = arena;
    _ = jsonParams;
    keep_running = false;
    return lsp.Response.createNull(id);
}

pub fn @"textDocument/didOpen"(self: *Self, arena: *std.heap.ArenaAllocator, jsonParams: ?std.json.Value) !void {
    const params = try lsp.fromDynamicTree(arena, lsp.requests.OpenDocument, jsonParams.?);
    _ = try self.workspace.openDocument(params.textDocument.uri, params.textDocument.text);
    // if (createNotifyDiagnostics(self, doc)) |notification| {
    //     self.transport.sendToJson(notification);
    // } else |_| {}
}

pub fn @"textDocument/didChange"(self: *Self, arena: *std.heap.ArenaAllocator, jsonParams: ?std.json.Value) !void {
    const params = try lsp.fromDynamicTree(arena, lsp.requests.ChangeDocument, jsonParams.?);
    const doc = try self.workspace.getDocument(params.textDocument.uri);
    try doc.applyChanges(params.contentChanges.Array, self.offset_encoding == .utf16, self.zigenv);
    // if (createNotifyDiagnostics(self, doc)) |notification| {
    //     self.transport.sendToJson(notification);
    // } else |_| {}
}

pub fn @"textDocument/didSave"(self: *Self, arena: *std.heap.ArenaAllocator, jsonParams: ?std.json.Value) !void {
    const params = try lsp.fromDynamicTree(arena, lsp.requests.SaveDocument, jsonParams.?);
    const doc = try self.workspace.getDocument(params.textDocument.uri);
    try doc.applySave(self.zigenv);
}

pub fn @"textDocument/didClose"(self: *Self, arena: *std.heap.ArenaAllocator, jsonParams: ?std.json.Value) !void {
    const params = try lsp.fromDynamicTree(arena, lsp.requests.CloseDocument, jsonParams.?);
    const doc = try self.workspace.getDocument(params.textDocument.uri);
    _ = doc.decrement();
}

pub fn @"textDocument/documentSymbol"(self: *Self, arena: *std.heap.ArenaAllocator, id: i64, jsonParams: ?std.json.Value) !lsp.Response {
    const params = try lsp.fromDynamicTree(arena, lsp.requests.DocumentSymbols, jsonParams.?);
    const doc = try self.workspace.getDocument(params.textDocument.uri);
    var symbol_tree = SymbolTree.init(arena.allocator(), doc.tree);
    try symbol_tree.traverse(0);
    logger.debug("{} symbols", .{symbol_tree.symbols.items.len});
    const symbols = try to_symbols(arena.allocator(), doc, self.offset_encoding, symbol_tree.symbols.items, 0);
    return lsp.Response{
        .id = id,
        .result = .{
            .DocumentSymbols = symbols,
        },
    };
}

pub fn @"textDocument/hover"(self: *Self, arena: *std.heap.ArenaAllocator, id: i64, jsonParams: ?std.json.Value) !lsp.Response {
    const params = try lsp.fromDynamicTree(arena, lsp.requests.Hover, jsonParams.?);
    const doc = try self.workspace.getDocument(params.textDocument.uri);
    const position = params.position;
    const bytePosition = switch (self.offset_encoding) {
        .utf8 => try TextPosition.utf8BytePositionFromUtf8Pos(doc.utf8_buffer.text, .{ .line = @intCast(u32, position.line), .x = @intCast(u32, position.character) }),
        .utf16 => try TextPosition.utf8BytePositionFromUtf16Pos(doc.utf8_buffer.text, .{ .line = @intCast(u32, position.line), .x = @intCast(u32, position.character) }),
    };
    if (hover_util.process(arena, &self.workspace, doc, bytePosition, &self.client_capabilities)) |hover_or_null| {
        if (hover_or_null) |hover_contents| {
            return lsp.Response{
                .id = id,
                .result = .{
                    .Hover = .{
                        .contents = .{ .value = hover_contents },
                    },
                },
            };
        } else {
            return lsp.Response.createNull(id);
        }
    } else |err| {
        const hover_contents = try std.fmt.allocPrint(arena.allocator(), "{}", .{err});
        return lsp.Response{
            .id = id,
            .result = .{
                .Hover = .{
                    .contents = .{ .value = hover_contents },
                },
            },
        };
    }
}

pub fn @"textDocument/semanticTokens/full"(self: *Self, arena: *std.heap.ArenaAllocator, id: i64, jsonParams: ?std.json.Value) !lsp.Response {
    if (!self.config.enable_semantic_tokens) {
        return lsp.Response{
            .id = id,
            .result = lsp.ResponseParams{ .SemanticTokensFull = .{
                .data = &.{},
            } },
        };
    }
    const params = try lsp.fromDynamicTree(arena, lsp.requests.SemanticTokensFull, jsonParams.?);
    const doc = try self.workspace.getDocument(params.textDocument.uri);
    var token_array = try SemanticTokensBuilder.writeAllSemanticTokens(arena, doc);
    if (self.offset_encoding == .utf16) {
        var i: u32 = 0;
        while (i < token_array.len) : (i += 5) {
            // { line: 2, startChar:  5, length: 3, tokenType: 0, tokenModifiers: 3 },
            const line = token_array[i];
            const start_char = token_array[i + 1];
            const end_char = start_char + token_array[i + 2];
            const start = try doc.line_position.utf8PositionToUtf16(.{ .line = line, .x = start_char });
            const end = try doc.line_position.utf8PositionToUtf16(.{ .line = line, .x = end_char });

            token_array[i + 1] = @intCast(u32, start.x);
            token_array[i + 2] = @intCast(u32, end.x - start.x);
        }
    }
    // convert to delta
    {
        var prev_line: u32 = 0;
        var prev_character: u32 = 0;
        var i: u32 = 0;
        while (i < token_array.len) : (i += 5) {
            const current_line = token_array[i];
            const current_character = token_array[i + 1];

            token_array[i] = current_line - prev_line;
            token_array[i + 1] = current_character - if (current_line == prev_line) prev_character else 0;

            prev_line = current_line;
            prev_character = current_character;
        }
    }
    return lsp.Response{
        .id = id,
        .result = .{ .SemanticTokensFull = .{ .data = token_array } },
    };
}

pub fn @"textDocument/formatting"(self: *Self, arena: *std.heap.ArenaAllocator, id: i64, jsonParams: ?std.json.Value) !lsp.Response {
    const params = try lsp.fromDynamicTree(arena, lsp.requests.Formatting, jsonParams.?);
    const doc = try self.workspace.getDocument(params.textDocument.uri);

    const stdout_bytes = self.zigenv.spawnZigFmt(arena.allocator(), doc.utf8_buffer.text) catch |err|
        {
        logger.err("zig fmt: {}", .{err});
        return lsp.Response.createNull(id);
    };

    var edits = try arena.allocator().alloc(lsp.TextEdit, 1);
    edits[0] = .{
        .range = try documentRange(doc.utf8_buffer.text, self.offset_encoding),
        .newText = stdout_bytes,
    };
    return lsp.Response{
        .id = id,
        .result = .{
            .TextEdits = edits,
        },
    };
}

pub fn @"textDocument/definition"(self: *Self, arena: *std.heap.ArenaAllocator, id: i64, jsonParams: ?std.json.Value) !lsp.Response {
    const params = try lsp.fromDynamicTree(arena, lsp.requests.GotoDefinition, jsonParams.?);
    logger.debug("[definition]{s} {}", .{ params.textDocument.uri, params.position });
    const doc = try self.workspace.getDocument(params.textDocument.uri);
    const position = params.position;
    const doc_position = switch (self.offset_encoding) {
        .utf8 => try DocumentPosition.fromUtf8Pos(doc.utf8_buffer.text, .{
            .line = @intCast(u32, position.line),
            .x = @intCast(u32, position.character),
        }),
        .utf16 => try DocumentPosition.fromUtf16Pos(doc.utf8_buffer.text, .{
            .line = @intCast(u32, position.line),
            .x = @intCast(u32, position.character),
        }),
    };
    if (try self.workspace.gotoHandler(arena, doc, doc_position, true)) |location| {
        var goto = lsp.Position{ .line = location.row, .character = location.col };
        if (self.offset_encoding == .utf16) {
            goto = try utf8PositionToUtf16(doc.line_position, goto);
        }
        return lsp.Response{
            .id = id,
            .result = .{
                .Location = .{
                    .uri = location.uri,
                    .range = .{
                        .start = goto,
                        .end = goto,
                    },
                },
            },
        };
    } else {
        return lsp.Response.createNull(id);
    }
}

pub fn @"$/cancelRequest"(self: *Self, arena: *std.heap.ArenaAllocator, jsonParams: ?std.json.Value) !void {
    _ = self;
    _ = arena;
    _ = jsonParams;
}

pub fn @"textDocument/completion"(self: *Self, arena: *std.heap.ArenaAllocator, id: i64, jsonParams: ?std.json.Value) !lsp.Response {
    const params = try lsp.fromDynamicTree(arena, lsp.requests.Completion, jsonParams.?);
    const doc = try self.workspace.getDocument(params.textDocument.uri);
    const position = params.position;
    const doc_position = switch (self.offset_encoding) {
        .utf8 => try DocumentPosition.fromUtf8Pos(doc.utf8_buffer.text, .{
            .line = @intCast(u32, position.line),
            .x = @intCast(u32, position.character),
        }),
        .utf16 => try DocumentPosition.fromUtf16Pos(doc.utf8_buffer.text, .{
            .line = @intCast(u32, position.line),
            .x = @intCast(u32, position.character),
        }),
    };
    return completion_util.process(arena, &self.workspace, id, doc, doc_position, self.config, &self.client_capabilities);
}

pub fn @"textDocument/rename"(self: *Self, arena: *std.heap.ArenaAllocator, id: i64, jsonParams: ?std.json.Value) !lsp.Response {
    const params = try lsp.fromDynamicTree(arena, lsp.requests.Rename, jsonParams.?);
    const doc = try self.workspace.getDocument(params.textDocument.uri);
    const position = params.position;
    const doc_position = switch (self.offset_encoding) {
        .utf8 => try DocumentPosition.fromUtf8Pos(doc.utf8_buffer.text, .{
            .line = @intCast(u32, position.line),
            .x = @intCast(u32, position.character),
        }),
        .utf16 => try DocumentPosition.fromUtf16Pos(doc.utf8_buffer.text, .{
            .line = @intCast(u32, position.line),
            .x = @intCast(u32, position.character),
        }),
    };
    if (try rename_util.process(arena, &self.workspace, doc, doc_position, params.newName)) |*workspace_edit| {
        if (self.offset_encoding == .utf16) {
            var it = workspace_edit.changes.?.valueIterator();
            while (it.next()) |edits| {
                for (edits.*) |*edit| {
                    edit.range.start = try utf8PositionToUtf16(doc.line_position, edit.range.start);
                    edit.range.end = try utf8PositionToUtf16(doc.line_position, edit.range.end);
                }
            }
        }
        return lsp.Response{
            .id = id,
            .result = .{ .WorkspaceEdit = workspace_edit.* },
        };
    } else {
        return lsp.Response.createNull(id);
    }
}

pub fn @"textDocument/references"(self: *Self, arena: *std.heap.ArenaAllocator, id: i64, jsonParams: ?std.json.Value) !lsp.Response {
    const params = try lsp.fromDynamicTree(arena, lsp.requests.References, jsonParams.?);
    const doc = try self.workspace.getDocument(params.textDocument.uri);
    const position = params.position;
    const doc_position = switch (self.offset_encoding) {
        .utf8 => try DocumentPosition.fromUtf8Pos(doc.utf8_buffer.text, .{
            .line = @intCast(u32, position.line),
            .x = @intCast(u32, position.character),
        }),
        .utf16 => try DocumentPosition.fromUtf16Pos(doc.utf8_buffer.text, .{
            .line = @intCast(u32, position.line),
            .x = @intCast(u32, position.character),
        }),
    };
    if (try references_util.process(arena, &self.workspace, doc, doc_position, params.context.includeDeclaration, self.config)) |*locations| {
        if (self.offset_encoding == .utf16) {
            for (locations.*) |*loc| {
                loc.range.start = try utf8PositionToUtf16(doc.line_position, loc.range.start);
                loc.range.end = try utf8PositionToUtf16(doc.line_position, loc.range.end);
            }
        }

        return lsp.Response{
            .id = id,
            .result = .{ .Locations = locations.* },
        };
    } else {
        return lsp.Response.createNull(id);
    }
}

const no_signatures_response = lsp.ResponseParams{
    .SignatureHelp = .{
        .signatures = &.{},
        .activeSignature = null,
        .activeParameter = null,
    },
};

pub fn @"textDocument/signatureHelp"(self: *Self, arena: *std.heap.ArenaAllocator, id: i64, jsonParams: ?std.json.Value) !lsp.Response {
    const params = try lsp.fromDynamicTree(arena, lsp.requests.SignatureHelp, jsonParams.?);
    const doc = try self.workspace.getDocument(params.textDocument.uri);
    const position = params.position;
    const doc_position = switch (self.offset_encoding) {
        .utf8 => try DocumentPosition.fromUtf8Pos(doc.utf8_buffer.text, .{
            .line = @intCast(u32, position.line),
            .x = @intCast(u32, position.character),
        }),
        .utf16 => try DocumentPosition.fromUtf16Pos(doc.utf8_buffer.text, .{
            .line = @intCast(u32, position.line),
            .x = @intCast(u32, position.character),
        }),
    };

    if (try getSignatureInfo(
        arena,
        &self.workspace,
        doc,
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
