/// https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#initialize
const std = @import("std");
const types = @import("./types.zig");
const Default = types.Default;
const Exists = types.Exists;
const string = types.string;
const MaybeStringArray = types.MaybeStringArray;

pub const ClientCapabilities = struct {
    workspace: ?struct {
        workspaceFolders: Default(bool, false),
    },
    textDocument: ?struct {
        semanticTokens: Exists,
        hover: ?struct {
            contentFormat: MaybeStringArray,
        },
        completion: ?struct {
            completionItem: ?struct {
                snippetSupport: Default(bool, false),
                documentationFormat: MaybeStringArray,
            },
        },
    },
    offsetEncoding: MaybeStringArray,
};

pub const WorkspaceFolder = struct {
    uri: string,
    name: string,
};

pub const InitializeParams = struct {
    processId: ?i64,
    clientInfo: ?struct {
        name: string,
        version: ?string,
    },
    locale: ?string,
    rootPath: ?string,
    rootUri: ?string,
    capabilities: ClientCapabilities,
    trace: ?string,
    workspaceFolders: ?[]const WorkspaceFolder,
};

pub const SemanticTokensProvider = struct {
    full: bool,
    range: bool,
    legend: struct {
        tokenTypes: []const string,
        tokenModifiers: []const string,
    },
};

// Only includes options we set in our initialize result.
pub const ServerCapabilities = struct {
    signatureHelpProvider: struct {
        triggerCharacters: []const string,
        retriggerCharacters: []const string,
    },
    textDocumentSync: enum(u32) {
        None = 0,
        Full = 1,
        Incremental = 2,

        pub fn jsonStringify(value: @This(), options: std.json.StringifyOptions, out_stream: anytype) !void {
            try std.json.stringify(@enumToInt(value), options, out_stream);
        }
    },
    renameProvider: bool,
    completionProvider: struct {
        resolveProvider: bool,
        triggerCharacters: []const string,
    },
    documentHighlightProvider: bool,
    hoverProvider: bool,
    codeActionProvider: bool,
    codeLensProvider: ?struct {
        resolveProvider: ?bool,
    },
    declarationProvider: bool,
    definitionProvider: bool,
    typeDefinitionProvider: bool,
    implementationProvider: bool,
    referencesProvider: bool,
    documentSymbolProvider: bool,
    colorProvider: bool,
    documentFormattingProvider: bool,
    documentRangeFormattingProvider: bool,
    foldingRangeProvider: bool,
    selectionRangeProvider: bool,
    workspaceSymbolProvider: bool,
    rangeProvider: bool,
    documentProvider: bool,
    workspace: ?struct {
        workspaceFolders: ?struct {
            supported: bool,
            changeNotifications: bool,
        },
    },
    semanticTokensProvider: SemanticTokensProvider,
};

pub const InitializeResult = struct {
    offsetEncoding: string,
    capabilities: ServerCapabilities,
    serverInfo: struct {
        name: string,
        version: ?string = null,
    },
};
