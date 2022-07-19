//! This file contains request types zls handles.
//! Note that the parameter types may be incomplete.
//! We only define what we actually use.

const std = @import("std");
const types = @import("./types.zig");

/// Only check for the field's existence.
const Exists = struct {
    exists: bool,
};

fn Default(comptime T: type, comptime default_value: T) type {
    return struct {
        pub const value_type = T;
        pub const default = default_value;
        value: T,
    };
}

pub fn ErrorUnwrappedReturnOf(comptime func: anytype) type {
    return switch (@typeInfo(@TypeOf(func))) {
        .Fn, .BoundFn => |fn_info| switch (@typeInfo(fn_info.return_type.?)) {
            .ErrorUnion => |err_union| err_union.payload,
            else => |T| return T,
        },
        else => unreachable,
    };
}

fn Transform(comptime Original: type, comptime transform_fn: anytype) type {
    return struct {
        pub const original_type = Original;
        pub const transform = transform_fn;

        value: ErrorUnwrappedReturnOf(transform_fn),
    };
}


const MaybeStringArray = Default([]const []const u8, &.{});

pub const Initialize = struct {
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

    capabilities: ClientCapabilities,
    workspaceFolders: ?[]const types.WorkspaceFolder,
};

pub const WorkspaceFoldersChange = struct {
    params: struct {
        event: struct {
            added: []const types.WorkspaceFolder,
            removed: []const types.WorkspaceFolder,
        },
    },
};

pub const OpenDocument = struct {
    textDocument: struct {
        uri: []const u8,
        text: []const u8,
    },
};

const TextDocumentIdentifier = struct {
    uri: []const u8,
};

pub const ChangeDocument = struct {
    textDocument: TextDocumentIdentifier,
    contentChanges: std.json.Value,
};

const TextDocumentIdentifierRequest = struct {
    textDocument: TextDocumentIdentifier,
};

pub const SaveDocument = TextDocumentIdentifierRequest;
pub const CloseDocument = TextDocumentIdentifierRequest;
pub const SemanticTokensFull = TextDocumentIdentifierRequest;

const TextDocumentIdentifierPositionRequest = struct {
    params: struct {
        textDocument: TextDocumentIdentifier,
        position: types.Position,
    },
};

pub const SignatureHelp = struct {
    params: struct {
        textDocument: TextDocumentIdentifier,
        position: types.Position,
        context: ?struct {
            triggerKind: enum(u32) {
                invoked = 1,
                trigger_character = 2,
                content_change = 3,
            },
            triggerCharacter: ?[]const u8,
            isRetrigger: bool,
            activeSignatureHelp: ?types.SignatureHelp,
        },
    },
};

pub const Completion = TextDocumentIdentifierPositionRequest;
pub const GotoDefinition = TextDocumentIdentifierPositionRequest;
pub const GotoDeclaration = TextDocumentIdentifierPositionRequest;
pub const Hover = TextDocumentIdentifierPositionRequest;
pub const DocumentSymbols = TextDocumentIdentifierRequest;
pub const Formatting = TextDocumentIdentifierRequest;
pub const Rename = struct {
    params: struct {
        textDocument: TextDocumentIdentifier,
        position: types.Position,
        newName: []const u8,
    },
};

pub const References = struct {
    params: struct {
        textDocument: TextDocumentIdentifier,
        position: types.Position,
        context: struct {
            includeDeclaration: bool,
        },
    },
};
