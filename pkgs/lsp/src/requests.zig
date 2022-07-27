//! This file contains request types zls handles.
//! Note that the parameter types may be incomplete.
//! We only define what we actually use.

const std = @import("std");
const types = @import("./types.zig");
const Default = types.Default;

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

pub const WorkspaceFoldersChange = struct {
    event: struct {
        added: []const types.WorkspaceFolder,
        removed: []const types.WorkspaceFolder,
    },
};

pub const OpenDocument = struct {
    textDocument: struct {
        uri: []const u8,
        text: []const u8,
    },
};

pub const TextDocumentIdentifier = struct {
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
    textDocument: TextDocumentIdentifier,
    position: types.Position,
};

pub const SignatureHelp = struct {
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
};

pub const GotoDefinition = TextDocumentIdentifierPositionRequest;
pub const GotoDeclaration = TextDocumentIdentifierPositionRequest;
pub const Hover = TextDocumentIdentifierPositionRequest;
pub const DocumentSymbols = TextDocumentIdentifierRequest;
pub const Formatting = TextDocumentIdentifierRequest;
pub const Rename = struct {
    textDocument: TextDocumentIdentifier,
    position: types.Position,
    newName: []const u8,
};

pub const References = struct {
    textDocument: TextDocumentIdentifier,
    position: types.Position,
    context: struct {
        includeDeclaration: bool,
    },
};
