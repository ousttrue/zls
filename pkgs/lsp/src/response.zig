const types = @import("./types.zig");
const initialize = @import("./initialize.zig");
const signature_help = @import("./signature_help.zig");
const textdocument_documentSymbol = @import("./textdocument_documentSymbol.zig");

/// Params of a response (result)
pub const ResponseParams = union(enum) {
    SignatureHelp: signature_help.SignatureHelp,
    CompletionList: types.CompletionList,
    CompletionItems: []const types.CompletionItem,
    Location: types.Location,
    Hover: types.Hover,
    DocumentSymbols: []textdocument_documentSymbol.DocumentSymbol,
    SemanticTokensFull: struct { data: []const u32 },
    TextEdits: []types.TextEdit,
    Locations: []types.Location,
    WorkspaceEdit: types.WorkspaceEdit,
    InitializeResult: initialize.InitializeResult,
    CodeLens: []types.CodeLens,
    Null: ?struct {},
};

pub const null_result_response = ResponseParams{
    .Null = null,
};

pub const ResponseError = struct {
    code: i32,
    message: []const u8,

    // Defined by JSON-RPC
    pub fn createParseError() ResponseError {
        return .{ .code = -32700, .message = "ParseError" };
    }
    pub fn createInvalidRequest() ResponseError {
        return .{ .code = -32600, .message = "InvalidRequest" };
    }
    pub fn createMethodNotFound() ResponseError {
        return .{ .code = -32601, .message = "MethodNotFound" };
    }
    pub fn createInvalidParams() ResponseError {
        return .{ .code = -32602, .message = "InvalidParams" };
    }
    pub fn createInternalError() ResponseError {
        return .{ .code = -32603, .message = "InternalError" };
    }
};

/// JSONRPC response
pub const Response = struct {
    jsonrpc: types.string = "2.0",
    id: ?i64,
    result: ResponseParams,
    @"error": ?ResponseError = null,

    pub fn createNull(id: i64) Response {
        return Response{ .id = id, .result = null_result_response };
    }

    pub fn createParseError() Response {
        return .{ .id = null, .result = null_result_response, .@"error" = ResponseError.createParseError() };
    }
    pub fn createInvalidRequest(id: ?i64) Response {
        return .{ .id = id, .result = null_result_response, .@"error" = ResponseError.createInvalidRequest() };
    }
    pub fn createMethodNotFound(id: ?i64) Response {
        return .{ .id = id, .result = null_result_response, .@"error" = ResponseError.createMethodNotFound() };
    }
    pub fn createInvalidParams(id: ?i64) Response {
        return .{ .id = id, .result = null_result_response, .@"error" = ResponseError.createInvalidParams() };
    }
    pub fn createInternalError(id: ?i64) Response {
        return .{ .id = id, .result = null_result_response, .@"error" = ResponseError.createInternalError() };
    }
};
