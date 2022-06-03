const types = @import("./types.zig");

/// Params of a response (result)
pub const ResponseParams = union(enum) {
    SignatureHelp: types.SignatureHelp,
    CompletionList: types.CompletionList,
    Location: types.Location,
    Hover: types.Hover,
    DocumentSymbols: []types.DocumentSymbol,
    SemanticTokensFull: struct { data: []const u32 },
    TextEdits: []types.TextEdit,
    Locations: []types.Location,
    WorkspaceEdit: types.WorkspaceEdit,
    InitializeResult: types.InitializeResult,
    Null: ?struct {},
};

pub const null_result_response = ResponseParams{
    .Null = null,
};

// "error":{"code":-32601,"message":"NotImplemented"}}
pub const ResponseError = struct {
    code: i32,
    message: []const u8,
};

/// JSONRPC response
pub const Response = struct {
    jsonrpc: types.string = "2.0",
    id: types.RequestId,
    result: ResponseParams,
    @"error": ?ResponseError = null,

    pub fn createNull(id: types.RequestId) Response {
        return Response{ .id = id, .result = null_result_response };
    }

    pub fn createError(id: types.RequestId, e: ResponseError) Response {
        return Response{ .id = id, .result = null_result_response, .@"error" = e };
    }

    pub fn createErrorNotImplemented(id: types.RequestId) Response {
        return createError(id, .{
            .code = -32601,
            .message = "NotImplemented",
        });
    }
};
