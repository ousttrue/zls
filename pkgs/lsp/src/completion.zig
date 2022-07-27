const types = @import("./types.zig");
const requests = @import("./requests.zig");

pub const CompletionTriggerKind = enum(u8) {
    Invoked = 1,
    TriggerCharacter = 2,
    TriggerForIncompleteCompletions = 3,
};

// export interface CompletionParams extends TextDocumentPositionParams, WorkDoneProgressParams, PartialResultParams
// pub const Completion = TextDocumentIdentifierPositionRequest;
pub const Completion = struct {
    // TextDocumentPositionParams
    textDocument: requests.TextDocumentIdentifier,
    position: types.Position,
    // CompletionContext
    context: struct {
        triggerKind: CompletionTriggerKind,
        triggerCharacter: ?[]const u8,
    },
};
