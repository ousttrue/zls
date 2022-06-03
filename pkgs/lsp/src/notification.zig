const std = @import("std");
const types = @import("./types.zig");
const string = types.string;

/// Type of a debug message
pub const MessageType = enum(i64) {
    Error = 1,
    Warning = 2,
    Info = 3,
    Log = 4,

    pub fn jsonStringify(value: MessageType, options: std.json.StringifyOptions, out_stream: anytype) !void {
        try std.json.stringify(@enumToInt(value), options, out_stream);
    }
};

pub const DiagnosticSeverity = enum(i64) {
    Error = 1,
    Warning = 2,
    Information = 3,
    Hint = 4,

    pub fn jsonStringify(value: DiagnosticSeverity, options: std.json.StringifyOptions, out_stream: anytype) !void {
        try std.json.stringify(@enumToInt(value), options, out_stream);
    }
};

pub const Diagnostic = struct {
    range: types.Range,
    severity: DiagnosticSeverity,
    code: string,
    source: string,
    message: string,
};

pub const NotificationParams = union(enum) {
    LogMessage: struct {
        type: MessageType,
        message: string,
    },
    PublishDiagnostics: struct {
        uri: string,
        diagnostics: []Diagnostic,
    },
    ShowMessage: struct {
        type: MessageType,
        message: string,
    },
};

/// JSONRPC notifications
pub const Notification = struct {
    jsonrpc: string = "2.0",
    method: string,
    params: NotificationParams,
};
