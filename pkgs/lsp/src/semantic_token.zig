const std = @import("std");

pub const SemanticTokenType = enum(u32) {
    type,
    parameter,
    variable,
    enumMember,
    field,
    errorTag,
    function,
    keyword,
    comment,
    string,
    number,
    operator,
    builtin,
    label,
    keywordLiteral,
};

pub const SemanticTokenModifiers = packed struct {
    const Self = @This();

    namespace: bool = false,
    @"struct": bool = false,
    @"enum": bool = false,
    @"union": bool = false,
    @"opaque": bool = false,
    declaration: bool = false,
    @"async": bool = false,
    documentation: bool = false,
    generic: bool = false,

    pub fn toInt(self: Self) u32 {
        var res: u32 = 0;
        inline for (std.meta.fields(Self)) |field, i| {
            if (@field(self, field.name)) {
                res |= 1 << i;
            }
        }
        return res;
    }

    pub inline fn set(self: *Self, comptime field: []const u8) void {
        @field(self, field) = true;
    }
};
