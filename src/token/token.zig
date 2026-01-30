const std = @import("std");

pub const ValueType = enum {
    int,
    float,
    string,
    bool,
    time,
    nothing,

    pub fn toString(self: ValueType) []const u8 {
        return switch (self) {
            .int => "int",
            .float => "float",
            .string => "string",
            .bool => "bool",
            .time => "time",
            .nothing => "nothing",
        };
    }
};

pub const ValueKind = enum {
    int,
    float,
    string,
    bool,
    time,
    array,
    nothing,
};

pub const Array = struct {
    items: []Value,
};

pub const Value = union(ValueKind) {
    int: i64,
    float: f64,
    string: []const u8,
    bool: bool,
    time: i64,
    array: Array,
    nothing: void,
};

pub const TokenKind = enum {
    TKN_IDENTIFIER, // variable/group names
    TKN_TYPE_ASSIGN, // :
    TKN_DOT, // . (path separator)
    TKN_VALUE_ASSIGN, // =
    TKN_SLASH, // /
    TKN_STAR, // *
    TKN_PLUS, // +
    TKN_MINUS, // -
    TKN_POWER, // ^
    TKN_PERCENT, // %
    TKN_HASH, // #
    TKN_LPAREN, // (
    TKN_RPAREN, // )
    TKN_LBRACE, // {
    TKN_RBRACE, // }
    TKN_LBRACKET, // [
    TKN_RBRACKET, // ]
    TKN_GT, // >
    TKN_LT, // <
    TKN_GTE, // >=
    TKN_LTE, // <=
    TKN_EQ, // ==
    TKN_NEQ, // !=
    TKN_COMMA, // ,
    TKN_EXCLAIM, // !
    TKN_AND, // and
    TKN_OR, // or
    TKN_VAR, // var keyword
    TKN_CONST, // const keyword
    TKN_TEMP, // temp keyword
    TKN_VALUE, // value
    TKN_TYPE, // type
    TKN_GROUP, // group
    TKN_NEWLINE, // \n
    TKN_INSPECT, // ?
    TKN_LOOKUP, //
    TKN_EXPRESSION, // expression
    TKN_ASSERT, // assert keyword
    TKN_EOF, // end of file

    pub fn toString(self: TokenKind) []const u8 {
        return switch (self) {
            .TKN_IDENTIFIER => "TKN_IDENTIFIER",
            .TKN_NEWLINE => "TKN_NEWLINE",
            .TKN_EOF => "TKN_EOF",
            .TKN_DOT => "TKN_DOT",
            .TKN_EXPRESSION => "TKN_EXPRESSION",
            .TKN_VALUE_ASSIGN => "TKN_VALUE_ASSIGN",
            .TKN_TYPE_ASSIGN => "TKN_TYPE_ASSIGN",
            .TKN_SLASH => "TKN_SLASH",
            .TKN_STAR => "TKN_STAR",
            .TKN_PLUS => "TKN_PLUS",
            .TKN_MINUS => "TKN_MINUS",
            .TKN_POWER => "TKN_POWER",
            .TKN_PERCENT => "TKN_PERCENT",
            .TKN_HASH => "TKN_HASH",
            .TKN_LPAREN => "TKN_LPAREN",
            .TKN_RPAREN => "TKN_RPAREN",
            .TKN_LBRACE => "TKN_LBRACE",
            .TKN_RBRACE => "TKN_RBRACE",
            .TKN_LBRACKET => "TKN_LBRACKET",
            .TKN_RBRACKET => "TKN_RBRACKET",
            .TKN_COMMA => "TKN_COMMA",
            .TKN_EXCLAIM => "TKN_EXCLAIM",
            .TKN_AND => "TKN_AND",
            .TKN_OR => "TKN_OR",
            .TKN_VAR => "TKN_VAR",
            .TKN_CONST => "TKN_CONST",
            .TKN_TEMP => "TKN_TEMP",
            .TKN_TYPE => "TKN_TYPE",
            .TKN_GROUP => "TKN_GROUP",
            .TKN_VALUE => "TKN_VALUE",
            .TKN_GT => "TKN_GT",
            .TKN_LT => "TKN_LT",
            .TKN_GTE => "TKN_GTE",
            .TKN_LTE => "TKN_LTE",
            .TKN_EQ => "TKN_EQ",
            .TKN_NEQ => "TKN_NEQ",
            .TKN_INSPECT => "TKN_INSPECT",
            .TKN_LOOKUP => "TKN_LOOKUP",
            .TKN_ASSERT => "TKN_ASSERT",
        };
    }

    pub fn compare(self: TokenKind, other: TokenKind) bool {
        return self == other;
    }
};

pub const Token = struct {
    literal: []const u8,
    token_type: TokenKind,
    value_type: ValueType,
    line_number: usize,
    token_number: usize,
};

pub fn makeToken(literal: []const u8, token_type: TokenKind, value_type: ValueType, line_number: usize, token_number: usize) Token {
    return Token{
        .literal = literal,
        .token_type = token_type,
        .value_type = value_type,
        .line_number = line_number,
        .token_number = token_number,
    };
}
