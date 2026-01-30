const std = @import("std");
const token = @import("../token/token.zig");
const reporting = @import("../utils/reporting.zig");

const printError = std.debug.print;

pub const Lexer = struct {
    input: []const u8,
    pos: usize,
    line: usize,
    column: usize,
    token_count: usize,
    tokens: std.ArrayList(token.Token),
    lines: std.ArrayList([]const u8),
    allocator: std.mem.Allocator,
    assignment_mode: bool,

    pub fn init(allocator: std.mem.Allocator, input: []const u8) !Lexer {
        var lexer = Lexer{
            .input = input,
            .pos = 0,
            .line = 1,
            .column = 1,
            .token_count = 0,
            .tokens = .empty,
            .lines = .empty,
            .allocator = allocator,
            .assignment_mode = false,
        };
        try lexer.readLines();
        return lexer;
    }

    pub fn deinit(self: *Lexer) void {
        self.tokens.deinit(self.allocator);
        self.lines.deinit(self.allocator);
    }

    fn readLines(self: *Lexer) !void {
        var start: usize = 0;
        var i: usize = 0;
        while (i < self.input.len) : (i += 1) {
            const c = self.input[i];
            if (c == '\n' or c == '\r') {
                try self.lines.append(self.allocator, self.input[start..i]);
                if (c == '\r' and i + 1 < self.input.len and self.input[i + 1] == '\n') {
                    i += 1;
                }
                start = i + 1;
            }
        }
        if (start <= self.input.len) {
            try self.lines.append(self.allocator, self.input[start..self.input.len]);
        }
    }

    fn peek(self: *Lexer) ?u8 {
        return if (self.pos < self.input.len) self.input[self.pos] else null;
    }

    fn peekNext(self: *Lexer) ?u8 {
        return if (self.pos + 1 < self.input.len) self.input[self.pos + 1] else null;
    }

    fn advance(self: *Lexer) void {
        if (self.pos < self.input.len) {
            const current = self.input[self.pos];
            self.pos += 1;
            if (current == '\n') {
                self.line += 1;
                self.column = 1;
            } else {
                self.column += 1;
            }
        }
    }

    fn skipWhitespace(self: *Lexer) void {
        while (self.peek()) |c| {
            switch (c) {
                ' ', '\t' => {
                    self.advance();
                },
                else => break,
            }
        }
    }

    fn skipMultiline(self: *Lexer, start_line: usize, start_column: usize) !void {
        while (self.peek()) |c| {
            if (c == '*' and self.peekNext() == '/') {
                self.advance();
                self.advance();
                return;
            }
            self.advance();
        }

        if (start_line > 0 and start_line <= self.lines.items.len) {
            const line_content = self.lines.items[start_line - 1];
            const column = if (start_column > 0) start_column - 1 else 0;
            reporting.underline(line_content, column, 2);
        }
        printError("Unterminated multiline comment at line {d}, column {d}\n", .{ start_line, start_column });
        return error.UnterminatedMultilineComment;
    }

    fn skipLine(self: *Lexer) void {
        while (self.peek()) |c| {
            if (c == '\n' or c == '\r') break;
            self.advance();
        }
    }

    fn handleNewline(self: *Lexer) !void {
        self.assignment_mode = false;
        if (self.tokens.items.len == 0) {
            return;
        }

        const last_token = self.tokens.items[self.tokens.items.len - 1];
        if (last_token.token_type == .TKN_NEWLINE) {
            return;
        }

        const current_column = self.column;
        try self.tokens.append(self.allocator, .{
            .literal = "\\n",
            .token_type = .TKN_NEWLINE,
            .value_type = .nothing,
            .line_number = self.line,
            .token_number = current_column,
        });
        self.token_count += 1;
    }

    pub fn tokenize(self: *Lexer) !void {
        while (self.peek()) |c| {
            if (c == '\r') {
                try self.handleNewline();
                self.advance();
                if (self.peek() == '\n') {
                    self.advance();
                }
                continue;
            } else if (c == '\n') {
                try self.handleNewline();
                self.advance();
                if (self.peek() == '\r') {
                    self.advance();
                }
                continue;
            }

            if (c == ' ' or c == '\t') {
                self.skipWhitespace();
                continue;
            }

            if (c == '?') {
                const current_column = self.column;
                self.advance();
                try self.tokens.append(self.allocator, .{
                    .literal = "?",
                    .token_type = .TKN_INSPECT,
                    .value_type = .nothing,
                    .line_number = self.line,
                    .token_number = current_column,
                });
                self.token_count += 1;
                continue;
            }

            if (c == '/') {
                const current_column = self.column;
                const start_line = self.line;
                self.advance();
                if (self.peek() == '*') {
                    try self.skipMultiline(start_line, current_column);
                } else if (self.peek() == '/') {
                    self.advance();
                    self.skipLine();
                } else {
                    try self.tokens.append(self.allocator, .{
                        .literal = "/",
                        .token_type = .TKN_SLASH,
                        .value_type = .nothing,
                        .line_number = self.line,
                        .token_number = current_column,
                    });
                    self.token_count += 1;
                }
                continue;
            }

            if (c == 'i' or c == 's' or c == 'b' or c == 'f' or c == 't' or c == 'e' or c == 'T' or c == 'F' or c == 'I' or c == 'S' or c == 'B' or c == 'E' or c == 'a' or c == 'o') {
                const current_column = self.column;
                const word = try self.readWord();

                const is_type_word =
                    (c == 'i' and std.mem.eql(u8, word, "int")) or
                    (c == 'f' and std.mem.eql(u8, word, "float")) or
                    (c == 's' and std.mem.eql(u8, word, "string")) or
                    (c == 'b' and std.mem.eql(u8, word, "bool")) or
                    (c == 't' and std.mem.eql(u8, word, "time")) or
                    (c == 'e' and std.mem.eql(u8, word, "env")) or
                    (c == 'I' and std.mem.eql(u8, word, "INT")) or
                    (c == 'F' and std.mem.eql(u8, word, "FLOAT")) or
                    (c == 'S' and std.mem.eql(u8, word, "STRING")) or
                    (c == 'B' and std.mem.eql(u8, word, "BOOL")) or
                    (c == 'T' and std.mem.eql(u8, word, "TIME")) or
                    (c == 'E' and std.mem.eql(u8, word, "ENV"));

                const is_value_word =
                    (c == 't' and std.mem.eql(u8, word, "true")) or
                    (c == 'f' and std.mem.eql(u8, word, "false")) or
                    (c == 'T' and std.mem.eql(u8, word, "TRUE")) or
                    (c == 'F' and std.mem.eql(u8, word, "FALSE"));

                const is_temp_keyword = std.mem.eql(u8, word, "temp");

                if (is_type_word) {
                    try self.tokens.append(self.allocator, .{
                        .literal = word,
                        .token_type = .TKN_TYPE,
                        .value_type = .nothing,
                        .line_number = self.line,
                        .token_number = current_column,
                    });
                } else if (is_value_word) {
                    try self.tokens.append(self.allocator, .{
                        .literal = word,
                        .token_type = .TKN_VALUE,
                        .value_type = .bool,
                        .line_number = self.line,
                        .token_number = current_column,
                    });
                } else if (is_temp_keyword) {
                    try self.tokens.append(self.allocator, .{
                        .literal = word,
                        .token_type = .TKN_TEMP,
                        .value_type = .nothing,
                        .line_number = self.line,
                        .token_number = current_column,
                    });
                } else if (c == 'a' and std.mem.eql(u8, word, "and")) {
                    try self.tokens.append(self.allocator, .{
                        .literal = word,
                        .token_type = .TKN_AND,
                        .value_type = .nothing,
                        .line_number = self.line,
                        .token_number = current_column,
                    });
                } else if (c == 'o' and std.mem.eql(u8, word, "or")) {
                    try self.tokens.append(self.allocator, .{
                        .literal = word,
                        .token_type = .TKN_OR,
                        .value_type = .nothing,
                        .line_number = self.line,
                        .token_number = current_column,
                    });
                } else {
                    if (self.assignment_mode) {
                        try self.tokens.append(self.allocator, .{
                            .literal = word,
                            .token_type = .TKN_LOOKUP,
                            .value_type = .nothing,
                            .line_number = self.line,
                            .token_number = current_column,
                        });
                    } else {
                        try self.tokens.append(self.allocator, .{
                            .literal = word,
                            .token_type = .TKN_IDENTIFIER,
                            .value_type = .nothing,
                            .line_number = self.line,
                            .token_number = current_column,
                        });
                    }
                }

                self.token_count += 1;
                continue;
            }

            switch (c) {
                ':' => {
                    const current_column = self.column;
                    self.advance();
                    try self.tokens.append(self.allocator, .{
                        .literal = ":",
                        .token_type = .TKN_TYPE_ASSIGN,
                        .value_type = .nothing,
                        .line_number = self.line,
                        .token_number = current_column,
                    });
                    self.token_count += 1;
                },
                '=' => {
                    const current_column = self.column;
                    self.advance();
                    if (self.peek() == '=') {
                        self.advance();
                        try self.tokens.append(self.allocator, .{
                            .literal = "==",
                            .token_type = .TKN_EQ,
                            .value_type = .nothing,
                            .line_number = self.line,
                            .token_number = current_column,
                        });
                    } else {
                        try self.tokens.append(self.allocator, .{
                            .literal = "=",
                            .token_type = .TKN_VALUE_ASSIGN,
                            .value_type = .nothing,
                            .line_number = self.line,
                            .token_number = current_column,
                        });
                        self.assignment_mode = true;
                    }
                    self.token_count += 1;
                },
                '>' => {
                    const current_column = self.column;
                    self.advance();
                    if (self.peek() == '=') {
                        self.advance();
                        try self.tokens.append(self.allocator, .{
                            .literal = ">=",
                            .token_type = .TKN_GTE,
                            .value_type = .nothing,
                            .line_number = self.line,
                            .token_number = current_column,
                        });
                    } else {
                        try self.tokens.append(self.allocator, .{
                            .literal = ">",
                            .token_type = .TKN_GT,
                            .value_type = .nothing,
                            .line_number = self.line,
                            .token_number = current_column,
                        });
                    }
                    self.token_count += 1;
                },
                '<' => {
                    const current_column = self.column;
                    self.advance();
                    if (self.peek() == '=') {
                        self.advance();
                        try self.tokens.append(self.allocator, .{
                            .literal = "<=",
                            .token_type = .TKN_LTE,
                            .value_type = .nothing,
                            .line_number = self.line,
                            .token_number = current_column,
                        });
                    } else {
                        try self.tokens.append(self.allocator, .{
                            .literal = "<",
                            .token_type = .TKN_LT,
                            .value_type = .nothing,
                            .line_number = self.line,
                            .token_number = current_column,
                        });
                    }
                    self.token_count += 1;
                },
                '!' => {
                    const current_column = self.column;
                    self.advance();
                    if (self.peek() == '=') {
                        self.advance();
                        try self.tokens.append(self.allocator, .{
                            .literal = "!=",
                            .token_type = .TKN_NEQ,
                            .value_type = .nothing,
                            .line_number = self.line,
                            .token_number = current_column,
                        });
                    } else {
                        try self.tokens.append(self.allocator, .{
                            .literal = "!",
                            .token_type = .TKN_EXCLAIM,
                            .value_type = .nothing,
                            .line_number = self.line,
                            .token_number = current_column,
                        });
                    }
                    self.token_count += 1;
                },
                '-' => {
                    const current_column = self.column;
                    try self.tokens.append(self.allocator, .{
                        .literal = "-",
                        .token_type = .TKN_MINUS,
                        .value_type = .nothing,
                        .line_number = self.line,
                        .token_number = current_column,
                    });
                    self.token_count += 1;
                    self.advance();
                },
                '.' => {
                    const current_column = self.column;
                    try self.tokens.append(self.allocator, .{
                        .literal = ".",
                        .token_type = .TKN_DOT,
                        .value_type = .nothing,
                        .line_number = self.line,
                        .token_number = current_column,
                    });
                    self.token_count += 1;
                    self.advance();
                },
                '+' => {
                    const current_column = self.column;
                    try self.tokens.append(self.allocator, .{
                        .literal = "+",
                        .token_type = .TKN_PLUS,
                        .value_type = .nothing,
                        .line_number = self.line,
                        .token_number = current_column,
                    });
                    self.token_count += 1;
                    self.advance();
                },
                '^' => {
                    const current_column = self.column;
                    try self.tokens.append(self.allocator, .{
                        .literal = "^",
                        .token_type = .TKN_POWER,
                        .value_type = .nothing,
                        .line_number = self.line,
                        .token_number = current_column,
                    });
                    self.token_count += 1;
                    self.advance();
                },
                '%' => {
                    const current_column = self.column;
                    try self.tokens.append(self.allocator, .{
                        .literal = "%",
                        .token_type = .TKN_PERCENT,
                        .value_type = .nothing,
                        .line_number = self.line,
                        .token_number = current_column,
                    });
                    self.token_count += 1;
                    self.advance();
                },
                '#' => {
                    const current_column = self.column;
                    self.advance(); // consume '#'
                    self.skipWhitespace();

                    // Check if next word is "assert"
                    if (self.peek()) |next_char| {
                        if ((next_char >= 'a' and next_char <= 'z') or (next_char >= 'A' and next_char <= 'Z')) {
                            const word = try self.readWord();
                            if (std.mem.eql(u8, word, "assert")) {
                                try self.tokens.append(self.allocator, .{
                                    .literal = "#assert",
                                    .token_type = .TKN_ASSERT,
                                    .value_type = .nothing,
                                    .line_number = self.line,
                                    .token_number = current_column,
                                });
                                self.token_count += 1;
                                continue;
                            }
                        }
                    }

                    // Not "assert", emit regular hash token
                    try self.tokens.append(self.allocator, .{
                        .literal = "#",
                        .token_type = .TKN_HASH,
                        .value_type = .nothing,
                        .line_number = self.line,
                        .token_number = current_column,
                    });
                    self.token_count += 1;
                },
                '*' => {
                    const current_column = self.column;
                    try self.tokens.append(self.allocator, .{
                        .literal = "*",
                        .token_type = .TKN_STAR,
                        .value_type = .nothing,
                        .line_number = self.line,
                        .token_number = current_column,
                    });
                    self.token_count += 1;
                    self.advance();
                },
                '(' => {
                    const current_column = self.column;
                    try self.tokens.append(self.allocator, .{
                        .literal = "(",
                        .token_type = .TKN_LPAREN,
                        .value_type = .nothing,
                        .line_number = self.line,
                        .token_number = current_column,
                    });
                    self.token_count += 1;
                    self.advance();
                },
                ')' => {
                    const current_column = self.column;
                    try self.tokens.append(self.allocator, .{
                        .literal = ")",
                        .token_type = .TKN_RPAREN,
                        .value_type = .nothing,
                        .line_number = self.line,
                        .token_number = current_column,
                    });
                    self.token_count += 1;
                    self.advance();
                },
                '{' => {
                    const current_column = self.column;
                    try self.tokens.append(self.allocator, .{
                        .literal = "{",
                        .token_type = .TKN_LBRACE,
                        .value_type = .nothing,
                        .line_number = self.line,
                        .token_number = current_column,
                    });
                    self.token_count += 1;
                    self.advance();
                },
                '}' => {
                    const current_column = self.column;
                    try self.tokens.append(self.allocator, .{
                        .literal = "}",
                        .token_type = .TKN_RBRACE,
                        .value_type = .nothing,
                        .line_number = self.line,
                        .token_number = current_column,
                    });
                    self.token_count += 1;
                    self.advance();
                },
                '[' => {
                    const current_column = self.column;
                    try self.tokens.append(self.allocator, .{
                        .literal = "[",
                        .token_type = .TKN_LBRACKET,
                        .value_type = .nothing,
                        .line_number = self.line,
                        .token_number = current_column,
                    });
                    self.token_count += 1;
                    self.advance();
                },
                ']' => {
                    const current_column = self.column;
                    try self.tokens.append(self.allocator, .{
                        .literal = "]",
                        .token_type = .TKN_RBRACKET,
                        .value_type = .nothing,
                        .line_number = self.line,
                        .token_number = current_column,
                    });
                    self.token_count += 1;
                    self.advance();
                },
                ',' => {
                    const current_column = self.column;
                    try self.tokens.append(self.allocator, .{
                        .literal = ",",
                        .token_type = .TKN_COMMA,
                        .value_type = .nothing,
                        .line_number = self.line,
                        .token_number = current_column,
                    });
                    self.token_count += 1;
                    self.advance();
                },
                '"' => try self.readString(),
                'a'...'z', 'A'...'Z', '_' => {
                    const current_column = self.column;
                    const word = try self.readWord();
                    try self.makeWordToken(word, current_column);
                },
                '0'...'9' => try self.readNumber(),
                else => {
                    const line_content = self.lines.items[self.line - 1];
                    const column = self.column;
                    reporting.underline(line_content, column - 1, 1);
                    printError("Unexpected character: {c}\n", .{c});
                    printError("Line: {d} Column: {d}\n", .{ self.line, self.column });
                    return error.UnexpectedCharacter;
                },
            }
        }

        try self.tokens.append(self.allocator, .{
            .literal = "EOF",
            .token_type = .TKN_EOF,
            .value_type = .nothing,
            .line_number = self.line,
            .token_number = self.column,
        });
        self.token_count += 1;
    }

    fn readWord(self: *Lexer) ![]const u8 {
        const start = self.pos;
        while (self.peek()) |c| {
            switch (c) {
                'a'...'z', 'A'...'Z', '0'...'9', '_' => self.advance(),
                else => break,
            }
        }
        return self.input[start..self.pos];
    }

    fn makeWordToken(self: *Lexer, word: []const u8, column: usize) !void {
        if (std.mem.eql(u8, word, "temp")) {
            try self.tokens.append(self.allocator, .{
                .literal = word,
                .token_type = .TKN_TEMP,
                .value_type = .nothing,
                .line_number = self.line,
                .token_number = column,
            });
        } else if (std.mem.eql(u8, word, "var")) {
            try self.tokens.append(self.allocator, .{
                .literal = word,
                .token_type = .TKN_VAR,
                .value_type = .nothing,
                .line_number = self.line,
                .token_number = column,
            });
        } else if (std.mem.eql(u8, word, "const")) {
            try self.tokens.append(self.allocator, .{
                .literal = word,
                .token_type = .TKN_CONST,
                .value_type = .nothing,
                .line_number = self.line,
                .token_number = column,
            });
        } else if (std.meta.stringToEnum(token.ValueType, word)) |value_type| {
            try self.tokens.append(self.allocator, .{
                .literal = word,
                .token_type = .TKN_VALUE,
                .value_type = value_type,
                .line_number = self.line,
                .token_number = column,
            });
        } else {
            if (self.assignment_mode) {
                try self.tokens.append(self.allocator, .{
                    .literal = word,
                    .token_type = .TKN_LOOKUP,
                    .value_type = .nothing,
                    .line_number = self.line,
                    .token_number = column,
                });
            } else {
                try self.tokens.append(self.allocator, .{
                    .literal = word,
                    .token_type = .TKN_IDENTIFIER,
                    .value_type = .nothing,
                    .line_number = self.line,
                    .token_number = column,
                });
            }
        }
        self.token_count += 1;
    }

    fn readString(self: *Lexer) !void {
        const start = self.pos;
        const current_column = self.column;
        self.advance();
        while (self.peek()) |c| {
            if (c == '"') {
                self.advance();
                break;
            }
            if (c == '\\') {
                self.advance();
                if (self.peek()) |_| self.advance();
            } else {
                self.advance();
            }
        }

        if (self.peek() == null) {
            const line_content = self.lines.items[self.line - 1];
            const column = current_column - 1;
            reporting.underline(line_content, column, self.pos - start);
            printError("Unterminated string at line {d}, column {d}\n", .{ self.line, current_column });
            return error.UnterminatedString;
        }

        try self.tokens.append(self.allocator, .{
            .literal = self.input[start..self.pos],
            .token_type = .TKN_VALUE,
            .value_type = .string,
            .line_number = self.line,
            .token_number = current_column,
        });
        self.token_count += 1;
    }

    fn readNumber(self: *Lexer) !void {
        const start = self.pos;
        const current_column = self.column;
        var has_dot = false;
        var is_valid = true;

        while (self.peek()) |c| {
            switch (c) {
                '0'...'9' => self.advance(),
                '.' => {
                    if (has_dot) {
                        is_valid = false;
                        break;
                    }
                    has_dot = true;
                    self.advance();
                    if (self.peek()) |next| {
                        if (next < '0' or next > '9') {
                            is_valid = false;
                            break;
                        }
                    } else {
                        is_valid = false;
                        break;
                    }
                },
                else => break,
            }
        }

        if (!is_valid) {
            printError("Invalid number format at line {d}, column {d}\n", .{ self.line, current_column });
            return error.InvalidNumberFormat;
        }

        try self.tokens.append(self.allocator, .{
            .literal = self.input[start..self.pos],
            .token_type = .TKN_VALUE,
            .value_type = if (has_dot) .float else .int,
            .line_number = self.line,
            .token_number = current_column,
        });
        self.token_count += 1;
    }

    pub fn dumpLexer(self: *Lexer) void {
        for (self.tokens.items) |t| {
            if (t.token_type == .TKN_NEWLINE or t.token_type == .TKN_EOF) continue;
            printError("{s} (kind: {s}, line {d}, token {d})\n", .{
                t.literal,
                t.token_type.toString(),
                t.line_number,
                t.token_number,
            });
        }
    }
};
