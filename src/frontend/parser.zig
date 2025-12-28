const std = @import("std");
const TokenImport = @import("../token/token.zig");
const Token = TokenImport.Token;
const TokenKind = TokenImport.TokenKind;
const ValueType = TokenImport.ValueType;
const Value = TokenImport.Value;
const Reporting = @import("../utils/reporting.zig");
const printError = std.debug.print;
const printDebug = std.debug.print;
const printInspect = std.debug.print;

const Group = struct {
    name: []const u8,
    type: ?[]const u8,
};

pub const ParsedToken = struct {
    token_type: TokenKind,
    value_type: ValueType,
    value: Value,
    expression: ?[]Token,
    literal: []const u8,
    line_number: usize,
    token_number: usize,
    is_mutable: bool,
    is_temporary: bool,
    has_decl_prefix: bool,

    pub fn deinit(self: *ParsedToken, allocator: std.mem.Allocator) void {
        if (self.expression) |expr| {
            allocator.free(expr);
            self.expression = null;
        }
    }
};

pub const Parser = struct {
    tokens: []Token,
    groups: std.ArrayList(Group),
    parsed_tokens: std.ArrayList(ParsedToken),
    allocator: std.mem.Allocator,
    is_mutable: bool,
    is_temporary: bool,
    has_decl_prefix: bool,

    pub fn init(allocator: std.mem.Allocator, tokens: []Token) Parser {
        return Parser{
            .tokens = tokens,
            .groups = std.ArrayList(Group).init(allocator),
            .parsed_tokens = std.ArrayList(ParsedToken).init(allocator),
            .allocator = allocator,
            .is_mutable = false,
            .is_temporary = false,
            .has_decl_prefix = false,
        };
    }

    pub fn deinit(self: *Parser) void {
        // Clean up any allocated memory in parsed tokens
        for (self.parsed_tokens.items) |*token| {
            token.deinit(self.allocator);
        }

        self.groups.deinit();
        self.parsed_tokens.deinit();
        self.tokens = undefined;
    }

    pub fn parse(self: *Parser) !void {
        if (self.tokens.len == 0) {
            return error.NoTokens;
        }

        var current_token = self.tokens[0];
        var current_index: usize = 0;
        var has_equals: bool = false;
        var line_prefixed_with_groups: bool = false;

        while (current_index < self.tokens.len) {
            current_token = self.tokens[current_index];
            current_index += 1;

            switch (current_token.token_type) {
                .TKN_VAR => {
                    // After seeing `var`, the next token on this line must be
                    // an identifier.
                    if (!TokenKind.compare(self.tokens[current_index].token_type, .TKN_IDENTIFIER)) {
                        Reporting.throwError(
                            "VAR must be followed by an identifier (line {d}, token {d})\n",
                            .{ current_token.line_number, current_token.token_number },
                        );
                        return error.InvalidVAR;
                    }
                    self.is_mutable = true;
                    self.has_decl_prefix = true;
                },
                .TKN_CONST => {
                    // After seeing `const`, the next token on this line must be
                    // an identifier.
                    if (!TokenKind.compare(self.tokens[current_index].token_type, .TKN_IDENTIFIER)) {
                        Reporting.throwError(
                            "CONST must be followed by an identifier (line {d}, token {d})\n",
                            .{ current_token.line_number, current_token.token_number },
                        );
                        return error.InvalidCONST;
                    }
                    self.is_mutable = false;
                    self.has_decl_prefix = true;
                },
                .TKN_TEMP => {
                    // `temp` may prefix a declaration; it must be followed by
                    // either an identifier, `var`, or `const` on the same line.
                    if (!TokenKind.compare(self.tokens[current_index].token_type, .TKN_VAR) and
                        !TokenKind.compare(self.tokens[current_index].token_type, .TKN_CONST))
                    {
                        Reporting.throwError(
                            "TEMP must be followed by var or const (line {d}, token {d})\n",
                            .{ current_token.line_number, current_token.token_number },
                        );
                        return error.InvalidTEMP;
                    }
                    self.is_temporary = true;
                    self.has_decl_prefix = true;
                },
                .TKN_IDENTIFIER => {
                    // If this is the first significant token on a new line and we are inside group blocks,
                    // prefix the line with the current group path so downstream processing can infer scope correctly.
                    if (!line_prefixed_with_groups and self.groups.items.len > 0) {
                        for (self.groups.items) |g| {
                            try self.parsed_tokens.append(ParsedToken{
                                .token_type = .TKN_GROUP,
                                .literal = g.name,
                                .expression = null,
                                .value_type = .nothing,
                                .value = .{ .nothing = {} },
                                .line_number = current_token.line_number,
                                .token_number = current_token.token_number,
                                .is_mutable = false,
                                .is_temporary = false,
                                .has_decl_prefix = false,
                            });
                        }
                        line_prefixed_with_groups = true;
                    }

                    if (self.tokens[current_index].token_type == .TKN_ARROW) {
                        if (self.tokens[current_index + 1].token_type == .TKN_LBRACE) {
                            continue;
                        } else if (self.tokens[current_index + 1].token_type == .TKN_TYPE_ASSIGN) {
                            continue;
                        }
                        // Add group token to represent the path, but do NOT push to groups stack since there's no brace-opened scope
                        if (self.parsed_tokens.items.len == 0 or
                            self.parsed_tokens.items[self.parsed_tokens.items.len - 1].token_type != .TKN_GROUP or
                            !std.mem.eql(u8, self.parsed_tokens.items[self.parsed_tokens.items.len - 1].literal, current_token.literal))
                        {
                            try self.parsed_tokens.append(ParsedToken{
                                .token_type = .TKN_GROUP,
                                .literal = current_token.literal,
                                .expression = null,
                                .value_type = .nothing,
                                .value = .{ .nothing = {} },
                                .line_number = current_token.line_number,
                                .token_number = current_token.token_number,
                                .is_mutable = false,
                                .is_temporary = false,
                                .has_decl_prefix = false,
                            });
                        }
                        continue;
                    }
                    if (self.tokens[current_index].token_type == .TKN_TYPE_ASSIGN or
                        self.tokens[current_index].token_type == .TKN_VALUE_ASSIGN or
                        self.tokens[current_index].token_type == .TKN_NEWLINE or
                        self.tokens[current_index].token_type == .TKN_INSPECT)
                    {
                        try self.parsed_tokens.append(ParsedToken{
                            .token_type = .TKN_IDENTIFIER,
                            .literal = current_token.literal,
                            .expression = null,
                            .value_type = .nothing,
                            .value = .{ .nothing = {} },
                            .line_number = current_token.line_number,
                            .token_number = current_token.token_number,
                            .is_mutable = self.is_mutable,
                            .is_temporary = self.is_temporary,
                            .has_decl_prefix = self.has_decl_prefix,
                        });
                        if (self.tokens[current_index].token_type == .TKN_TYPE_ASSIGN) {
                            continue;
                        }
                        if (self.groups.items.len > 0) {
                            if (self.groups.items[self.groups.items.len - 1].type) |t| {
                                try self.parsed_tokens.append(ParsedToken{
                                    .token_type = .TKN_TYPE,
                                    .literal = t,
                                    .expression = null,
                                    .value_type = .nothing,
                                    .value = .{ .nothing = {} },
                                    .line_number = current_token.line_number,
                                    .token_number = current_token.token_number,
                                    .is_mutable = false,
                                    .is_temporary = false,
                                    .has_decl_prefix = false,
                                });
                            }
                        }
                        continue;
                    }
                },
                .TKN_LOOKUP => {
                    // Prefix line with current group path if needed
                    if (!line_prefixed_with_groups and self.groups.items.len > 0) {
                        for (self.groups.items) |g| {
                            try self.parsed_tokens.append(ParsedToken{
                                .token_type = .TKN_GROUP,
                                .literal = g.name,
                                .expression = null,
                                .value_type = .nothing,
                                .value = .{ .nothing = {} },
                                .line_number = current_token.line_number,
                                .token_number = current_token.token_number,
                                .is_mutable = false,
                                .is_temporary = false,
                                .has_decl_prefix = false,
                            });
                        }
                        line_prefixed_with_groups = true;
                    }

                    if (self.tokens[current_index].token_type == .TKN_ARROW) {
                        if (self.tokens[current_index + 1].token_type == .TKN_LBRACE) {
                            continue;
                        } else if (self.tokens[current_index + 1].token_type == .TKN_TYPE_ASSIGN) {
                            continue;
                        }
                        try self.parsed_tokens.append(ParsedToken{
                            .token_type = .TKN_GROUP,
                            .literal = current_token.literal,
                            .expression = null,
                            .value_type = .nothing,
                            .value = .{ .nothing = {} },
                            .line_number = current_token.line_number,
                            .token_number = current_token.token_number,
                            .is_mutable = false,
                            .is_temporary = false,
                            .has_decl_prefix = false,
                        });
                        continue;
                    }
                    try self.parsed_tokens.append(ParsedToken{
                        .token_type = .TKN_LOOKUP,
                        .literal = current_token.literal,
                        .expression = null,
                        .value_type = .nothing,
                        .value = .{ .nothing = {} },
                        .line_number = current_token.line_number,
                        .token_number = current_token.token_number,
                        .is_mutable = false,
                        .is_temporary = false,
                        .has_decl_prefix = false,
                    });
                    continue;
                },
                .TKN_RBRACKET => {
                    try self.parsed_tokens.append(ParsedToken{
                        .token_type = .TKN_RBRACKET,
                        .literal = current_token.literal,
                        .expression = null,
                        .value_type = .nothing,
                        .value = .{ .nothing = {} },
                        .line_number = current_token.line_number,
                        .token_number = current_token.token_number,
                        .is_mutable = false,
                        .is_temporary = false,
                        .has_decl_prefix = false,
                    });
                    continue;
                },
                .TKN_LBRACKET => {
                    try self.parsed_tokens.append(ParsedToken{
                        .token_type = .TKN_LBRACKET,
                        .literal = current_token.literal,
                        .expression = null,
                        .value_type = .nothing,
                        .value = .{ .nothing = {} },
                        .line_number = current_token.line_number,
                        .token_number = current_token.token_number,
                        .is_mutable = false,
                        .is_temporary = false,
                        .has_decl_prefix = false,
                    });
                    continue;
                },
                .TKN_LBRACE => {
                    // Enter a new group scope. We support two patterns:
                    //   name { ... }
                    //   name :: Type { ... }
                    var type_to_add: ?[]const u8 = null;
                    var name_index: ?usize = null;

                    // Typed group: <identifier> :: <type> {
                    if (current_index >= 4 and
                        self.tokens[current_index - 3].token_type == .TKN_TYPE_ASSIGN and
                        self.tokens[current_index - 2].token_type == .TKN_TYPE and
                        self.tokens[current_index - 4].token_type == .TKN_IDENTIFIER)
                    {
                        type_to_add = self.tokens[current_index - 2].literal;
                        name_index = current_index - 4;
                    }
                    // Untyped group: <identifier> {
                    else if (current_index >= 2 and
                        self.tokens[current_index - 2].token_type == .TKN_IDENTIFIER)
                    {
                        name_index = current_index - 2;
                    }

                    if (name_index) |idx| {
                        const token_to_add = self.tokens[idx];
                        try self.groups.append(Group{ .name = token_to_add.literal, .type = type_to_add });
                    }
                    continue;
                },
                .TKN_RBRACE => {
                    _ = self.groups.pop();
                    continue;
                },
                .TKN_VALUE_ASSIGN => {
                    if (has_equals) {
                        return error.MultipleAssignments;
                    }
                    has_equals = true;

                    const assign_line: std.ArrayList(Token) = grabLine(self, current_token) catch |err| {
                        printError("Error grabbing line: {s}\n", .{@errorName(err)});
                        return error.ErrorGrabbingLine;
                    };
                    defer assign_line.deinit();
                    const if_expression = ifExpression(assign_line.items);
                    if (if_expression) {
                        try self.parsed_tokens.append(ParsedToken{
                            .token_type = .TKN_VALUE_ASSIGN,
                            .literal = current_token.literal,
                            .expression = null,
                            .value_type = .nothing,
                            .value = .{ .nothing = {} },
                            .line_number = assign_line.items[0].line_number,
                            .token_number = current_token.token_number,
                            .is_mutable = false,
                            .is_temporary = false,
                            .has_decl_prefix = false,
                        });

                        // Clone the tokens to ensure they remain valid
                        var expression_tokens = try self.allocator.alloc(Token, assign_line.items.len);
                        for (assign_line.items, 0..) |token, i| {
                            expression_tokens[i] = token;
                        }

                        // Only add expression tokens if there are actually tokens to process
                        if (expression_tokens.len > 0) {
                            try self.parsed_tokens.append(ParsedToken{
                                .token_type = .TKN_EXPRESSION,
                                .literal = "Expression",
                                .value_type = .nothing,
                                .value = .{ .nothing = {} },
                                .expression = expression_tokens,
                                .line_number = assign_line.items[0].line_number,
                                .token_number = current_token.token_number,
                                .is_mutable = false,
                                .is_temporary = false,
                                .has_decl_prefix = false,
                            });
                        } else {
                            // If no expression tokens, don't add an expression token and free the allocated memory
                            self.allocator.free(expression_tokens);
                            printError("Warning: Empty expression detected\n", .{});
                            return error.EmptyExpression;
                        }
                    } else {
                        try self.parsed_tokens.append(ParsedToken{
                            .token_type = .TKN_VALUE_ASSIGN,
                            .literal = current_token.literal,
                            .expression = null,
                            .value_type = .nothing,
                            .value = .{ .nothing = {} },
                            .line_number = assign_line.items[0].line_number,
                            .token_number = current_token.token_number,
                            .is_mutable = false,
                            .is_temporary = false,
                            .has_decl_prefix = false,
                        });
                    }
                    continue;
                },
                .TKN_TYPE_ASSIGN => {
                    // Enforce that explicit type annotations (`::`) on value
                    // declarations are only used when the line is prefixed
                    // with `var` or `const`. Typed group scopes
                    // (`name :: Type { ... }`) are exempt from this rule.

                    var is_typed_group = false;
                    const idx = current_index;
                    if (idx >= 3 and idx < self.tokens.len) {
                        const maybe_name = self.tokens[idx - 2];
                        const maybe_type = self.tokens[idx];
                        const maybe_lbrace = if (idx + 1 < self.tokens.len)
                            self.tokens[idx + 1]
                        else
                            null;

                        if (maybe_name.token_type == .TKN_IDENTIFIER and
                            maybe_type.token_type == .TKN_TYPE and
                            maybe_lbrace != null and maybe_lbrace.?.token_type == .TKN_LBRACE)
                        {
                            is_typed_group = true;
                        }
                    }

                    if (!is_typed_group and !self.has_decl_prefix) {
                        Reporting.throwError(
                            "Type annotation must be prefixed with var or const (line {d}, token {d})\n",
                            .{ current_token.line_number, current_token.token_number },
                        );
                        return error.TypeAnnotationMissingVarOrConst;
                    }

                    // We never emit a token for `::` itself.
                    continue;
                },
                .TKN_NEWLINE => {
                    has_equals = false;
                    line_prefixed_with_groups = false;
                    // Reset declaration context at end of line.
                    self.is_mutable = false;
                    self.is_temporary = false;
                    self.has_decl_prefix = false;
                    try self.parsed_tokens.append(ParsedToken{
                        .token_type = .TKN_NEWLINE,
                        .literal = current_token.literal,
                        .expression = null,
                        .value_type = .nothing,
                        .value = .{ .nothing = {} },
                        .line_number = current_token.line_number,
                        .token_number = current_token.token_number,
                        .is_mutable = false,
                        .is_temporary = false,
                        .has_decl_prefix = false,
                    });
                    continue;
                },
                .TKN_TYPE => {
                    // Standalone type tokens (e.g. in variable declarations) are
                    // forwarded; group-level types are handled when entering the
                    // corresponding `{` block.
                    try self.parsed_tokens.append(ParsedToken{
                        .token_type = .TKN_TYPE,
                        .literal = current_token.literal,
                        .expression = null,
                        .value_type = .nothing,
                        .value = .{ .nothing = {} },
                        .line_number = current_token.line_number,
                        .token_number = current_token.token_number,
                        .is_mutable = false,
                        .is_temporary = false,
                        .has_decl_prefix = false,
                    });
                    continue;
                },
                .TKN_VALUE => {
                    // Parse the value from the literal based on the token's value type
                    const parsed_value = parseValueFromLiteral(current_token.literal, current_token.value_type);
                    try self.parsed_tokens.append(ParsedToken{
                        .token_type = .TKN_VALUE,
                        .literal = current_token.literal,
                        .expression = null,
                        .value_type = current_token.value_type,
                        .value = parsed_value,
                        .line_number = current_token.line_number,
                        .token_number = current_token.token_number,
                        .is_mutable = false,
                        .is_temporary = false,
                        .has_decl_prefix = false,
                    });
                    continue;
                },
                .TKN_INSPECT => {
                    try self.parsed_tokens.append(ParsedToken{
                        .token_type = .TKN_INSPECT,
                        .literal = current_token.literal,
                        .expression = null,
                        .value_type = .nothing,
                        .value = .{ .nothing = {} },
                        .line_number = current_token.line_number,
                        .token_number = current_token.token_number,
                        .is_mutable = false,
                        .is_temporary = false,
                        .has_decl_prefix = false,
                    });
                    continue;
                },
                .TKN_ARROW => {
                    continue;
                },
                .TKN_PLUS => {
                    try self.parsed_tokens.append(ParsedToken{
                        .token_type = .TKN_PLUS,
                        .literal = current_token.literal,
                        .expression = null,
                        .value_type = .nothing,
                        .value = .{ .nothing = {} },
                        .line_number = current_token.line_number,
                        .token_number = current_token.token_number,
                        .is_mutable = false,
                        .is_temporary = false,
                        .has_decl_prefix = false,
                    });
                    continue;
                },
                .TKN_MINUS => {
                    try self.parsed_tokens.append(ParsedToken{
                        .token_type = .TKN_MINUS,
                        .literal = current_token.literal,
                        .expression = null,
                        .value_type = .nothing,
                        .value = .{ .nothing = {} },
                        .line_number = current_token.line_number,
                        .token_number = current_token.token_number,
                        .is_mutable = false,
                        .is_temporary = false,
                        .has_decl_prefix = false,
                    });
                    continue;
                },
                .TKN_STAR => {
                    try self.parsed_tokens.append(ParsedToken{
                        .token_type = .TKN_STAR,
                        .literal = current_token.literal,
                        .expression = null,
                        .value_type = .nothing,
                        .value = .{ .nothing = {} },
                        .line_number = current_token.line_number,
                        .token_number = current_token.token_number,
                        .is_mutable = false,
                        .is_temporary = false,
                        .has_decl_prefix = false,
                    });
                    continue;
                },
                .TKN_SLASH => {
                    try self.parsed_tokens.append(ParsedToken{
                        .token_type = .TKN_SLASH,
                        .literal = current_token.literal,
                        .expression = null,
                        .value_type = .nothing,
                        .value = .{ .nothing = {} },
                        .line_number = current_token.line_number,
                        .token_number = current_token.token_number,
                        .is_mutable = false,
                        .is_temporary = false,
                        .has_decl_prefix = false,
                    });
                    continue;
                },
                .TKN_PERCENT => {
                    try self.parsed_tokens.append(ParsedToken{
                        .token_type = .TKN_PERCENT,
                        .literal = current_token.literal,
                        .expression = null,
                        .value_type = .nothing,
                        .value = .{ .nothing = {} },
                        .line_number = current_token.line_number,
                        .token_number = current_token.token_number,
                        .is_mutable = false,
                        .is_temporary = false,
                        .has_decl_prefix = false,
                    });
                    continue;
                },
                .TKN_POWER => {
                    try self.parsed_tokens.append(ParsedToken{
                        .token_type = .TKN_POWER,
                        .literal = current_token.literal,
                        .expression = null,
                        .value_type = .nothing,
                        .value = .{ .nothing = {} },
                        .line_number = current_token.line_number,
                        .token_number = current_token.token_number,
                        .is_mutable = false,
                        .is_temporary = false,
                        .has_decl_prefix = false,
                    });
                    continue;
                },
                else => {
                    // Unhandled tokens are ignored for now
                },
            }
        }
    }

    pub fn dumpParser(self: *Parser) void {
        for (self.parsed_tokens.items) |token| {
            const m = if (token.is_mutable) "mutable" else "immutable";
            const t = if (token.is_temporary) "temporary" else "permanent";
            printDebug("{s} ", .{token.literal});
            printDebug("({s}) ", .{@tagName(token.token_type)});
            if (token.token_type == .TKN_VALUE) {
                printDebug("({s}) ", .{@tagName(token.value_type)});
            }
            if (token.token_type == .TKN_IDENTIFIER) {
                printDebug("({s}) ", .{m});
                printDebug("({s}) ", .{t});
            }
            printDebug("\n", .{});
        }
    }
};

fn ifExpression(tokens: []Token) bool {
    for (tokens) |token| {
        if (isExpressionToken(token)) {
            return true;
        }
    }
    return false;
}

fn isExpressionToken(token: Token) bool {
    return token.token_type == .TKN_PLUS or
        token.token_type == .TKN_MINUS or
        token.token_type == .TKN_STAR or
        token.token_type == .TKN_SLASH or
        token.token_type == .TKN_PERCENT or
        token.token_type == .TKN_POWER or
        token.token_type == .TKN_LPAREN or
        token.token_type == .TKN_RPAREN;
}

fn grabLine(self: *Parser, current_token: Token) !std.ArrayList(Token) {
    var line_array = std.ArrayList(Token).init(self.allocator);
    var found_assign: bool = false;
    var last_token_type: ?TokenKind = null;

    for (self.tokens) |token| {
        if (token.line_number == current_token.line_number) {
            if (found_assign) {
                if (token.token_type == .TKN_NEWLINE) {
                    break;
                }

                // Add implicit multiplication for juxtaposition of value and parentheses
                if (last_token_type != null) {
                    const is_value = last_token_type.? == .TKN_VALUE;
                    const is_rparen = last_token_type.? == .TKN_RPAREN;
                    const is_lparen = token.token_type == .TKN_LPAREN;
                    const is_value_next = token.token_type == .TKN_VALUE;

                    if ((is_value and is_lparen) or (is_rparen and is_value_next)) {
                        const implicit_mul = Token{
                            .token_type = .TKN_STAR,
                            .literal = "*",
                            .line_number = token.line_number,
                            .token_number = token.token_number,
                            .value_type = .nothing,
                        };
                        line_array.append(implicit_mul) catch |err| {
                            printError("Error appending implicit multiplication: {s}\n", .{@errorName(err)});
                            return error.ErrorAppendingImplicitMultiplication;
                        };
                    }
                }

                line_array.append(token) catch |err| {
                    printError("Error appending to line_array: {s}\n", .{@errorName(err)});
                    return error.ErrorAppendingToLineArray;
                };
                last_token_type = token.token_type;
            }
            if (token.token_type == .TKN_VALUE_ASSIGN) {
                found_assign = true;
            }
        }
    }
    return line_array;
}

fn parseIntFromLiteral(literal: []const u8) i32 {
    return std.fmt.parseInt(i32, literal, 10) catch 0;
}

fn parseFloatFromLiteral(literal: []const u8) f64 {
    return std.fmt.parseFloat(f64, literal) catch 0;
}

fn parseStringFromLiteral(literal: []const u8) []const u8 {
    if (literal.len >= 2 and literal[0] == '"' and literal[literal.len - 1] == '"') {
        return literal[1 .. literal.len - 1];
    }
    return literal;
}

fn parseBoolFromLiteral(literal: []const u8) bool {
    return std.mem.eql(u8, literal, "true") or std.mem.eql(u8, literal, "TRUE");
}

fn parseValueFromLiteral(literal: []const u8, value_type: ValueType) Value {
    return switch (value_type) {
        .int => .{ .int = parseIntFromLiteral(literal) },
        .float => .{ .float = parseFloatFromLiteral(literal) },
        .string => .{ .string = parseStringFromLiteral(literal) },
        .bool => .{ .bool = parseBoolFromLiteral(literal) },
        .nothing => .{ .nothing = {} },
    };
}
