const std = @import("std");
const ParsedToken = @import("../frontend/parser.zig").ParsedToken;
const Preprocessor = @import("../preprocessor/prepro.zig").Preprocessor;
const TokenImport = @import("../token/token.zig");
const Value = TokenImport.Value;
const ValueType = TokenImport.ValueType;

pub fn writeFlatFileToWriter(writer: anytype, tokens: []ParsedToken) !void {
    var new_line_needed: bool = false;
    for (tokens) |token| {
        if (token.token_type == .TKN_NEWLINE) {
            if (new_line_needed == true) {
                try writer.writeAll("\n");
                new_line_needed = false;
            }
            continue;
        }
        new_line_needed = true;

        switch (token.token_type) {
            .TKN_GROUP => {
                try writer.print("{s}.", .{token.literal});
            },
            .TKN_TYPE => {
                try writer.print(":{s} ", .{token.literal});
            },
            .TKN_VALUE => {
                switch (token.value_type) {
                    .string => {
                        try writer.print("\"{s}\" ", .{token.value.string});
                    },
                    .env => {
                        try writer.print("\"{s}\" ", .{token.value.env});
                    },
                    .bool => {
                        try writer.print("{s} ", .{if (token.value.bool) "TRUE" else "FALSE"});
                    },
                    .int => {
                        try writer.print("{d} ", .{token.value.int});
                    },
                    .float => {
                        try writer.print("{d} ", .{token.value.float});
                    },
                    .time => {
                        try writer.print("{d} ", .{token.value.time});
                    },
                    .nothing => {
                        try writer.writeAll("(nothing) ");
                    },
                }
            },
            .TKN_VALUE_ASSIGN, .TKN_INSPECT, .TKN_IDENTIFIER, .TKN_PLUS, .TKN_LOOKUP, .TKN_EXPRESSION, .TKN_MINUS, .TKN_STAR, .TKN_SLASH, .TKN_PERCENT, .TKN_POWER, .TKN_LPAREN, .TKN_RPAREN, .TKN_COMMA, .TKN_LBRACKET, .TKN_RBRACKET, .TKN_IF, .TKN_THEN, .TKN_ELSE => {
                try writer.print("{s} ", .{token.literal});
            },
            else => try writer.print("UNUSED TOKEN ENCOUNTERED:  {s} \n", .{token.literal}),
        }
    }
}

pub fn writeFlatFile(tokens: []ParsedToken) !void {
    var file = try std.fs.cwd().createFile("build/output.f.para", .{});
    defer file.close();
    try writeFlatFileToWriter(file.deprecatedWriter(), tokens);
}

pub fn writeBakedFile(tokens: []ParsedToken, preprocessor: *Preprocessor, allocator: std.mem.Allocator) !void {
    var file = try std.fs.cwd().createFile("build/output.f.para", .{});
    defer file.close();

    var writer = file.deprecatedWriter();
    var new_line_needed: bool = false;

    for (tokens) |token| {
        if (token.token_type == .TKN_NEWLINE) {
            if (new_line_needed == true) {
                try writer.writeAll("\n");
                new_line_needed = false;
            }
            continue;
        }
        new_line_needed = true;
    }

    std.debug.print("Writing baked file\n", .{});

    var i: usize = 0;
    var current_line_scopes: std.ArrayList([]const u8) = .empty;
    defer current_line_scopes.deinit(allocator);

    while (i < tokens.len) : (i += 1) {
        const current_token = tokens[i];

        switch (current_token.token_type) {
            .TKN_GROUP => {
                try current_line_scopes.append(allocator, current_token.literal);
                try writer.print("{s}.", .{current_token.literal});
            },
            .TKN_IDENTIFIER => {
                try writer.print("{s} ", .{current_token.literal});
            },
            .TKN_TYPE => {
                try writer.print(":{s} ", .{current_token.literal});
            },
            .TKN_VALUE_ASSIGN => {
                try writer.print("= ", .{});

                if (i + 1 < tokens.len and (tokens[i + 1].token_type == .TKN_IDENTIFIER or
                    tokens[i + 1].token_type == .TKN_GROUP or
                    tokens[i + 1].token_type == .TKN_LOOKUP))
                {
                    const lookup_path = try preprocessor.buildLookupPathForward(tokens, i + 1);
                    defer allocator.free(lookup_path);

                    if (preprocessor.getLookupValue(lookup_path)) |var_value| {
                        switch (var_value.type) {
                            .int => try writer.print("{d}", .{var_value.value.int}),
                            .float => try writer.print("{d:.2}", .{var_value.value.float}),
                            .string => try writer.print("\"{s}\"", .{var_value.value.string}),
                            .bool => try writer.print("{s}", .{if (var_value.value.bool) "TRUE" else "FALSE"}),
                            .time => try writer.print("{d}", .{var_value.value.time}),
                            .nothing => try writer.print("UNDEFINED", .{}),
                        }

                        const skip_count: usize = lookup_path.len;
                        i += skip_count;
                    } else {
                        try writer.print("UNUSED TOKEN ENCOUNTERED: ", .{});
                        try writer.print(" {s}", .{tokens[i + 1].literal});
                        i += 1;
                    }
                }
            },
            .TKN_VALUE => {
                switch (current_token.value_type) {
                    .int => try writer.print("{d}", .{current_token.value.int}),
                    .float => try writer.print("{d:.2}", .{current_token.value.float}),
                    .string => try writer.print("\"{s}\"", .{current_token.value.string}),
                    .bool => try writer.print("{s}", .{if (current_token.value.bool) "TRUE" else "FALSE"}),
                    .nothing => try writer.print("UNDEFINED", .{}),
                }
            },
            .TKN_NEWLINE => {
                try writer.print("\n", .{});
                current_line_scopes.clearRetainingCapacity();
            },
            .TKN_INSPECT => {
                try writer.print("? ", .{});
            },
            .TKN_LOOKUP => {
                try writer.print("{s} ", .{current_token.literal});
            },
            else => {},
        }
    }
}
