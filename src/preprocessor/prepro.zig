const std = @import("std");
const ParsedToken = @import("../frontend/parser.zig").ParsedToken;
const TokenImport = @import("../token/token.zig");
const Token = TokenImport.Token;
const TokenKind = TokenImport.TokenKind;
const ValueType = TokenImport.ValueType;
const Value = TokenImport.Value;
const Reporting = @import("../utils/reporting.zig");

pub const Preprocessor = struct {
    pub const Variable = struct {
        name: []const u8,
        value: Value,
        type: ValueType,
        mutable: bool,
        temp: bool,
    };

    pub const Scope = struct {
        variables: std.StringHashMap(Variable),
        nested_scopes: std.StringHashMap(*Scope),
        allocator: std.mem.Allocator,

        pub fn init(allocator: std.mem.Allocator) Scope {
            return Scope{
                .variables = std.StringHashMap(Variable).init(allocator),
                .nested_scopes = std.StringHashMap(*Scope).init(allocator),
                .allocator = allocator,
            };
        }

        pub fn deinit(self: *Scope) void {
            var it = self.nested_scopes.iterator();
            while (it.next()) |entry| {
                entry.value_ptr.*.deinit();
                self.allocator.destroy(entry.value_ptr.*);
            }
            self.nested_scopes.deinit();
            self.variables.deinit();
        }
    };

    allocator: std.mem.Allocator,
    root_scope: Scope,

    pub fn init(allocator: std.mem.Allocator) Preprocessor {
        return Preprocessor{
            .allocator = allocator,
            .root_scope = Scope.init(allocator),
        };
    }

    pub fn deinit(self: *Preprocessor) void {
        self.root_scope.deinit();
    }

    // Build lookup path for forward traversal (looking ahead from current position)
    pub fn buildLookupPathForward(self: *Preprocessor, tokens: []ParsedToken, start_index: usize) ![][]const u8 {
        var path = std.ArrayList([]const u8).init(self.allocator);
        defer path.deinit();

        var i = start_index;

        // Add the first token (identifier, group, or lookup)
        if (i < tokens.len and (tokens[i].token_type == .TKN_IDENTIFIER or
            tokens[i].token_type == .TKN_GROUP or
            tokens[i].token_type == .TKN_LOOKUP))
        {
            try path.append(tokens[i].literal);
            i += 1;
        } else {
            return error.InvalidLookupPath;
        }

        // Continue collecting tokens as long as there are more in the path
        while (i < tokens.len) {
            // Skip arrow tokens
            if (tokens[i].token_type == .TKN_ARROW) {
                i += 1;
                continue;
            }

            if (tokens[i].token_type == .TKN_GROUP or
                tokens[i].token_type == .TKN_IDENTIFIER or
                tokens[i].token_type == .TKN_LOOKUP)
            {
                try path.append(tokens[i].literal);
                i += 1;
            } else {
                break; // Stop at the first non-path token
            }
        }

        // Create a slice that will be owned by the caller
        const result = try self.allocator.alloc([]const u8, path.items.len);
        for (path.items, 0..) |item, idx| {
            result[idx] = item;
        }

        return result;
    }

    // Build an array of lookup parts for backward traversal (for inspections or variable lookups)
    pub fn buildLookupPathBackward(self: *Preprocessor, tokens: []ParsedToken, index: usize) ![][]const u8 {
        var path = std.ArrayList([]const u8).init(self.allocator);
        defer path.deinit();

        // Improved approach to capture complete paths
        // Start at the token right before the inspect symbol
        if (index == 0 or index >= tokens.len) {
            return &[_][]const u8{};
        }

        var current_index: isize = @intCast(index - 1);

        // First get the variable name (identifier) that is being inspected
        if (tokens[@intCast(current_index)].token_type == .TKN_IDENTIFIER or
            tokens[@intCast(current_index)].token_type == .TKN_LOOKUP)
        {
            try path.append(tokens[@intCast(current_index)].literal);
            current_index -= 1;
        } else {
            // If not an identifier or lookup, just return an empty path
            return &[_][]const u8{};
        }

        // Now walk backward looking for groups and arrows
        var in_path = false;
        while (current_index >= 0) {
            const token = tokens[@intCast(current_index)];

            if (token.token_type == .TKN_ARROW) {
                in_path = true;
                current_index -= 1;
                continue;
            }

            if (in_path and (token.token_type == .TKN_GROUP or
                token.token_type == .TKN_IDENTIFIER or
                token.token_type == .TKN_LOOKUP))
            {
                try path.insert(0, token.literal);
                in_path = false;
                current_index -= 1;
                continue;
            }

            // If we see a newline or any non-path token, stop searching
            if (token.token_type == .TKN_NEWLINE or
                token.token_type == .TKN_VALUE_ASSIGN or
                token.token_type == .TKN_TYPE_ASSIGN)
            {
                break;
            }

            current_index -= 1;
        }

        // Create a slice that will be owned by the caller
        const result = try self.allocator.alloc([]const u8, path.items.len);
        for (path.items, 0..) |item, y| {
            result[y] = item;
        }
        return result;
    }

    pub fn buildAssignmentArray(self: *Preprocessor, tokens: []ParsedToken, index: usize) ![]Variable {
        var result = std.ArrayList(Variable).init(self.allocator);
        defer result.deinit();

        // First, collect any groups (scopes) that come before the identifier
        var i: isize = @intCast(index - 1);
        var groups = std.ArrayList(Variable).init(self.allocator);
        defer groups.deinit();

        // Collect groups in reverse order (right to left)
        while (i >= 0) : (i -= 1) {
            if (tokens[@intCast(i)].token_type == .TKN_GROUP) {
                try groups.append(Variable{
                    .name = tokens[@intCast(i)].literal,
                    .value = Value{ .nothing = {} },
                    .type = .nothing,
                    .mutable = false,
                    .temp = false,
                });
            } else if (tokens[@intCast(i)].token_type == .TKN_NEWLINE or
                tokens[@intCast(i)].token_type == .TKN_EOF)
            {
                break; // Stop at the beginning of the line
            }
        }

        // Add groups to result in correct order (left to right)
        if (groups.items.len > 0) {
            var group_idx: usize = 0;
            while (group_idx < groups.items.len) : (group_idx += 1) {
                try result.append(groups.items[groups.items.len - 1 - group_idx]);
            }
        }

        // Look for the lookup after the last group
        var lookup_found = false;
        if (index > 0) {
            const id_pos: isize = @intCast(index - 1);

            if (tokens[@intCast(id_pos)].token_type == .TKN_IDENTIFIER) {
                try result.append(Variable{
                    .name = tokens[@intCast(id_pos)].literal,
                    .value = Value{ .nothing = {} },
                    .type = .nothing,
                    .mutable = tokens[@intCast(id_pos)].is_mutable,
                    .temp = tokens[@intCast(id_pos)].is_temporary,
                });
                lookup_found = true;
            } else if (id_pos > 0 and tokens[@intCast(id_pos)].token_type == .TKN_TYPE and
                tokens[@intCast(id_pos - 1)].token_type == .TKN_IDENTIFIER)
            {
                try result.append(Variable{
                    .name = tokens[@intCast(id_pos - 1)].literal,
                    .value = Value{ .nothing = {} },
                    .type = .nothing,
                    .mutable = tokens[@intCast(id_pos - 1)].is_mutable,
                    .temp = tokens[@intCast(id_pos - 1)].is_temporary,
                });
                lookup_found = true;
            }
        }

        if (!lookup_found) {
            Reporting.throwError("Error: No lookup found before assignment at token {d}\n", .{index});
            return error.InvalidAssignment;
        }

        var value_found = false;

        if (index + 1 < tokens.len and tokens[index + 1].token_type == .TKN_EXPRESSION) {
            try result.append(Variable{
                .name = "value", // Placeholder name
                .value = Value{ .int = 12 }, // Default for expressions for now
                .type = .int,
                .mutable = false,
                .temp = false,
            });
            value_found = true;
        } else if (index + 1 < tokens.len and tokens[index + 1].token_type == .TKN_VALUE) {
            // Get the mutability from the identifier (last item in result)
            const identifier = result.items[result.items.len - 1];
            try result.append(Variable{
                .name = "value", // Placeholder
                .value = tokens[index + 1].value,
                .type = tokens[index + 1].value_type,
                .mutable = identifier.mutable, // Use the mutability from the identifier
                .temp = tokens[index + 1].is_temporary,
            });
            value_found = true;
        } else if (index + 1 < tokens.len and (tokens[index + 1].token_type == .TKN_IDENTIFIER or
            tokens[index + 1].token_type == .TKN_GROUP or
            tokens[index + 1].token_type == .TKN_LOOKUP))
        {
            const original_lookup_path = try self.buildLookupPathForward(tokens, index + 1);
            defer self.allocator.free(original_lookup_path);

            var resolved_rhs_variable: ?Variable = null;
            const direct_lookup_result = self.getLookupValue(original_lookup_path);

            if (direct_lookup_result) |var_value| {
                resolved_rhs_variable = var_value;
            } else |err| {
                // Get the LHS groups by excluding the last item (which is the identifier)
                const lhs_groups = if (result.items.len > 0) result.items[0 .. result.items.len - 1] else &[_]Variable{};

                // Debug print the LHS groups
                var lhs_str = std.ArrayList(u8).init(self.allocator);
                defer lhs_str.deinit();
                for (lhs_groups, 0..) |g, j| {
                    if (j > 0) try lhs_str.appendSlice("->");
                    try lhs_str.appendSlice(g.name);
                }

                // Check if the RHS path starts with our LHS groups
                var lhs_prefix_matches_rhs_path = true;
                if (original_lookup_path.len == 0 or lhs_groups.len == 0) {
                    lhs_prefix_matches_rhs_path = false;
                } else {
                    for (lhs_groups, 0..) |lhs_group, k| {
                        if (k >= original_lookup_path.len or !std.mem.eql(u8, lhs_group.name, original_lookup_path[k])) {
                            lhs_prefix_matches_rhs_path = false;
                            break;
                        }
                    }
                }

                // If we have a ScopeNotFound and the RHS path starts with our LHS groups,
                // try to resolve the remainder from the root scope
                if (err == error.ScopeNotFound and lhs_prefix_matches_rhs_path) {
                    const tail_path = original_lookup_path[lhs_groups.len..];

                    if (tail_path.len > 0) {
                        const fallback_lookup_result = self.getLookupValue(tail_path);
                        if (fallback_lookup_result) |fb_var_value| {
                            resolved_rhs_variable = fb_var_value;
                        } else |fb_err| {
                            Reporting.throwError("Fallback lookup failed: {any}\n", .{fb_err});
                        }
                    }
                }

                if (resolved_rhs_variable == null) {
                    Reporting.throwError("Value not found in lookup path\n", .{});
                    return error.ValueNotFoundInLookupPath;
                }
            }

            if (resolved_rhs_variable) |var_struct| {
                try result.append(var_struct);
                value_found = true;
            } else {
                Reporting.throwError("Internal error: RHS variable was not resolved\n", .{});
                return error.InvalidAssignment;
            }
        }

        if (!value_found) {
            Reporting.throwError("Warning: No value found after assignment, using default\n", .{});
            return error.NoValueFoundAfterAssignment;
        }

        const array = try self.allocator.alloc(Variable, result.items.len);
        for (result.items, 0..) |item, y| {
            array[y] = item;
        }
        return array;
    }

    // Retrieves a value from the appropriate scope
    pub fn getLookupValue(self: *Preprocessor, path: [][]const u8) !?Variable {
        if (path.len < 1) return error.InvalidLookupPath;

        // Last element is the variable name
        const var_name = path[path.len - 1];

        // Navigate to the correct scope
        var current_scope = &self.root_scope;

        // Create all intermediate scopes if they don't exist
        for (path[0 .. path.len - 1]) |scope_name| {
            if (!current_scope.nested_scopes.contains(scope_name)) {
                const new_scope = try self.allocator.create(Scope);
                new_scope.* = Scope.init(self.allocator);
                try current_scope.nested_scopes.put(scope_name, new_scope);
            }
            current_scope = current_scope.nested_scopes.get(scope_name).?;
        }

        // Look up the variable
        const result = current_scope.variables.get(var_name);
        if (result == null) {
            // Build a path string for better error reporting
            var path_str = std.ArrayList(u8).init(self.allocator);
            defer path_str.deinit();
            for (path[0 .. path.len - 1], 0..) |part, i| {
                if (i > 0) try path_str.appendSlice(" -> ");
                try path_str.appendSlice(part);
            }
            return error.VariableNotFoundInScope;
        }
        return result;
    }

    // Helper function to get the target scope
    fn getTargetScope(self: *Preprocessor, groups: []Variable) !*Scope {
        var current_scope = &self.root_scope;
        for (groups) |group| {
            if (!current_scope.nested_scopes.contains(group.name)) {
                const new_scope = try self.allocator.create(Scope);
                new_scope.* = Scope.init(self.allocator);
                try current_scope.nested_scopes.put(group.name, new_scope);
            }
            current_scope = current_scope.nested_scopes.get(group.name).?;
        }
        return current_scope;
    }

    // Recursively search for a variable by name in the given scope and its nested scopes
    fn findVariableByName(self: *Preprocessor, scope: *Scope, name: []const u8) ?Variable {
        if (scope.variables.get(name)) |var_ptr| {
            return var_ptr;
        }
        var it = scope.nested_scopes.iterator();
        while (it.next()) |entry| {
            if (self.findVariableByName(entry.value_ptr.*, name)) |found| {
                return found;
            }
        }
        return null;
    }

    // Assigns a value using the assignment array
    pub fn assignValue(self: *Preprocessor, assignment_array: []Variable) !void {
        if (assignment_array.len < 2) return error.InvalidAssignment;

        // Last item is the value
        const value_item = assignment_array[assignment_array.len - 1];
        // Second to last is the identifier
        const identifier = assignment_array[assignment_array.len - 2];
        // Everything before the identifier is groups
        const groups = assignment_array[0 .. assignment_array.len - 2];

        // Get the target scope
        const target_scope = try self.getTargetScope(groups);

        // Step 1: Determine if we have an explicit type declaration
        const has_explicit_type = identifier.type != .nothing;
        const declared_type = if (has_explicit_type) identifier.type else value_item.type;

        // Step 2: Check if variable already exists
        if (target_scope.variables.get(identifier.name)) |existing_var| {
            // Check mutability
            if (!existing_var.mutable) {
                Reporting.throwError("Error: Cannot reassign immutable variable '{s}'\n", .{identifier.name});
                return error.ImmutableVariable;
            }

            // Check type compatibility with existing variable
            if (!isTypeCompatible(existing_var.type, value_item.type)) {
                Reporting.throwError("Error: Cannot assign {s} value to variable '{s}' of type {s}\n", .{ value_item.type.toString(), identifier.name, existing_var.type.toString() });
                return error.TypeMismatch;
            }
        } else {
            // Step 3: For new variables, handle type checking/inference
            if (has_explicit_type) {
                // Check if value matches declared type
                if (!isTypeCompatible(declared_type, value_item.type)) {
                    Reporting.throwError("Error: Cannot initialize {s} variable '{s}' with {s} value\n", .{ declared_type.toString(), identifier.name, value_item.type.toString() });
                    return error.TypeMismatch;
                }
            }
            // If no explicit type, we'll infer from the value (handled in variable creation)
        }

        // Create/update the variable with either declared or inferred type
        const variable = Variable{
            .name = identifier.name,
            .value = value_item.value,
            .type = declared_type,
            .mutable = identifier.mutable,
            .temp = identifier.temp,
        };

        try target_scope.variables.put(identifier.name, variable);
    }

    fn evaluateExpression(self: *Preprocessor, tokens: []Token) !Value {
        if (tokens.len == 0) {
            Reporting.throwError("Warning: Empty expression\n", .{});
            return Value{ .int = 0 };
        }

        // Use the shunting yard algorithm to convert infix to postfix
        var stack = std.ArrayList(Token).init(self.allocator);
        defer stack.deinit();

        var output = std.ArrayList(Token).init(self.allocator);
        defer output.deinit();

        const getPrecedence = struct {
            fn get(token_type: TokenKind) u8 {
                return switch (token_type) {
                    .TKN_POWER => 3,
                    .TKN_STAR, .TKN_SLASH, .TKN_PERCENT => 2,
                    .TKN_PLUS, .TKN_MINUS => 1,
                    else => 0,
                };
            }
        }.get;

        // First pass: Convert to postfix notation
        for (tokens) |token| {
            if (token.token_type == .TKN_VALUE) {
                output.append(token) catch unreachable;
            } else if (token.token_type == .TKN_LOOKUP) {
                // Handle variables by looking them up
                output.append(token) catch unreachable;
            } else if (token.token_type == .TKN_PLUS or token.token_type == .TKN_MINUS or
                token.token_type == .TKN_STAR or token.token_type == .TKN_SLASH or
                token.token_type == .TKN_PERCENT or token.token_type == .TKN_POWER)
            {
                while (stack.items.len > 0 and
                    getPrecedence(stack.items[stack.items.len - 1].token_type) >= getPrecedence(token.token_type))
                {
                    if (stack.items.len > 0) {
                        const last_op = stack.items[stack.items.len - 1];
                        output.append(last_op) catch unreachable;
                        _ = stack.orderedRemove(stack.items.len - 1);
                    }
                }
                stack.append(token) catch unreachable;
            } else if (token.token_type == .TKN_LPAREN) {
                stack.append(token) catch unreachable;
            } else if (token.token_type == .TKN_RPAREN) {
                while (stack.items.len > 0 and stack.items[stack.items.len - 1].token_type != .TKN_LPAREN) {
                    if (stack.items.len > 0) {
                        const last_op = stack.items[stack.items.len - 1];
                        output.append(last_op) catch unreachable;
                        _ = stack.orderedRemove(stack.items.len - 1);
                    }
                }
                if (stack.items.len > 0 and stack.items[stack.items.len - 1].token_type == .TKN_LPAREN) {
                    _ = stack.orderedRemove(stack.items.len - 1);
                }
            }
        }

        while (stack.items.len > 0) {
            const last_op = stack.items[stack.items.len - 1];
            output.append(last_op) catch unreachable;
            _ = stack.orderedRemove(stack.items.len - 1);
        }

        // Now evaluate the postfix expression
        var result_stack = std.ArrayList(Value).init(self.allocator);
        defer result_stack.deinit();

        for (output.items) |token| {
            switch (token.token_type) {
                .TKN_VALUE => {
                    // Push the value onto the stack
                    const value: Value = switch (token.value_type) {
                        .int => Value{ .int = std.fmt.parseInt(i32, token.literal, 10) catch 0 },
                        .float => Value{ .float = std.fmt.parseFloat(f64, token.literal) catch 0 },
                        .string => Value{ .string = token.literal },
                        .bool => Value{ .bool = std.mem.eql(u8, token.literal, "true") },
                        .nothing => Value{ .nothing = {} },
                    };
                    result_stack.append(value) catch unreachable;
                },
                .TKN_LOOKUP => {
                    // Look up the variable value and push onto the stack
                    var found = false;
                    if (self.findVariableByName(&self.root_scope, token.literal)) |resolved| {
                        result_stack.append(resolved.value) catch unreachable;
                        found = true;
                    }

                    if (!found) {
                        Reporting.throwError("Warning: Variable '{s}' not found in expression, using 0\n", .{token.literal});
                        return error.VariableNotFoundInExpression;
                    }
                },
                .TKN_PLUS, .TKN_MINUS, .TKN_STAR, .TKN_SLASH, .TKN_PERCENT, .TKN_POWER => {
                    if (result_stack.items.len < 2) {
                        Reporting.throwError("Error: Not enough operands for operator {s}\n", .{token.literal});
                        return error.NotEnoughOperands;
                    }

                    const b_index = result_stack.items.len - 1;
                    const a_index = result_stack.items.len - 2;

                    const b = result_stack.items[b_index];
                    const a = result_stack.items[a_index];

                    // Determine if we need to use float arithmetic
                    var use_float = false;
                    var a_float: f64 = 0;
                    var b_float: f64 = 0;
                    var a_int: i32 = 0;
                    var b_int: i32 = 0;

                    // Extract values and determine operation type
                    switch (a) {
                        .int => |val| {
                            a_int = val;
                            a_float = @floatFromInt(val);
                        },
                        .float => |val| {
                            use_float = true;
                            a_float = val;
                        },
                        .string, .bool, .nothing => {
                            Reporting.throwError("Warning: Non-numeric value in expression, using 0\n", .{});
                            return error.NonNumericValue;
                        },
                    }

                    switch (b) {
                        .int => |val| {
                            b_int = val;
                            b_float = @floatFromInt(val);
                        },
                        .float => |val| {
                            use_float = true;
                            b_float = val;
                        },
                        .string, .bool, .nothing => {
                            Reporting.throwError("Warning: Non-numeric value in expression, using 0\n", .{});
                            return error.NonNumericValue;
                        },
                    }

                    // Remove the operands
                    _ = result_stack.orderedRemove(b_index);
                    _ = result_stack.orderedRemove(a_index);

                    // Perform the operation
                    if (use_float) {
                        const float_result: f64 = switch (token.token_type) {
                            .TKN_PLUS => a_float + b_float,
                            .TKN_MINUS => a_float - b_float,
                            .TKN_STAR => a_float * b_float,
                            .TKN_SLASH => {
                                if (b_float == 0) {
                                    Reporting.throwError("Error: Division by zero\n", .{});
                                    return error.DivisionByZero;
                                }
                                return Value{ .float = a_float / b_float };
                            },
                            .TKN_PERCENT => {
                                if (b_float == 0) {
                                    Reporting.throwError("Error: Modulo by zero\n", .{});
                                    return error.ModuloByZero;
                                }
                                return Value{ .float = @mod(a_float, b_float) };
                            },
                            .TKN_POWER => std.math.pow(f64, a_float, b_float),
                            else => unreachable,
                        };
                        try result_stack.append(Value{ .float = float_result });
                    } else {
                        const int_result: i32 = switch (token.token_type) {
                            .TKN_PLUS => a_int + b_int,
                            .TKN_MINUS => a_int - b_int,
                            .TKN_STAR => a_int * b_int,
                            .TKN_SLASH => {
                                if (b_int == 0) {
                                    Reporting.throwError("Error: Division by zero\n", .{});
                                    return error.DivisionByZero;
                                }
                                return Value{ .int = @divTrunc(a_int, b_int) };
                            },
                            .TKN_PERCENT => {
                                if (b_int == 0) {
                                    Reporting.throwError("Error: Modulo by zero\n", .{});
                                    return error.ModuloByZero;
                                }
                                return Value{ .int = @mod(a_int, b_int) };
                            },
                            .TKN_POWER => blk: {
                                var result: i32 = 1;
                                var exp = b_int;
                                while (exp > 0) : (exp -= 1) {
                                    result *= a_int;
                                }
                                break :blk result;
                            },
                            else => unreachable,
                        };
                        try result_stack.append(Value{ .int = int_result });
                    }
                },
                else => {},
            }
        }

        // Return the final result
        if (result_stack.items.len > 0) {
            return result_stack.items[result_stack.items.len - 1];
        } else {
            Reporting.throwError("Error: Empty expression result\n", .{});
            return Value{ .int = 0 };
        }
    }

    // Main interpret function
    pub fn interpret(self: *Preprocessor, tokens: []ParsedToken) !void {
        var i: usize = 0;

        while (i < tokens.len) : (i += 1) {
            const current_token = tokens[i];

            switch (current_token.token_type) {
                .TKN_VALUE_ASSIGN => {
                    const assignment = try self.buildAssignmentArray(tokens, i);
                    defer self.allocator.free(assignment);

                    // Check if the next token is an expression
                    if (i + 1 < tokens.len and tokens[i + 1].token_type == .TKN_EXPRESSION) {
                        // Skip the expression token in the next iteration since we're handling it now
                        defer i += 1;

                        if (tokens[i + 1].expression) |expression| {
                            // Safely evaluate the expression
                            const result = try self.evaluateExpression(expression);

                            // Update the assignment with the expression result
                            if (assignment.len >= 2) {
                                var modified_assignment = try self.allocator.dupe(Variable, assignment);
                                defer self.allocator.free(modified_assignment);

                                // Replace the value part with our calculated result
                                modified_assignment[modified_assignment.len - 1].value = result;
                                modified_assignment[modified_assignment.len - 1].type = switch (result) {
                                    .int => .int,
                                    .float => .float,
                                    .string => .string,
                                    .bool => .bool,
                                    .nothing => .nothing,
                                };

                                try self.assignValue(modified_assignment);
                            } else {
                                try self.assignValue(assignment);
                            }
                        } else {
                            try self.assignValue(assignment);
                        }
                    } else {
                        try self.assignValue(assignment);
                    }
                },
                .TKN_EXPRESSION => {
                    // Expression tokens are handled alongside VALUE_ASSIGN tokens
                    continue;
                },
                .TKN_INSPECT => {
                    if (i > 0) {
                        // Handle direct values
                        if (i > 0 and tokens[i - 1].token_type == .TKN_VALUE) {
                            const value_type_str = tokens[i - 1].value_type.toString();
                            Reporting.log("[{d}:{d}] value  :{s} = ", .{
                                current_token.line_number,
                                current_token.token_number,
                                value_type_str,
                            });

                            // Print the value based on its type
                            switch (tokens[i - 1].value_type) {
                                .int => Reporting.log("{d}\n", .{tokens[i - 1].value.int}),
                                .float => Reporting.log("{any}\n", .{tokens[i - 1].value.float}),
                                .string => Reporting.log("\"{s}\"\n", .{tokens[i - 1].value.string}),
                                .bool => Reporting.log("{s}\n", .{if (tokens[i - 1].value.bool) "TRUE" else "FALSE"}),
                                .nothing => Reporting.log("(nothing)\n", .{}),
                            }
                            continue;
                        }

                        // Handle variable lookups
                        const path = try self.buildLookupPathForInspection(tokens, i);
                        defer self.allocator.free(path);

                        // Build a formatted path string for display
                        var path_str: []u8 = undefined;
                        if (path.len > 0) {
                            var buffer = std.ArrayList(u8).init(self.allocator);
                            defer buffer.deinit();

                            for (path, 0..) |part, idx| {
                                if (idx > 0) {
                                    try buffer.appendSlice("-> ");
                                }
                                try buffer.appendSlice(part);
                            }

                            path_str = try self.allocator.dupe(u8, buffer.items);
                        } else {
                            path_str = try self.allocator.dupe(u8, "undefined");
                        }

                        defer self.allocator.free(path_str);

                        if (try self.getLookupValue(path)) |var_value| {
                            Reporting.log("[{d}:{d}] {s} :{s} = ", .{
                                current_token.line_number,
                                current_token.token_number,
                                path_str,
                                var_value.type.toString(),
                            });

                            switch (var_value.type) {
                                .int => Reporting.log("{d}\n", .{var_value.value.int}),
                                .float => Reporting.log("{any}\n", .{var_value.value.float}),
                                .string => Reporting.log("\"{s}\"\n", .{var_value.value.string}),
                                .bool => Reporting.log("{s}\n", .{if (var_value.value.bool) "TRUE" else "FALSE"}),
                                .nothing => Reporting.log("(nothing)\n", .{}),
                            }
                        } else {
                            unreachable;
                        }
                    }
                },
                else => {},
            }
        }
    }

    // Process tokens and execute the script
    pub fn process(self: *Preprocessor, tokens: []ParsedToken) !void {
        try self.interpret(tokens);
    }

    // Dump all variables to a writer
    pub fn dumpVariables(self: *Preprocessor, allocator: std.mem.Allocator) !void {
        Reporting.log("\n+====================================+\n", .{});
        Reporting.log("|          VARIABLE INSPECTOR          |\n", .{});
        Reporting.log("+====================================+\n\n", .{});

        // Root scope variables
        Reporting.log("ROOT SCOPE:\n", .{});
        var root_it = self.root_scope.variables.iterator();
        while (root_it.next()) |entry| {
            const var_value = entry.value_ptr.*;
            try dumpVariable("", entry.key_ptr.*, var_value);
        }

        // Nested scopes
        var scope_it = self.root_scope.nested_scopes.iterator();
        while (scope_it.next()) |scope_entry| {
            try dumpScope(scope_entry.key_ptr.*, scope_entry.value_ptr.*, "", allocator);
        }

        Reporting.log("\n+====================================+\n", .{});
        Reporting.log("|          END OF INSPECTION          |\n", .{});
        Reporting.log("+====================================+\n\n", .{});
    }

    fn dumpScope(scope_name: []const u8, scope: *Scope, prefix: []const u8, allocator: std.mem.Allocator) !void {
        const new_prefix = try std.fmt.allocPrint(allocator, "{s}{s}->", .{ prefix, scope_name });
        defer allocator.free(new_prefix);

        Reporting.log("\n.----------------------------------------.\n", .{});
        Reporting.log("| SCOPE: {s}\n", .{new_prefix});
        Reporting.log("|----------------------------------------|\n", .{});

        // Variables in this scope
        var var_it = scope.variables.iterator();
        while (var_it.next()) |entry| {
            const var_value = entry.value_ptr.*;
            try dumpVariable(new_prefix, entry.key_ptr.*, var_value);
        }

        // Nested scopes
        var nested_it = scope.nested_scopes.iterator();
        while (nested_it.next()) |nested_entry| {
            try dumpScope(nested_entry.key_ptr.*, nested_entry.value_ptr.*, new_prefix, allocator);
        }
    }

    fn dumpVariable(prefix: []const u8, name: []const u8, var_value: Variable) !void {
        const full_name = if (prefix.len > 0)
            try std.fmt.allocPrint(std.heap.page_allocator, "{s}{s}", .{ prefix, name })
        else
            name;
        defer if (prefix.len > 0) std.heap.page_allocator.free(full_name);

        Reporting.log("|----------------------------------------|\n", .{});
        Reporting.log("| {s}\n", .{full_name});
        Reporting.log("| Type: {s}\n", .{var_value.type.toString()});

        Reporting.log("| Value: ", .{});
        switch (var_value.type) {
            .int => Reporting.log("{d}", .{var_value.value.int}),
            .float => Reporting.log("{d:.2}", .{var_value.value.float}),
            .string => Reporting.log("\"{s}\"", .{var_value.value.string}),
            .bool => Reporting.log("{s}", .{if (var_value.value.bool) "true" else "false"}),
            .nothing => Reporting.log("(nothing)", .{}),
        }
        Reporting.log("\n", .{});

        Reporting.log("| Status: {s}{s}\n", .{
            if (var_value.mutable) "mutable" else "immutable",
            if (var_value.temp) " (temporary)" else "",
        });
        Reporting.log("`----------------------------------------'\n", .{});
    }

    // Handle inspection tokens (?) more accurately -
    // we need to fix the path order for inspection tokens
    pub fn buildLookupPathForInspection(self: *Preprocessor, tokens: []ParsedToken, index: usize) ![][]const u8 {
        var path = std.ArrayList([]const u8).init(self.allocator);
        defer path.deinit();

        // If the token is not an inspection token, return empty path
        if (index >= tokens.len or tokens[index].token_type != .TKN_INSPECT) {
            return &[_][]const u8{};
        }

        // We need to look backwards starting from the inspect token to find all parts
        // of the path. The tokens before the inspect should include:
        // 1. The variable name (identifier)
        // 2. Possibly group tokens representing nested scopes

        // Start with the token right before the inspect symbol
        if (index == 0) {
            return &[_][]const u8{};
        }

        // Working backward from the inspect token to construct the path
        var current_pos: isize = @intCast(index - 1);
        const start_line_pos = findLineStart(tokens, index);

        // First collect all identifiers and groups
        var parts = std.ArrayList([]const u8).init(self.allocator);
        defer parts.deinit();

        while (current_pos >= start_line_pos) : (current_pos -= 1) {
            const token = tokens[@intCast(current_pos)];

            if (token.token_type == .TKN_IDENTIFIER or
                token.token_type == .TKN_GROUP or
                token.token_type == .TKN_LOOKUP)
            {
                try parts.append(token.literal);
            } else if (token.token_type == .TKN_NEWLINE) {
                break; // Found the beginning of the line
            }
        }

        // Now reverse the parts to get the correct path order
        if (parts.items.len > 0) {
            var left: usize = 0;
            var right: usize = parts.items.len - 1;
            while (left < right) {
                const temp = parts.items[left];
                parts.items[left] = parts.items[right];
                parts.items[right] = temp;
                left += 1;
                right -= 1;
            }

            for (parts.items) |part| {
                try path.append(part);
            }
        }

        // Create a slice that will be owned by the caller
        const result = try self.allocator.alloc([]const u8, path.items.len);
        for (path.items, 0..) |item, y| {
            result[y] = item;
        }
        return result;
    }

    // Helper function to find the start of the current line
    fn findLineStart(tokens: []ParsedToken, index: usize) isize {
        var pos: isize = @intCast(index);
        while (pos >= 0) {
            if (tokens[@intCast(pos)].token_type == .TKN_NEWLINE) {
                return pos + 1;
            }
            pos -= 1;
        }
        return 0; // Start of file
    }
};

// Helper function to check type compatibility
fn isTypeCompatible(expected: ValueType, actual: ValueType) bool {
    return switch (expected) {
        .int => actual == .int,
        .float => actual == .float,
        .string => actual == .string,
        .bool => actual == .bool,
        .nothing => true, // nothing type can be assigned to anything
    };
}
