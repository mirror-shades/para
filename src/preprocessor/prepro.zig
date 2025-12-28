const std = @import("std");
const ParsedToken = @import("../frontend/parser.zig").ParsedToken;
const TokenImport = @import("../token/token.zig");
const Token = TokenImport.Token;
const TokenKind = TokenImport.TokenKind;
const ValueType = TokenImport.ValueType;
const Value = TokenImport.Value;
const Reporting = @import("../utils/reporting.zig");
const ir = @import("../ir.zig");

pub const Preprocessor = struct {
    pub const Variable = struct {
        name: []const u8,
        value: Value,
        type: ValueType,
        mutable: bool,
        temp: bool,
        has_decl_prefix: bool,
        /// Source location for diagnostics
        line_number: usize,
        token_number: usize,
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
    // Borrowed view of source lines for diagnostics (from the lexer).
    source_lines: [][]const u8,

    pub fn init(allocator: std.mem.Allocator) Preprocessor {
        return Preprocessor{
            .allocator = allocator,
            .root_scope = Scope.init(allocator),
            .source_lines = &[_][]const u8{},
        };
    }

    pub fn setSourceLines(self: *Preprocessor, lines: [][]const u8) void {
        self.source_lines = lines;
    }

    pub fn deinit(self: *Preprocessor) void {
        self.root_scope.deinit();
    }

    pub fn buildIrProgram(self: *Preprocessor) ir.Program {
        var program = ir.Program.init(self.allocator);

        // Emit all variables in the root scope as top-level bindings.
        var var_it = self.root_scope.variables.iterator();
        while (var_it.next()) |entry| {
            const variable = entry.value_ptr.*;
            if (variable.type == .nothing or variable.temp) continue;

            const ir_value = valueToIrValue(self.allocator, variable) catch continue;
            program.globals.append(self.allocator, .{
                .name = entry.key_ptr.*,
                .value = ir_value,
            }) catch unreachable;
        }

        // Emit each nested scope as an object at the top level.
        var scope_it = self.root_scope.nested_scopes.iterator();
        while (scope_it.next()) |scope_entry| {
            const scope_name = scope_entry.key_ptr.*;
            const scope_ptr = scope_entry.value_ptr.*;

            const obj_ptr = scopeToObject(self.allocator, scope_ptr) catch continue;
            program.globals.append(self.allocator, .{
                .name = scope_name,
                .value = ir.Value{ .object = obj_ptr },
            }) catch unreachable;
        }

        return program;
    }

    fn valueToIrValue(allocator: std.mem.Allocator, variable: Variable) !ir.Value {
        return switch (variable.type) {
            .int => ir.Value{ .int = variable.value.int },
            .float => ir.Value{ .float = variable.value.float },
            .string => ir.Value{ .string = try allocator.dupe(u8, variable.value.string) },
            .bool => ir.Value{ .bool = variable.value.bool },
            .time => ir.Value{ .time = variable.value.time },
            .nothing => ir.Value{ .null_ = {} },
        };
    }

    fn scopeToObject(allocator: std.mem.Allocator, scope: *Scope) !*ir.Object {
        const obj_ptr = try allocator.create(ir.Object);
        obj_ptr.* = ir.Object.init(allocator);

        // Add variables as fields.
        var var_it = scope.variables.iterator();
        while (var_it.next()) |entry| {
            const variable = entry.value_ptr.*;
            if (variable.type == .nothing or variable.temp) continue;

            const ir_value = valueToIrValue(allocator, variable) catch continue;
            obj_ptr.fields.append(allocator, .{
                .name = entry.key_ptr.*,
                .value = ir_value,
            }) catch unreachable;
        }

        // Add nested scopes as nested objects.
        var nested_it = scope.nested_scopes.iterator();
        while (nested_it.next()) |nested_entry| {
            const child_name = nested_entry.key_ptr.*;
            const child_scope = nested_entry.value_ptr.*;

            const child_obj = scopeToObject(allocator, child_scope) catch continue;
            obj_ptr.fields.append(allocator, .{
                .name = child_name,
                .value = ir.Value{ .object = child_obj },
            }) catch unreachable;
        }

        return obj_ptr;
    }

    // Build lookup path for forward traversal (looking ahead from current position)
    pub fn buildLookupPathForward(self: *Preprocessor, tokens: []ParsedToken, start_index: usize) ![][]const u8 {
        var path: std.ArrayList([]const u8) = .empty;
        defer path.deinit(self.allocator);

        var i = start_index;

        // Add the first token (identifier, group, or lookup)
        if (i < tokens.len and (tokens[i].token_type == .TKN_IDENTIFIER or
            tokens[i].token_type == .TKN_GROUP or
            tokens[i].token_type == .TKN_LOOKUP))
        {
            try path.append(self.allocator, tokens[i].literal);
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
                try path.append(self.allocator, tokens[i].literal);
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
        var path: std.ArrayList([]const u8) = .empty;
        defer path.deinit(self.allocator);

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
            try path.append(self.allocator, tokens[@intCast(current_index)].literal);
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
                try path.insert(self.allocator, 0, token.literal);
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
        var result: std.ArrayList(Variable) = .empty;
        defer result.deinit(self.allocator);

        // First, collect any groups (scopes) that come before the identifier
        var i: isize = @intCast(index - 1);
        var groups: std.ArrayList(Variable) = .empty;
        defer groups.deinit(self.allocator);

        // Collect groups in reverse order (right to left)
        while (i >= 0) : (i -= 1) {
            if (tokens[@intCast(i)].token_type == .TKN_GROUP) {
                const t = tokens[@intCast(i)];
                try groups.append(self.allocator, Variable{
                    .name = t.literal,
                    .value = Value{ .nothing = {} },
                    .type = .nothing,
                    .mutable = false,
                    .temp = false,
                    .has_decl_prefix = false,
                    .line_number = t.line_number,
                    .token_number = t.token_number,
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
                try result.append(self.allocator, groups.items[groups.items.len - 1 - group_idx]);
            }
        }

        // Look for the lookup after the last group
        var lookup_found = false;
        if (index > 0) {
            const id_pos: isize = @intCast(index - 1);

            if (tokens[@intCast(id_pos)].token_type == .TKN_IDENTIFIER) {
                const id_tok = tokens[@intCast(id_pos)];
                try result.append(self.allocator, Variable{
                    .name = id_tok.literal,
                    .value = Value{ .nothing = {} },
                    .type = .nothing,
                    .mutable = id_tok.is_mutable,
                    .temp = id_tok.is_temporary,
                    .has_decl_prefix = id_tok.has_decl_prefix,
                    .line_number = id_tok.line_number,
                    .token_number = id_tok.token_number,
                });
                lookup_found = true;
            } else if (id_pos > 0 and tokens[@intCast(id_pos)].token_type == .TKN_TYPE and
                tokens[@intCast(id_pos - 1)].token_type == .TKN_IDENTIFIER)
            {
                const type_tok = tokens[@intCast(id_pos)];
                const id_tok = tokens[@intCast(id_pos - 1)];
                try result.append(self.allocator, Variable{
                    .name = id_tok.literal,
                    .value = Value{ .nothing = {} },
                    .type = try valueTypeFromLiteral(type_tok.literal),
                    .mutable = id_tok.is_mutable,
                    .temp = id_tok.is_temporary,
                    .has_decl_prefix = id_tok.has_decl_prefix,
                    .line_number = id_tok.line_number,
                    .token_number = id_tok.token_number,
                });
                lookup_found = true;
            }
        }

        if (!lookup_found) {
            if (self.source_lines.len > 0) {
                const t = tokens[index];
                self.underlineAt(t.line_number, t.token_number, t.literal.len);
            }
            Reporting.throwError(
                "Error: No lookup found before assignment at token index {d} (line {d}, token {d})\n",
                .{ index, tokens[index].line_number, tokens[index].token_number },
            );
            return error.InvalidAssignment;
        }

        var value_found = false;

        if (index + 1 < tokens.len and tokens[index + 1].token_type == .TKN_EXPRESSION) {
            const assign_tok = tokens[index];
            try result.append(self.allocator, Variable{
                .name = "value", // Placeholder name
                .value = Value{ .int = 12 }, // Default for expressions for now
                .type = .int,
                .mutable = false,
                .temp = false,
                .has_decl_prefix = false,
                .line_number = assign_tok.line_number,
                .token_number = assign_tok.token_number,
            });
            value_found = true;
        } else if (index + 1 < tokens.len and tokens[index + 1].token_type == .TKN_VALUE) {
            // Get the mutability from the identifier (last item in result)
            const identifier = result.items[result.items.len - 1];
            const val_tok = tokens[index + 1];
            try result.append(self.allocator, Variable{
                .name = "value", // Placeholder
                .value = val_tok.value,
                .type = val_tok.value_type,
                .mutable = identifier.mutable, // Use the mutability from the identifier
                .temp = val_tok.is_temporary,
                .has_decl_prefix = false,
                .line_number = val_tok.line_number,
                .token_number = val_tok.token_number,
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
                try result.append(self.allocator, var_struct);
                value_found = true;
            } else {
                Reporting.throwError("Internal error: RHS variable was not resolved\n", .{});
                return error.InvalidAssignment;
            }
        }

        if (!value_found) {
            if (self.source_lines.len > 0) {
                const t = tokens[index];
                self.underlineAt(t.line_number, t.token_number, t.literal.len);
            }
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
            var path_str: std.ArrayList(u8) = .empty;
            defer path_str.deinit(self.allocator);
            for (path[0 .. path.len - 1], 0..) |part, i| {
                if (i > 0) try path_str.appendSlice(self.allocator, ".");
                try path_str.appendSlice(self.allocator, part);
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

        // Last item is the value (may be coerced below)
        var value_item = assignment_array[assignment_array.len - 1];
        // Second to last is the identifier
        const identifier = assignment_array[assignment_array.len - 2];
        // Everything before the identifier is groups
        const groups = assignment_array[0 .. assignment_array.len - 2];

        // Get the target scope
        const target_scope = try self.getTargetScope(groups);

        // Step 1: Determine if we have an explicit type declaration
        const has_explicit_type = identifier.type != .nothing;
        const declared_type = if (has_explicit_type) identifier.type else value_item.type;
        var final_mutable = identifier.mutable;
        var final_temp = identifier.temp;

        // Coerce values for certain declared types (e.g. `time` accepts ISO strings).
        if (has_explicit_type) {
            const coerced = try coerceValueToType(self, declared_type, value_item);
            value_item = coerced;
        }

        // Step 2: Check if variable already exists
        if (target_scope.variables.get(identifier.name)) |existing_var| {
            // Disallow using a declaration prefix (var/const/temp) on an
            // identifier that already exists in this scope.
            if (identifier.has_decl_prefix) {
                if (self.source_lines.len > 0) {
                    self.underlineAt(identifier.line_number, identifier.token_number, identifier.name.len);
                }
                Reporting.throwError(
                    "Error: Cannot redeclare existing name '{s}' with var/const (line {d}, token {d})\n",
                    .{ identifier.name, identifier.line_number, identifier.token_number },
                );
                return error.RedeclarationNotAllowed;
            }

            // Check mutability
            if (!existing_var.mutable) {
                if (self.source_lines.len > 0) {
                    self.underlineAt(identifier.line_number, identifier.token_number, identifier.name.len);
                }
                Reporting.throwError(
                    "Error: Cannot reassign immutable variable '{s}' (assignment to '{s}' at line {d}, token {d})\n",
                    .{ identifier.name, identifier.name, identifier.line_number, identifier.token_number },
                );
                return error.ImmutableVariable;
            }

            // Check type compatibility with existing variable
            if (!isTypeCompatible(existing_var.type, value_item.type)) {
                if (self.source_lines.len > 0) {
                    self.underlineAt(identifier.line_number, identifier.token_number, identifier.name.len);
                }
                Reporting.throwError(
                    "Error: Cannot assign {s} value to variable '{s}' of type {s} (line {d}, token {d})\n",
                    .{ value_item.type.toString(), identifier.name, existing_var.type.toString(), identifier.line_number, identifier.token_number },
                );
                return error.TypeMismatch;
            }

            // For reassignments, preserve original mutability and temp status
            final_mutable = existing_var.mutable;
            final_temp = existing_var.temp;
        } else {
            // Step 3: For new variables, handle type checking/inference
            if (has_explicit_type) {
                // Check if value matches declared type
                if (!isTypeCompatible(declared_type, value_item.type)) {
                    if (self.source_lines.len > 0) {
                        self.underlineAt(identifier.line_number, identifier.token_number, identifier.name.len);
                    }
                    Reporting.throwError(
                        "Error: Cannot initialize {s} variable '{s}' with {s} value (line {d}, token {d})\n",
                        .{ declared_type.toString(), identifier.name, value_item.type.toString(), identifier.line_number, identifier.token_number },
                    );
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
            .mutable = final_mutable,
            .temp = final_temp,
            .has_decl_prefix = identifier.has_decl_prefix,
            .line_number = identifier.line_number,
            .token_number = identifier.token_number,
        };

        try target_scope.variables.put(identifier.name, variable);
    }

    fn buildExpressionLookupPath(self: *Preprocessor, expr_tokens: []Token, lookup_token: Token) ![][]const u8 {
        var path: std.ArrayList([]const u8) = .empty;
        defer path.deinit(self.allocator);

        // Locate the lookup token in the original infix expression.
        var start_index_opt: ?usize = null;
        for (expr_tokens, 0..) |t, idx| {
            if (t.token_type == lookup_token.token_type and
                t.line_number == lookup_token.line_number and
                t.token_number == lookup_token.token_number and
                std.mem.eql(u8, t.literal, lookup_token.literal))
            {
                start_index_opt = idx;
                break;
            }
        }

        if (start_index_opt == null) {
            // Fallback: treat the lookup as a single-segment name.
            try path.append(self.allocator, lookup_token.literal);
        } else {
            const start_index = start_index_opt.?;

            // First segment in the path.
            try path.append(self.allocator, expr_tokens[start_index].literal);

            // Walk forward to collect `.segment` pieces, e.g. person.job.salary.
            var j = start_index + 1;
            while (j + 1 < expr_tokens.len and
                expr_tokens[j].token_type == .TKN_ARROW and
                (expr_tokens[j + 1].token_type == .TKN_LOOKUP or expr_tokens[j + 1].token_type == .TKN_IDENTIFIER))
            {
                try path.append(self.allocator, expr_tokens[j + 1].literal);
                j += 2;
            }
        }

        const result = try self.allocator.alloc([]const u8, path.items.len);
        for (path.items, 0..) |part, idx| {
            result[idx] = part;
        }
        return result;
    }

    fn evaluateExpression(self: *Preprocessor, tokens: []Token) !Value {
        if (tokens.len == 0) {
            if (self.source_lines.len > 0 and tokens.len > 0) {
                const t = tokens[0];
                self.underlineAt(t.line_number, t.token_number, t.literal.len);
            }
            Reporting.throwError("Error: Empty expression (line {d}, token {d})\n", .{ tokens[0].line_number, tokens[0].token_number });
            return Value{ .int = 0 };
        }

        // Use the shunting yard algorithm to convert infix to postfix
        var stack: std.ArrayList(Token) = .empty;
        defer stack.deinit(self.allocator);

        var output: std.ArrayList(Token) = .empty;
        defer output.deinit(self.allocator);

        const getPrecedence = struct {
            fn get(token_type: TokenKind) u8 {
                return switch (token_type) {
                    .TKN_EXCLAIM => 4,
                    .TKN_POWER => 3,
                    .TKN_STAR, .TKN_SLASH, .TKN_PERCENT => 2,
                    .TKN_PLUS, .TKN_MINUS => 1,
                    else => 0,
                };
            }
        }.get;

        // First pass: Convert to postfix notation
        var idx: usize = 0;
        while (idx < tokens.len) : (idx += 1) {
            const token = tokens[idx];

            if (token.token_type == .TKN_VALUE) {
                output.append(self.allocator, token) catch unreachable;
            } else if (token.token_type == .TKN_LOOKUP) {
                // Treat this as the start of a dotted path like person.job.salary.
                // We only emit a single lookup token into the output and skip the
                // subsequent `.segment` pieces here; the full path is reconstructed
                // later using the original infix tokens.
                output.append(self.allocator, token) catch unreachable;

                var lookahead = idx + 1;
                while (lookahead + 1 < tokens.len and
                    tokens[lookahead].token_type == .TKN_ARROW and
                    (tokens[lookahead + 1].token_type == .TKN_LOOKUP or
                    tokens[lookahead + 1].token_type == .TKN_IDENTIFIER))
                {
                    lookahead += 2;
                }
                if (lookahead > idx + 1) {
                    idx = lookahead - 1;
                }
            } else if (token.token_type == .TKN_EXCLAIM) {
                // Unary not, highest precedence; just push onto operator stack.
                stack.append(self.allocator, token) catch unreachable;
            } else if (token.token_type == .TKN_PLUS or token.token_type == .TKN_MINUS or
                token.token_type == .TKN_STAR or token.token_type == .TKN_SLASH or
                token.token_type == .TKN_PERCENT or token.token_type == .TKN_POWER)
            {
                while (stack.items.len > 0 and
                    getPrecedence(stack.items[stack.items.len - 1].token_type) >= getPrecedence(token.token_type))
                {
                    if (stack.items.len > 0) {
                        const last_op = stack.items[stack.items.len - 1];
                        output.append(self.allocator, last_op) catch unreachable;
                        _ = stack.orderedRemove(stack.items.len - 1);
                    }
                }
                stack.append(self.allocator, token) catch unreachable;
            } else if (token.token_type == .TKN_LPAREN) {
                stack.append(self.allocator, token) catch unreachable;
            } else if (token.token_type == .TKN_RPAREN) {
                while (stack.items.len > 0 and stack.items[stack.items.len - 1].token_type != .TKN_LPAREN) {
                    if (stack.items.len > 0) {
                        const last_op = stack.items[stack.items.len - 1];
                        output.append(self.allocator, last_op) catch unreachable;
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
            output.append(self.allocator, last_op) catch unreachable;
            _ = stack.orderedRemove(stack.items.len - 1);
        }

        // Now evaluate the postfix expression
        var result_stack: std.ArrayList(Value) = .empty;
        defer result_stack.deinit(self.allocator);

        var i: usize = 0;
        while (i < output.items.len) : (i += 1) {
            const token = output.items[i];
            switch (token.token_type) {
                .TKN_VALUE => {
                    // Push the value onto the stack
                    const value: Value = switch (token.value_type) {
                        .int => Value{ .int = std.fmt.parseInt(i64, token.literal, 10) catch 0 },
                        .float => Value{ .float = std.fmt.parseFloat(f64, token.literal) catch 0 },
                        .string => Value{ .string = token.literal },
                        .bool => Value{ .bool = std.mem.eql(u8, token.literal, "true") },
                        .time => Value{ .time = std.fmt.parseInt(i64, token.literal, 10) catch 0 },
                        .nothing => Value{ .nothing = {} },
                    };
                    result_stack.append(self.allocator, value) catch unreachable;
                },
                .TKN_LOOKUP => {
                    // Resolve a variable or dotted path (person.job.salary) using
                    // the same scoped lookup rules as assignments.
                    const path = try self.buildExpressionLookupPath(tokens, token);
                    defer self.allocator.free(path);

                    const maybe_var = self.getLookupValue(path) catch {
                        if (self.source_lines.len > 0) {
                            self.underlineAt(token.line_number, token.token_number, token.literal.len);
                        }
                        // Build a dotted path string for diagnostics.
                        var buf: std.ArrayList(u8) = .empty;
                        defer buf.deinit(self.allocator);
                        for (path, 0..) |segment, seg_idx| {
                            if (seg_idx > 0) buf.appendSlice(self.allocator, ".") catch unreachable;
                            buf.appendSlice(self.allocator, segment) catch unreachable;
                        }
                        Reporting.throwError(
                            "Error: Variable '{s}' not found in expression, using 0 (line {d}, token {d})\n",
                            .{ buf.items, token.line_number, token.token_number },
                        );
                        return error.VariableNotFoundInExpression;
                    };

                    if (maybe_var) |resolved| {
                        result_stack.append(self.allocator, resolved.value) catch unreachable;
                    } else {
                        if (self.source_lines.len > 0) {
                            self.underlineAt(token.line_number, token.token_number, token.literal.len);
                        }
                        var buf: std.ArrayList(u8) = .empty;
                        defer buf.deinit(self.allocator);
                        for (path, 0..) |segment, seg_idx| {
                            if (seg_idx > 0) buf.appendSlice(self.allocator, ".") catch unreachable;
                            buf.appendSlice(self.allocator, segment) catch unreachable;
                        }
                        Reporting.throwError(
                            "Error: Variable '{s}' not found in expression, using 0 (line {d}, token {d})\n",
                            .{ buf.items, token.line_number, token.token_number },
                        );
                        return error.VariableNotFoundInExpression;
                    }
                },
                .TKN_EXCLAIM => {
                    // Unary logical NOT. Expect a single operand.
                    if (result_stack.items.len < 1) {
                        if (self.source_lines.len > 0) {
                            self.underlineAt(token.line_number, token.token_number, token.literal.len);
                        }
                        Reporting.throwError(
                            "Error: Not enough operands for operator {s} (line {d}, token {d})\n",
                            .{ token.literal, token.line_number, token.token_number },
                        );
                        return error.NotEnoughOperands;
                    }

                    const value_index = result_stack.items.len - 1;
                    const v = result_stack.items[value_index];

                    switch (v) {
                        .bool => |b| {
                            // Replace in place with inverted bool
                            result_stack.items[value_index] = Value{ .bool = !b };
                        },
                        else => {
                            if (self.source_lines.len > 0) {
                                self.underlineAt(token.line_number, token.token_number, token.literal.len);
                            }
                            Reporting.throwError(
                                "Error: Logical not (!) only supported on bool values (line {d}, token {d})\n",
                                .{ token.line_number, token.token_number },
                            );
                            return error.NonNumericValue;
                        },
                    }
                },
                .TKN_PLUS, .TKN_MINUS, .TKN_STAR, .TKN_SLASH, .TKN_PERCENT, .TKN_POWER => {
                    if (result_stack.items.len < 2) {
                        if (self.source_lines.len > 0) {
                            self.underlineAt(token.line_number, token.token_number, token.literal.len);
                        }
                        Reporting.throwError("Error: Not enough operands for operator {s} (line {d}, token {d})\n", .{ token.literal, token.line_number, token.token_number });
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
                    var a_int: i64 = 0;
                    var b_int: i64 = 0;

                    // Extract values and determine operation type
                    switch (a) {
                        .int => |val| {
                            a_int = val;
                            a_float = @floatFromInt(val);
                        },
                        .time => |val| {
                            a_int = val;
                            a_float = @floatFromInt(val);
                        },
                        .float => |val| {
                            use_float = true;
                            a_float = val;
                        },
                        .string, .bool, .nothing => {
                            if (self.source_lines.len > 0) {
                                self.underlineAt(token.line_number, token.token_number, token.literal.len);
                            }
                            Reporting.throwError("Error: Non-numeric value in expression, using 0 (line {d}, token {d})\n", .{ token.line_number, token.token_number });
                            return error.NonNumericValue;
                        },
                    }

                    switch (b) {
                        .int => |val| {
                            b_int = val;
                            b_float = @floatFromInt(val);
                        },
                        .time => |val| {
                            b_int = val;
                            b_float = @floatFromInt(val);
                        },
                        .float => |val| {
                            use_float = true;
                            b_float = val;
                        },
                        .string, .bool, .nothing => {
                            if (self.source_lines.len > 0) {
                                self.underlineAt(token.line_number, token.token_number, token.literal.len);
                            }
                            Reporting.throwError("Error: Non-numeric value in expression, using 0 (line {d}, token {d})\n", .{ token.line_number, token.token_number });
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
                                    if (self.source_lines.len > 0) {
                                        self.underlineAt(token.line_number, token.token_number, token.literal.len);
                                    }
                                    Reporting.throwError("Error: Division by zero (line {d}, token {d})\n", .{ token.line_number, token.token_number });
                                    return error.DivisionByZero;
                                }
                                return Value{ .float = a_float / b_float };
                            },
                            .TKN_PERCENT => {
                                if (b_float == 0) {
                                    if (self.source_lines.len > 0) {
                                        self.underlineAt(token.line_number, token.token_number, token.literal.len);
                                    }
                                    Reporting.throwError("Error: Modulo by zero (line {d}, token {d})\n", .{ token.line_number, token.token_number });
                                    return error.ModuloByZero;
                                }
                                return Value{ .float = @mod(a_float, b_float) };
                            },
                            .TKN_POWER => std.math.pow(f64, a_float, b_float),
                            else => unreachable,
                        };
                        try result_stack.append(self.allocator, Value{ .float = float_result });
                    } else {
                        const int_result: i64 = switch (token.token_type) {
                            .TKN_PLUS => a_int + b_int,
                            .TKN_MINUS => a_int - b_int,
                            .TKN_STAR => a_int * b_int,
                            .TKN_SLASH => {
                                if (b_int == 0) {
                                    if (self.source_lines.len > 0) {
                                        self.underlineAt(token.line_number, token.token_number, token.literal.len);
                                    }
                                    Reporting.throwError("Error: Division by zero (line {d}, token {d})\n", .{ token.line_number, token.token_number });
                                    return error.DivisionByZero;
                                }
                                return Value{ .int = @divTrunc(a_int, b_int) };
                            },
                            .TKN_PERCENT => {
                                if (b_int == 0) {
                                    if (self.source_lines.len > 0) {
                                        self.underlineAt(token.line_number, token.token_number, token.literal.len);
                                    }
                                    Reporting.throwError("Error: Modulo by zero (line {d}, token {d})\n", .{ token.line_number, token.token_number });
                                    return error.ModuloByZero;
                                }
                                return Value{ .int = @mod(a_int, b_int) };
                            },
                            .TKN_POWER => blk: {
                                var result: i64 = 1;
                                var exp = b_int;
                                while (exp > 0) : (exp -= 1) {
                                    result *= a_int;
                                }
                                break :blk result;
                            },
                            else => unreachable,
                        };
                        try result_stack.append(self.allocator, Value{ .int = int_result });
                    }
                },
                else => {},
            }
        }

        // Return the final result
        if (result_stack.items.len > 0) {
            return result_stack.items[result_stack.items.len - 1];
        } else {
            if (self.source_lines.len > 0 and tokens.len > 0) {
                const t = tokens[0];
                self.underlineAt(t.line_number, t.token_number, t.literal.len);
            }
            Reporting.throwError("Error: Empty expression result (line {d}, token {d})\n", .{ tokens[0].line_number, tokens[0].token_number });
            return Value{ .int = 0 };
        }
    }

    fn underlineAt(self: *Preprocessor, line_number: usize, token_number: usize, span_len: usize) void {
        if (self.source_lines.len == 0 or line_number == 0) return;
        const line_index = line_number - 1;
        if (line_index >= self.source_lines.len) return;
        const line = self.source_lines[line_index];
        const column: usize = if (token_number > 0) token_number - 1 else 0;
        const length: usize = if (span_len > 0) span_len else 1;
        Reporting.underline(line, column, length);
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
                                    .time => .time,
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
                                .time => Reporting.log("{d}\n", .{tokens[i - 1].value.time}),
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
                            var buffer: std.ArrayList(u8) = .empty;
                            defer buffer.deinit(self.allocator);

                            for (path, 0..) |part, idx| {
                                if (idx > 0) {
                                    try buffer.appendSlice(self.allocator, ".");
                                }
                                try buffer.appendSlice(self.allocator, part);
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
                                .time => Reporting.log("{d}\n", .{var_value.value.time}),
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
        const new_prefix = try std.fmt.allocPrint(allocator, "{s}{s}.", .{ prefix, scope_name });
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
            .time => Reporting.log("{d}", .{var_value.value.time}),
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
        var path: std.ArrayList([]const u8) = .empty;
        defer path.deinit(self.allocator);

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
        var parts: std.ArrayList([]const u8) = .empty;
        defer parts.deinit(self.allocator);

        while (current_pos >= start_line_pos) : (current_pos -= 1) {
            const token = tokens[@intCast(current_pos)];

            if (token.token_type == .TKN_IDENTIFIER or
                token.token_type == .TKN_GROUP or
                token.token_type == .TKN_LOOKUP)
            {
                try parts.append(self.allocator, token.literal);
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
                try path.append(self.allocator, part);
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

fn valueTypeFromLiteral(lit: []const u8) !ValueType {
    if (std.mem.eql(u8, lit, "int") or std.mem.eql(u8, lit, "INT")) return .int;
    if (std.mem.eql(u8, lit, "float") or std.mem.eql(u8, lit, "FLOAT")) return .float;
    if (std.mem.eql(u8, lit, "string") or std.mem.eql(u8, lit, "STRING")) return .string;
    if (std.mem.eql(u8, lit, "bool") or std.mem.eql(u8, lit, "BOOL")) return .bool;
    if (std.mem.eql(u8, lit, "time") or std.mem.eql(u8, lit, "TIME")) return .time;
    return error.UnknownType;
}

fn coerceValueToType(self: *Preprocessor, declared_type: ValueType, value_item: Preprocessor.Variable) !Preprocessor.Variable {
    var out = value_item;
    switch (declared_type) {
        .time => {
            switch (value_item.type) {
                .time => return out,
                .int => {
                    out.value = Value{ .time = value_item.value.int };
                    out.type = .time;
                    return out;
                },
                .string => {
                    const millis = try parseTimeToUnixMillis(value_item.value.string);
                    out.value = Value{ .time = millis };
                    out.type = .time;
                    return out;
                },
                else => {
                    if (self.source_lines.len > 0) {
                        self.underlineAt(value_item.line_number, value_item.token_number, 1);
                    }
                    return error.TypeMismatch;
                },
            }
        },
        else => return out,
    }
}

fn parseTimeToUnixMillis(text: []const u8) !i64 {
    const trimmed = std.mem.trim(u8, text, " \t\r\n\"");
    if (trimmed.len == 0) return error.InvalidTime;

    // Pure integer? treat as already-millis since epoch.
    var all_digits = true;
    for (trimmed) |c| {
        if (c < '0' or c > '9') {
            all_digits = false;
            break;
        }
    }
    if (all_digits) {
        return std.fmt.parseInt(i64, trimmed, 10);
    }

    // Parse a subset of ISO-8601:
    //   YYYY-MM-DD
    //   YYYY-MM-DDTHH:MM:SSZ
    //   YYYY-MM-DDTHH:MM:SS(.sss)?(Z|±HH:MM)?
    var i: usize = 0;
    const year = try parseFixedInt(trimmed, &i, 4);
    try expectChar(trimmed, &i, '-');
    const month = try parseFixedInt(trimmed, &i, 2);
    try expectChar(trimmed, &i, '-');
    const day = try parseFixedInt(trimmed, &i, 2);

    var hour: i64 = 0;
    var minute: i64 = 0;
    var second: i64 = 0;
    var millis: i64 = 0;

    if (i < trimmed.len and (trimmed[i] == 'T' or trimmed[i] == 't' or trimmed[i] == ' ')) {
        i += 1;
        hour = try parseFixedInt(trimmed, &i, 2);
        try expectChar(trimmed, &i, ':');
        minute = try parseFixedInt(trimmed, &i, 2);
        try expectChar(trimmed, &i, ':');
        second = try parseFixedInt(trimmed, &i, 2);

        if (i < trimmed.len and trimmed[i] == '.') {
            i += 1;
            const start = i;
            while (i < trimmed.len and trimmed[i] >= '0' and trimmed[i] <= '9') : (i += 1) {}
            const frac = trimmed[start..i];
            if (frac.len > 0) {
                // take up to 3 digits as milliseconds
                var tmp: i64 = 0;
                var n: usize = 0;
                while (n < frac.len and n < 3) : (n += 1) {
                    tmp = tmp * 10 + @as(i64, @intCast(frac[n] - '0'));
                }
                while (n < 3) : (n += 1) tmp *= 10;
                millis = tmp;
            }
        }
    }

    var tz_offset_minutes: i64 = 0;
    if (i < trimmed.len) {
        const c = trimmed[i];
        if (c == 'Z' or c == 'z') {
            i += 1;
        } else if (c == '+' or c == '-') {
            const sign: i64 = if (c == '-') -1 else 1;
            i += 1;
            const tzh = try parseFixedInt(trimmed, &i, 2);
            try expectChar(trimmed, &i, ':');
            const tzm = try parseFixedInt(trimmed, &i, 2);
            tz_offset_minutes = sign * (tzh * 60 + tzm);
        }
    }

    if (i != trimmed.len) return error.InvalidTime;

    const days = daysFromCivil(year, month, day);
    const total_seconds = days * 86400 + hour * 3600 + minute * 60 + second - tz_offset_minutes * 60;
    return total_seconds * 1000 + millis;
}

fn parseFixedInt(s: []const u8, idx: *usize, len: usize) !i64 {
    if (idx.* + len > s.len) return error.InvalidTime;
    var out: i64 = 0;
    for (s[idx.* .. idx.* + len]) |c| {
        if (c < '0' or c > '9') return error.InvalidTime;
        out = out * 10 + @as(i64, @intCast(c - '0'));
    }
    idx.* += len;
    return out;
}

fn expectChar(s: []const u8, idx: *usize, ch: u8) !void {
    if (idx.* >= s.len or s[idx.*] != ch) return error.InvalidTime;
    idx.* += 1;
}

fn daysFromCivil(year_in: i64, month_in: i64, day_in: i64) i64 {
    // Howard Hinnant algorithm: days relative to 1970-01-01.
    var y = year_in;
    const m = month_in;
    const d = day_in;
    y -= if (m <= 2) 1 else 0;
    const era = @divTrunc(y, 400);
    const yoe = y - era * 400;
    const mp: i64 = if (m > 2) (m - 3) else (m + 9);
    const doy = @divTrunc(153 * mp + 2, 5) + d - 1;
    const doe = yoe * 365 + @divTrunc(yoe, 4) - @divTrunc(yoe, 100) + doy;
    return era * 146097 + doe - 719468;
}

// Helper function to check type compatibility
fn isTypeCompatible(expected: ValueType, actual: ValueType) bool {
    return switch (expected) {
        .int => actual == .int,
        .float => actual == .float,
        .string => actual == .string,
        .bool => actual == .bool,
        .time => actual == .time or actual == .int,
        .nothing => true, // nothing type can be assigned to anything
    };
}
