const std = @import("std");
const lexer = @import("frontend/lexer.zig");
const token = @import("token/token.zig");
const Lexer = @import("frontend/lexer.zig").Lexer;
const Parser = @import("frontend/parser.zig").Parser;
const ast = @import("token/ast.zig");
const Preprocessor = @import("preprocessor/prepro.zig").Preprocessor;
const Writer = @import("utils/writer.zig");
const Reporting = @import("utils/reporting.zig");
const src = Reporting.DebugSource;
const json_backend = @import("backend/json.zig");
const zon_backend = @import("backend/zon.zig");
const yaml_backend = @import("backend/yaml.zig");
const toml_backend = @import("backend/toml.zig");
const ron_backend = @import("backend/ron.zig");

fn getDisplayText(token_kind: token.TokenKind, token_text: []const u8) []const u8 {
    return switch (token_kind) {
        .TKN_NEWLINE => "\\n",
        .TKN_TYPE_ASSIGN => ":",
        .TKN_VALUE_ASSIGN => "=",
        .TKN_EOF => "EOF",
        else => token_text,
    };
}

pub fn main() !void {
    var debug_lexer = false;
    var debug_parser = false;
    var debug_preprocessor = false;
    var output_json = false;
    var output_zon = false;
    var output_yaml = false;
    var output_toml = false;
    var output_ron = false;
    // Use a fixed buffer allocator to reduce heap allocations and improve performance
    // for typical input sizes. Increase the buffer if you expect very large files.
    var arena_buffer: [1024 * 1024]u8 = undefined;
    var fba = std.heap.FixedBufferAllocator.init(&arena_buffer);
    const allocator = fba.allocator();

    var args = try std.process.argsWithAllocator(allocator);
    defer args.deinit();

    // Get the program name first before skipping
    const program_name = args.next() orelse "para";

    // Process arguments
    var filename: ?[]const u8 = null;

    while (args.next()) |arg| {
        if (std.mem.eql(u8, arg, "--debug_lexer")) {
            debug_lexer = true;
        } else if (std.mem.eql(u8, arg, "--debug_parser")) {
            debug_parser = true;
        } else if (std.mem.eql(u8, arg, "--debug_preprocessor")) {
            debug_preprocessor = true;
        } else if (std.mem.eql(u8, arg, "--debug")) {
            debug_lexer = true;
            debug_parser = true;
            debug_preprocessor = true;
        } else if (std.mem.eql(u8, arg, "--json")) {
            output_json = true;
        } else if (std.mem.eql(u8, arg, "--zon")) {
            output_zon = true;
        } else if (std.mem.eql(u8, arg, "--yaml")) {
            output_yaml = true;
        } else if (std.mem.eql(u8, arg, "--toml")) {
            output_toml = true;
        } else if (std.mem.eql(u8, arg, "--ron")) {
            output_ron = true;
        } else if (std.mem.eql(u8, arg, "--help") or std.mem.eql(u8, arg, "-h") or std.mem.eql(u8, arg, "-help") or std.mem.eql(u8, arg, "--h")) {
            printUsage(program_name);
            return;
        } else if (filename == null) {
            filename = arg;
        }
    }

    var reporter = Reporting.Reporter.init(debug_lexer, debug_parser, debug_preprocessor);

    // Check if filename was provided
    const file_path = filename orelse {
        printUsage(program_name);
        return;
    };

    if (!std.mem.endsWith(u8, file_path, ".para")) {
        Reporting.throwError("File must have a .para extension\n", .{});
        return;
    }

    const file = try std.fs.cwd().openFile(file_path, .{});
    defer file.close();

    const contents = try file.readToEndAlloc(allocator, 1024 * 1024);
    defer allocator.free(contents);

    // if build folder doesn't exist, create it
    const build_dir = "build";
    std.fs.cwd().access(build_dir, .{}) catch {
        std.fs.cwd().makeDir(build_dir) catch |e| {
            Reporting.throwError("Error creating build directory: {s}\n", .{@errorName(e)});
            return;
        };
    };

    var para_lexer = try Lexer.init(allocator, contents);
    defer para_lexer.deinit();

    para_lexer.tokenize() catch |e| {
        Reporting.throwError("Error tokenizing file: {s}\n", .{@errorName(e)});
        para_lexer.dumpLexer();
        return;
    };

    if (debug_lexer) {
        Reporting.log("\n", .{});
        reporter.logDebug(src.main, "Successfully tokenized file: {s}\n", .{file_path});
        para_lexer.dumpLexer();
    }

    var para_parser = Parser.init(allocator, para_lexer.tokens.items);
    defer para_parser.deinit();

    para_parser.parse() catch |e| {
        Reporting.throwError("Error parsing file: {s}\n", .{@errorName(e)});
        para_parser.dumpParser();
        return;
    };

    if (debug_parser) {
        Reporting.log("\n", .{});
        reporter.logDebug(src.main, "Successfully parsed file: {s}\n", .{file_path});
        para_parser.dumpParser();
    }

    var preprocessor = Preprocessor.init(allocator);
    defer preprocessor.deinit();

    // Provide source lines to the preprocessor for better diagnostics
    preprocessor.setSourceLines(para_lexer.lines.items);

    if (debug_preprocessor) {
        Reporting.log("", .{});
        reporter.logDebug(src.main, "starting tokens: {s}\n", .{file_path});
    }

    try preprocessor.process(para_parser.parsed_tokens.items);

    if (output_json or output_zon or output_yaml or output_toml or output_ron) {
        var ir_program = preprocessor.buildIrProgram();
        defer ir_program.deinit(allocator);

        var stdout_file = std.fs.File.stdout().deprecatedWriter();

        if (output_json) {
            try json_backend.writeProgramJson(stdout_file, &ir_program);
        } else if (output_zon) {
            try zon_backend.writeProgramZon(stdout_file, &ir_program);
        } else if (output_yaml) {
            try yaml_backend.writeProgramYaml(stdout_file, &ir_program);
        } else if (output_toml) {
            try toml_backend.writeProgramToml(stdout_file, &ir_program);
        } else {
            try ron_backend.writeProgramRon(stdout_file, &ir_program);
        }

        try stdout_file.writeByte('\n');
    } else {
        try Writer.writeFlatFile(para_parser.parsed_tokens.items);

        // Create a file with the final variable state
        try Writer.writeVariableState("output.w.para", allocator);

        if (debug_preprocessor) {
            try preprocessor.dumpVariables(allocator);
        }
    }
}

fn printNode(node: *ast.Node, indent: usize, reporter: *Reporting.Reporter) !void {
    // Print indentation
    for (0..indent) |_| {
        reporter.logDebug("  ", .{});
    }

    // Print node info
    reporter.logDebug("{s}: {s}", .{ @tagName(node.type), node.name });
    if (node.is_const) {
        reporter.logDebug(" (const)", .{});
    }
    if (node.value) |value| {
        switch (value) {
            .int => |i| reporter.logDebug(" = {d}", .{i}),
            .float => |f| reporter.logDebug(" = {d}", .{f}),
            .string => |s| reporter.logDebug(" = {s}", .{s}),
            .bool => |b| reporter.logDebug(" = {}", .{b}),
            else => {},
        }
    }
    reporter.logDebug("\n", .{});

    // Print children with increased indentation
    if (node.children) |children| {
        for (children.items) |child| {
            try printNode(child, indent + 1, reporter);
        }
    }
}

fn printUsage(program_name: []const u8) void {
    Reporting.log("Usage: {s} [options] <file>\n", .{program_name});
    Reporting.log("Options:\n", .{});
    Reporting.log("  --debug_lexer        Enable lexer debug output\n", .{});
    Reporting.log("  --debug_parser       Enable parser debug output\n", .{});
    Reporting.log("  --debug_preprocessor Enable preprocessor debug output\n", .{});
    Reporting.log("  --debug              Enable all debug output\n", .{});
    Reporting.log("  --json               Emit JSON (experimental)\n", .{});
    Reporting.log("  --zon                Emit Zig object notation (ZON)\n", .{});
    Reporting.log("  --yaml               Emit YAML (experimental)\n", .{});
    Reporting.log("  --toml               Emit TOML (experimental)\n", .{});
    Reporting.log("  --ron                Emit RON (experimental)\n", .{});
    std.process.exit(0); // Exit cleanly after printing all help text
}
