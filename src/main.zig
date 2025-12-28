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
    var output_path: ?[]const u8 = null;
    var use_stdout: bool = false;
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();

    var arena = std.heap.ArenaAllocator.init(gpa.allocator());
    defer arena.deinit();
    const allocator = arena.allocator();

    var args = try std.process.argsWithAllocator(allocator);
    defer args.deinit();

    const program_name = args.next() orelse "para";

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
        } else if (std.mem.eql(u8, arg, "--out")) {
            output_path = args.next() orelse {
                Reporting.throwError("--out requires a path\n", .{});
                return;
            };
        } else if (std.mem.eql(u8, arg, "--stdout")) {
            use_stdout = true;
        } else if (std.mem.eql(u8, arg, "--help") or std.mem.eql(u8, arg, "-h") or std.mem.eql(u8, arg, "-help") or std.mem.eql(u8, arg, "--h")) {
            printUsage(program_name);
            return;
        } else if (filename == null) {
            filename = arg;
        }
    }

    var reporter = Reporting.Reporter.init(debug_lexer, debug_parser, debug_preprocessor);

    const max_input_bytes: usize = 64 * 1024 * 1024;

    const file_path = filename orelse "-";
    const contents = blk: {
        if (std.mem.eql(u8, file_path, "-")) {
            const stdin_file = std.fs.File.stdin();
            break :blk try stdin_file.readToEndAlloc(allocator, max_input_bytes);
        }

        if (!std.mem.endsWith(u8, file_path, ".para")) {
            Reporting.throwError("File must have a .para extension (or use '-' for stdin)\n", .{});
            return;
        }

        const file = try std.fs.cwd().openFile(file_path, .{});
        defer file.close();
        break :blk try file.readToEndAlloc(allocator, max_input_bytes);
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

    preprocessor.setSourceLines(para_lexer.lines.items);

    if (debug_preprocessor) {
        Reporting.log("", .{});
        reporter.logDebug(src.main, "starting tokens: {s}\n", .{file_path});
    }

    try preprocessor.process(para_parser.parsed_tokens.items);

    if (output_json or output_zon or output_yaml or output_toml or output_ron) {
        var ir_program = try preprocessor.buildIrProgram();
        defer ir_program.deinit(allocator);

        var out_file: ?std.fs.File = null;
        defer if (out_file) |*f| f.close();

        const writer = blk: {
            if (use_stdout or output_path == null or std.mem.eql(u8, output_path.?, "-")) {
                break :blk std.fs.File.stdout().deprecatedWriter();
            }

            const out = output_path.?;
            if (std.fs.path.dirname(out)) |dir| {
                std.fs.cwd().makePath(dir) catch |e| switch (e) {
                    error.PathAlreadyExists => {},
                    else => return e,
                };
            }
            out_file = try std.fs.cwd().createFile(out, .{});
            break :blk out_file.?.deprecatedWriter();
        };

        if (output_json) {
            try json_backend.writeProgramJson(writer, &ir_program);
        } else if (output_zon) {
            try zon_backend.writeProgramZon(writer, &ir_program);
        } else if (output_yaml) {
            try yaml_backend.writeProgramYaml(writer, &ir_program);
        } else if (output_toml) {
            try toml_backend.writeProgramToml(writer, &ir_program);
        } else {
            try ron_backend.writeProgramRon(writer, &ir_program);
        }

        try writer.writeByte('\n');
    } else {
        if (!use_stdout and output_path != null and !std.mem.eql(u8, output_path.?, "-")) {
            const out = output_path.?;
            if (std.fs.path.dirname(out)) |dir| {
                std.fs.cwd().makePath(dir) catch |e| switch (e) {
                    error.PathAlreadyExists => {},
                    else => return e,
                };
            }
            var file = try std.fs.cwd().createFile(out, .{});
            defer file.close();
            try Writer.writeFlatFileToWriter(file.deprecatedWriter(), para_parser.parsed_tokens.items);
        }

        if (debug_preprocessor) {
            try preprocessor.dumpVariables(allocator);
        }
    }
}

fn printNode(node: *ast.Node, indent: usize, reporter: *Reporting.Reporter) !void {
    for (0..indent) |_| {
        reporter.logDebug("  ", .{});
    }

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
    Reporting.log("  --out <path>          Write output to file (use '-' for stdout)\n", .{});
    Reporting.log("  --stdout              Force stdout output\n", .{});
    std.process.exit(0);
}
