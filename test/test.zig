const std = @import("std");
const builtin = @import("builtin");
const testing = std.testing;
const process = std.process;
const fs = std.fs;
const printf = std.debug.print;
const para_src = @import("para_src");
const Preprocessor = para_src.Preprocessor;
const token = para_src.token;
const ir = para_src.ir;
const json_backend = para_src.json_backend;
const yaml_backend = para_src.yaml_backend;
const toml_backend = para_src.toml_backend;
const zon_backend = para_src.zon_backend;

const TEST_TOTAL = 26;

fn print(comptime format: []const u8) void {
    printf(format, .{});
}

fn requireReadableFileAbsolute(path: []const u8) !void {
    const file = fs.openFileAbsolute(path, .{}) catch return error.FileNotFound;
    file.close();
}

fn getParaExePath(allocator: std.mem.Allocator, project_root: []const u8) ![]u8 {
    const exe_name = if (builtin.os.tag == .windows) "para.exe" else "para";

    if (process.getEnvVarOwned(allocator, "PARA_BIN")) |env_path| {
        if (fs.path.isAbsolute(env_path)) {
            errdefer allocator.free(env_path);
            try requireReadableFileAbsolute(env_path);
            return env_path;
        }

        defer allocator.free(env_path);
        const abs = try fs.path.join(allocator, &[_][]const u8{ project_root, env_path });
        errdefer allocator.free(abs);
        try requireReadableFileAbsolute(abs);
        return abs;
    } else |err| switch (err) {
        error.EnvironmentVariableNotFound => {},
        else => return err,
    }

    const exe_rel = try fs.path.join(allocator, &[_][]const u8{ "zig-out", "bin", exe_name });
    defer allocator.free(exe_rel);
    const exe_abs = try fs.path.join(allocator, &[_][]const u8{ project_root, exe_rel });
    errdefer allocator.free(exe_abs);
    try requireReadableFileAbsolute(exe_abs);
    return exe_abs;
}

fn dirHasFile(dir: fs.Dir, name: []const u8) bool {
    if (dir.access(name, .{})) |_| {
        return true;
    } else |_| {
        return false;
    }
}

fn dirHasDir(dir: fs.Dir, name: []const u8) bool {
    var sub = dir.openDir(name, .{}) catch return false;
    defer sub.close();
    return true;
}

fn findProjectRoot(allocator: std.mem.Allocator) ![]u8 {
    const cwd_dir = fs.cwd();
    var current = try cwd_dir.realpathAlloc(allocator, ".");
    while (true) : ({}) {
        var dir = fs.openDirAbsolute(current, .{ .iterate = true }) catch |err| {
            if (std.fs.path.dirname(current)) |parent| {
                allocator.free(current);
                current = try allocator.dupe(u8, parent);
                continue;
            } else {
                return err;
            }
        };
        defer dir.close();

        const has_build = dirHasFile(dir, "build.zig");
        const has_src_main = dirHasFile(dir, "src/main.zig");
        const has_test_dir = dirHasDir(dir, "test");
        if (has_build and has_src_main and has_test_dir) {
            return current;
        }

        if (std.fs.path.dirname(current)) |parent| {
            allocator.free(current);
            current = try allocator.dupe(u8, parent);
            continue;
        } else {
            return error.ProjectRootNotFound;
        }
    }
}

fn runParaCommand(allocator: std.mem.Allocator, path: []const u8) ![]const u8 {
    var arena = std.heap.ArenaAllocator.init(allocator);
    defer arena.deinit();
    const child_allocator = arena.allocator();

    const project_root = try findProjectRoot(allocator);
    defer allocator.free(project_root);

    const exe_path = try getParaExePath(allocator, project_root);
    defer allocator.free(exe_path);

    var root_dir = try fs.openDirAbsolute(project_root, .{});
    defer root_dir.close();
    const file = root_dir.openFile(path, .{}) catch |err| {
        const abs_candidate = fs.path.join(allocator, &[_][]const u8{ project_root, path }) catch "";
        defer if (abs_candidate.len > 0) allocator.free(abs_candidate);
        std.debug.print("Failed to open test file {s}: {}\n", .{ if (abs_candidate.len > 0) abs_candidate else path, err });
        return error.FileNotFound;
    };
    defer file.close();

    const file_contents = try file.readToEndAlloc(child_allocator, std.math.maxInt(usize));
    defer child_allocator.free(file_contents);

    const input_abs = try fs.path.join(allocator, &[_][]const u8{ project_root, path });
    defer allocator.free(input_abs);

    const result = try process.Child.run(.{
        .allocator = child_allocator,
        .argv = &[_][]const u8{ exe_path, input_abs },
        .cwd = project_root,
        .max_output_bytes = std.math.maxInt(usize),
    });
    defer child_allocator.free(result.stdout);
    defer child_allocator.free(result.stderr);

    switch (result.term) {
        .Exited => |code| if (code != 0) return error.CommandFailed,
        else => return error.CommandFailed,
    }

    return try allocator.dupe(u8, result.stdout);
}

fn runParaCommandExpectFailure(allocator: std.mem.Allocator, path: []const u8) ![]const u8 {
    var arena = std.heap.ArenaAllocator.init(allocator);
    defer arena.deinit();
    const child_allocator = arena.allocator();

    const project_root = try findProjectRoot(allocator);
    defer allocator.free(project_root);

    const exe_path = try getParaExePath(allocator, project_root);
    defer allocator.free(exe_path);

    var root_dir = try fs.openDirAbsolute(project_root, .{});
    defer root_dir.close();
    _ = root_dir.openFile(path, .{}) catch return error.FileNotFound;

    const input_abs = try fs.path.join(allocator, &[_][]const u8{ project_root, path });
    defer allocator.free(input_abs);

    const result = try process.Child.run(.{
        .allocator = child_allocator,
        .argv = &[_][]const u8{ exe_path, input_abs },
        .cwd = project_root,
        .max_output_bytes = std.math.maxInt(usize),
    });
    defer child_allocator.free(result.stdout);
    defer child_allocator.free(result.stderr);

    switch (result.term) {
        .Exited => |code| if (code == 0) return error.ExpectedFailure,
        else => return error.CommandFailed,
    }

    // Prefer stderr since Para reports errors there.
    if (result.stderr.len > 0) return try allocator.dupe(u8, result.stderr);
    return try allocator.dupe(u8, result.stdout);
}

fn runParaJsonCommand(allocator: std.mem.Allocator, path: []const u8) ![]const u8 {
    var arena = std.heap.ArenaAllocator.init(allocator);
    defer arena.deinit();
    const child_allocator = arena.allocator();

    const project_root = try findProjectRoot(allocator);
    defer allocator.free(project_root);

    const exe_path = try getParaExePath(allocator, project_root);
    defer allocator.free(exe_path);

    var root_dir = try fs.openDirAbsolute(project_root, .{});
    defer root_dir.close();
    const file = root_dir.openFile(path, .{}) catch return error.FileNotFound;
    defer file.close();

    const input_abs = try fs.path.join(allocator, &[_][]const u8{ project_root, path });
    defer allocator.free(input_abs);

    const result = try process.Child.run(.{
        .allocator = child_allocator,
        .argv = &[_][]const u8{ exe_path, "--json", input_abs },
        .cwd = project_root,
        .max_output_bytes = std.math.maxInt(usize),
    });
    defer child_allocator.free(result.stdout);
    defer child_allocator.free(result.stderr);

    switch (result.term) {
        .Exited => |code| if (code != 0) return error.CommandFailed,
        else => return error.CommandFailed,
    }

    return try allocator.dupe(u8, result.stdout);
}

fn runParaZonCommand(allocator: std.mem.Allocator, path: []const u8) ![]const u8 {
    var arena = std.heap.ArenaAllocator.init(allocator);
    defer arena.deinit();
    const child_allocator = arena.allocator();

    const project_root = try findProjectRoot(allocator);
    defer allocator.free(project_root);

    const exe_path = try getParaExePath(allocator, project_root);
    defer allocator.free(exe_path);

    var root_dir = try fs.openDirAbsolute(project_root, .{});
    defer root_dir.close();
    const file = root_dir.openFile(path, .{}) catch return error.FileNotFound;
    defer file.close();

    const input_abs = try fs.path.join(allocator, &[_][]const u8{ project_root, path });
    defer allocator.free(input_abs);

    const result = try process.Child.run(.{
        .allocator = child_allocator,
        .argv = &[_][]const u8{ exe_path, "--zon", input_abs },
        .cwd = project_root,
        .max_output_bytes = std.math.maxInt(usize),
    });
    defer child_allocator.free(result.stdout);
    defer child_allocator.free(result.stderr);

    switch (result.term) {
        .Exited => |code| if (code != 0) return error.CommandFailed,
        else => return error.CommandFailed,
    }

    return try allocator.dupe(u8, result.stdout);
}

fn runParaYamlCommand(allocator: std.mem.Allocator, path: []const u8) ![]const u8 {
    var arena = std.heap.ArenaAllocator.init(allocator);
    defer arena.deinit();
    const child_allocator = arena.allocator();

    const project_root = try findProjectRoot(allocator);
    defer allocator.free(project_root);

    const exe_path = try getParaExePath(allocator, project_root);
    defer allocator.free(exe_path);

    var root_dir = try fs.openDirAbsolute(project_root, .{});
    defer root_dir.close();
    const file = root_dir.openFile(path, .{}) catch return error.FileNotFound;
    defer file.close();

    const input_abs = try fs.path.join(allocator, &[_][]const u8{ project_root, path });
    defer allocator.free(input_abs);

    const result = try process.Child.run(.{
        .allocator = child_allocator,
        .argv = &[_][]const u8{ exe_path, "--yaml", input_abs },
        .cwd = project_root,
        .max_output_bytes = std.math.maxInt(usize),
    });
    defer child_allocator.free(result.stdout);
    defer child_allocator.free(result.stderr);

    switch (result.term) {
        .Exited => |code| if (code != 0) return error.CommandFailed,
        else => return error.CommandFailed,
    }

    return try allocator.dupe(u8, result.stdout);
}

fn runParaTomlCommand(allocator: std.mem.Allocator, path: []const u8) ![]const u8 {
    var arena = std.heap.ArenaAllocator.init(allocator);
    defer arena.deinit();
    const child_allocator = arena.allocator();

    const project_root = try findProjectRoot(allocator);
    defer allocator.free(project_root);

    const exe_path = try getParaExePath(allocator, project_root);
    defer allocator.free(exe_path);

    var root_dir = try fs.openDirAbsolute(project_root, .{});
    defer root_dir.close();
    const file = root_dir.openFile(path, .{}) catch return error.FileNotFound;
    defer file.close();

    const input_abs = try fs.path.join(allocator, &[_][]const u8{ project_root, path });
    defer allocator.free(input_abs);

    const result = try process.Child.run(.{
        .allocator = child_allocator,
        .argv = &[_][]const u8{ exe_path, "--toml", input_abs },
        .cwd = project_root,
        .max_output_bytes = std.math.maxInt(usize),
    });
    defer child_allocator.free(result.stdout);
    defer child_allocator.free(result.stderr);

    switch (result.term) {
        .Exited => |code| if (code != 0) return error.CommandFailed,
        else => return error.CommandFailed,
    }

    return try allocator.dupe(u8, result.stdout);
}

fn runParaRonCommand(allocator: std.mem.Allocator, path: []const u8) ![]const u8 {
    var arena = std.heap.ArenaAllocator.init(allocator);
    defer arena.deinit();
    const child_allocator = arena.allocator();

    const project_root = try findProjectRoot(allocator);
    defer allocator.free(project_root);

    const exe_path = try getParaExePath(allocator, project_root);
    defer allocator.free(exe_path);

    var root_dir = try fs.openDirAbsolute(project_root, .{});
    defer root_dir.close();
    const file = root_dir.openFile(path, .{}) catch return error.FileNotFound;
    defer file.close();

    const input_abs = try fs.path.join(allocator, &[_][]const u8{ project_root, path });
    defer allocator.free(input_abs);

    const result = try process.Child.run(.{
        .allocator = child_allocator,
        .argv = &[_][]const u8{ exe_path, "--ron", input_abs },
        .cwd = project_root,
        .max_output_bytes = std.math.maxInt(usize),
    });
    defer child_allocator.free(result.stdout);
    defer child_allocator.free(result.stderr);

    switch (result.term) {
        .Exited => |code| if (code != 0) return error.CommandFailed,
        else => return error.CommandFailed,
    }

    return try allocator.dupe(u8, result.stdout);
}

const Output = struct {
    name: []const u8,
    type: []const u8,
    value: []const u8,
};

fn parseOutput(output: []const u8, allocator: std.mem.Allocator) !std.ArrayList(Output) {
    var outputs: std.ArrayList(Output) = .empty;
    var toParse = output;
    while (toParse.len > 0) {
        const after_bracket = grabBetween(toParse, "]", "\n");
        if (after_bracket.len == 0) break;

        const name = std.mem.trim(u8, grabBetween(after_bracket, " ", ":"), &std.ascii.whitespace);
        const typ = std.mem.trim(u8, grabBetween(after_bracket, ":", "="), &std.ascii.whitespace);
        const value = std.mem.trim(u8, grabBetween(after_bracket, "=", "\n"), &std.ascii.whitespace);

        try outputs.append(allocator, Output{ .name = name, .type = typ, .value = value });

        const next_line = std.mem.indexOf(u8, toParse, "\n[") orelse break;
        toParse = toParse[next_line + 1 ..];
    }
    return outputs;
}

fn grabBetween(output: []const u8, start: []const u8, end: []const u8) []const u8 {
    const start_index = std.mem.indexOf(u8, output, start) orelse return "";
    const end_index = std.mem.indexOf(u8, output[start_index + start.len ..], end) orelse {
        return output[start_index + start.len ..];
    };
    return output[start_index + start.len .. start_index + start.len + end_index];
}

fn expectFloatStringApprox(expected: f64, actual: []const u8) !void {
    const parsed = try std.fmt.parseFloat(f64, actual);
    try testing.expectApproxEqAbs(expected, parsed, 1e-9);
}

const TestResult = struct {
    name: []const u8,
    passed: bool,
    duration_ms: u64,
    error_msg: ?[]const u8 = null,
};

const TestRunner = struct {
    allocator: std.mem.Allocator,
    results: std.ArrayList(TestResult),
    total_tests: u32 = 0,
    passed_tests: u32 = 0,
    start_time: i64,

    fn init(allocator: std.mem.Allocator) TestRunner {
        return TestRunner{
            .allocator = allocator,
            .results = .empty,
            .start_time = std.time.milliTimestamp(),
        };
    }

    fn deinit(self: *TestRunner) void {
        for (self.results.items) |result| {
            if (result.error_msg) |msg| {
                self.allocator.free(msg);
            }
        }
        self.results.deinit(self.allocator);
    }

    fn runTest(self: *TestRunner, name: []const u8, test_fn: fn (std.mem.Allocator) anyerror!void) void {
        self.total_tests += 1;
        const test_start = std.time.milliTimestamp();

        printf("[{d}/{d}] Running: {s}... \t", .{ self.total_tests, TEST_TOTAL, name });

        const result = test_fn(self.allocator);
        const duration = @as(u64, @intCast(std.time.milliTimestamp() - test_start));

        if (result) {
            printf("✅ PASSED ({d}ms)\n", .{duration});
            self.passed_tests += 1;
            self.results.append(self.allocator, TestResult{
                .name = name,
                .passed = true,
                .duration_ms = duration,
            }) catch {};
        } else |err| {
            const error_msg = std.fmt.allocPrint(self.allocator, "{}", .{err}) catch "Unknown error";
            printf("❌ FAILED ({d}ms) - {s}\n", .{ duration, error_msg });
            self.results.append(self.allocator, TestResult{
                .name = name,
                .passed = false,
                .duration_ms = duration,
                .error_msg = error_msg,
            }) catch {};
        }
    }

    fn generateReport(self: *TestRunner) void {
        const total_duration = @as(u64, @intCast(std.time.milliTimestamp() - self.start_time));

        print("\n" ++ "=" ** 50 ++ "\n");
        print("TEST REPORT\n");
        print("=" ** 50 ++ "\n");
        printf("Total tests: {d}\n", .{self.total_tests});
        printf("Passed: {d}\n", .{self.passed_tests});
        printf("Failed: {d}\n", .{self.total_tests - self.passed_tests});
        printf("Total time: {d}ms\n", .{total_duration});
        print("\n");

        if (self.passed_tests == self.total_tests) {
            print("\n🎉 All tests passed!\n");
        } else {
            printf("\n💥 {d} test(s) failed!\n", .{self.total_tests - self.passed_tests});
        }
    }
};

fn testBasicVariableAssignment(allocator: std.mem.Allocator) !void {
    const output = try runParaCommand(allocator, "./test/build-checks/variable_assign.para");
    defer allocator.free(output);

    var outputs = try parseOutput(output, allocator);
    defer outputs.deinit(allocator);

    try testing.expectEqualStrings("int_assign", outputs.items[0].name);
    try testing.expectEqualStrings("int", outputs.items[0].type);
    try testing.expectEqualStrings("5", outputs.items[0].value);

    try testing.expectEqualStrings("expression_assign", outputs.items[1].name);
    try testing.expectEqualStrings("int", outputs.items[1].type);
    try testing.expectEqualStrings("10", outputs.items[1].value);

    try testing.expectEqualStrings("float_assign", outputs.items[2].name);
    try testing.expectEqualStrings("float", outputs.items[2].type);
    try expectFloatStringApprox(5.5, outputs.items[2].value);

    try testing.expectEqualStrings("bool_assign", outputs.items[3].name);
    try testing.expectEqualStrings("bool", outputs.items[3].type);
    try testing.expectEqualStrings("TRUE", outputs.items[3].value);

    try testing.expectEqualStrings("string_assign", outputs.items[4].name);
    try testing.expectEqualStrings("string", outputs.items[4].type);
    try testing.expectEqualStrings("\"hello\"", outputs.items[4].value);
}

fn testGroupAssignments(allocator: std.mem.Allocator) !void {
    const output = try runParaCommand(allocator, "./test/build-checks/groupings.para");
    defer allocator.free(output);

    var outputs = try parseOutput(output, allocator);
    defer outputs.deinit(allocator);

    try testing.expectEqualStrings("person.age", outputs.items[0].name);
    try testing.expectEqualStrings("int", outputs.items[0].type);
    try testing.expectEqualStrings("50", outputs.items[0].value);

    try testing.expectEqualStrings("person.job.salary", outputs.items[1].name);
    try testing.expectEqualStrings("int", outputs.items[1].type);
    try testing.expectEqualStrings("50000", outputs.items[1].value);

    try testing.expectEqualStrings("newPersonAge", outputs.items[2].name);
    try testing.expectEqualStrings("int", outputs.items[2].type);
    try testing.expectEqualStrings("50", outputs.items[2].value);
}

fn testBigFile(allocator: std.mem.Allocator) !void {
    const output = try runParaCommand(allocator, "./test/build-checks/big_file.para");
    defer allocator.free(output);

    var outputs = try parseOutput(output, allocator);
    defer outputs.deinit(allocator);

    try testing.expectEqualStrings("person.age", outputs.items[0].name);
    try testing.expectEqualStrings("int", outputs.items[0].type);
    try testing.expectEqualStrings("51", outputs.items[0].value);

    try testing.expectEqualStrings("person.job.title", outputs.items[1].name);
    try testing.expectEqualStrings("string", outputs.items[1].type);
    try testing.expectEqualStrings("\"Painter\"", outputs.items[1].value);

    try testing.expectEqualStrings("person.job.salary", outputs.items[2].name);
    try testing.expectEqualStrings("float", outputs.items[2].type);
    try expectFloatStringApprox(70000.0, outputs.items[2].value);

    try testing.expectEqualStrings("person.name", outputs.items[3].name);
    try testing.expectEqualStrings("string", outputs.items[3].type);
    try testing.expectEqualStrings("\"Bob\"", outputs.items[3].value);

    try testing.expectEqualStrings("person.working", outputs.items[4].name);
    try testing.expectEqualStrings("bool", outputs.items[4].type);
    try testing.expectEqualStrings("TRUE", outputs.items[4].value);

    try testing.expectEqualStrings("y", outputs.items[5].name);
    try testing.expectEqualStrings("float", outputs.items[5].type);
    try expectFloatStringApprox(70000.0, outputs.items[5].value);

    try testing.expectEqualStrings("value", outputs.items[6].name);
    try testing.expectEqualStrings("int", outputs.items[6].type);
    try testing.expectEqualStrings("5", outputs.items[6].value);
}

fn testSugar(allocator: std.mem.Allocator) !void {
    const output = try runParaCommand(allocator, "./test/build-checks/sugar.para");
    defer allocator.free(output);

    var outputs = try parseOutput(output, allocator);
    defer outputs.deinit(allocator);

    try testing.expectEqualStrings("raise", outputs.items[0].name);
    try testing.expectEqualStrings("int", outputs.items[0].type);
    try testing.expectEqualStrings("500", outputs.items[0].value);

    try testing.expectEqualStrings("man", outputs.items[1].name);
    try testing.expectEqualStrings("string", outputs.items[1].type);
    try testing.expectEqualStrings("\"Bob\"", outputs.items[1].value);

    try testing.expectEqualStrings("person.age", outputs.items[2].name);
    try testing.expectEqualStrings("int", outputs.items[2].type);
    try testing.expectEqualStrings("50", outputs.items[2].value);

    try testing.expectEqualStrings("person.retirement", outputs.items[3].name);
    try testing.expectEqualStrings("int", outputs.items[3].type);
    try testing.expectEqualStrings("70", outputs.items[3].value);

    try testing.expectEqualStrings("person.name", outputs.items[4].name);
    try testing.expectEqualStrings("string", outputs.items[4].type);
    try testing.expectEqualStrings("\"Bob\"", outputs.items[4].value);

    try testing.expectEqualStrings("person.job.salary", outputs.items[5].name);
    try testing.expectEqualStrings("int", outputs.items[5].type);
    try testing.expectEqualStrings("15500", outputs.items[5].value);

    try testing.expectEqualStrings("person.job.title", outputs.items[6].name);
    try testing.expectEqualStrings("string", outputs.items[6].type);
    try testing.expectEqualStrings("\"Bartender\"", outputs.items[6].value);
}

fn testLineCommentsPreserveNewlines(allocator: std.mem.Allocator) !void {
    const output = try runParaCommand(allocator, "./test/build-checks/comments.para");
    defer allocator.free(output);

    var outputs = try parseOutput(output, allocator);
    defer outputs.deinit(allocator);

    try testing.expectEqualStrings("a", outputs.items[0].name);
    try testing.expectEqualStrings("int", outputs.items[0].type);
    try testing.expectEqualStrings("1", outputs.items[0].value);

    try testing.expectEqualStrings("b", outputs.items[1].name);
    try testing.expectEqualStrings("int", outputs.items[1].type);
    try testing.expectEqualStrings("2", outputs.items[1].value);
}

fn testTimeType(allocator: std.mem.Allocator) !void {
    const output = try runParaCommand(allocator, "./test/build-checks/time.para");
    defer allocator.free(output);

    var outputs = try parseOutput(output, allocator);
    defer outputs.deinit(allocator);

    try testing.expectEqualStrings("created", outputs.items[0].name);
    try testing.expectEqualStrings("time", outputs.items[0].type);
    try testing.expectEqualStrings("1710434700000", outputs.items[0].value);

    try testing.expectEqualStrings("created_ms", outputs.items[1].name);
    try testing.expectEqualStrings("time", outputs.items[1].type);
    try testing.expectEqualStrings("1710434700000", outputs.items[1].value);
}

fn testTimeTypeInvalid(allocator: std.mem.Allocator) !void {
    const err_month = try runParaCommandExpectFailure(allocator, "./test/build-checks/time_invalid_month.para");
    defer allocator.free(err_month);
    try testing.expect(std.mem.indexOf(u8, err_month, "Invalid time literal") != null);

    const err_day = try runParaCommandExpectFailure(allocator, "./test/build-checks/time_invalid_day.para");
    defer allocator.free(err_day);
    try testing.expect(std.mem.indexOf(u8, err_day, "Invalid time literal") != null);

    const err_tz = try runParaCommandExpectFailure(allocator, "./test/build-checks/time_invalid_tz.para");
    defer allocator.free(err_tz);
    try testing.expect(std.mem.indexOf(u8, err_tz, "Invalid time literal") != null);
}

fn testJsonGroupings(allocator: std.mem.Allocator) !void {
    const output = try runParaJsonCommand(allocator, "./test/build-checks/groupings.para");
    defer allocator.free(output);

    var it = std.mem.tokenizeAny(u8, output, "\r\n");
    var last: []const u8 = "";
    while (it.next()) |line| {
        if (line.len != 0) last = line;
    }

    try testing.expect(last.len != 0);
    try testing.expect(std.mem.startsWith(u8, last, "{\""));
    try testing.expect(std.mem.indexOf(u8, last, "\"person\"") != null);
    try testing.expect(std.mem.indexOf(u8, last, "\"newPersonAge\"") != null);
    try testing.expect(std.mem.indexOf(u8, last, "\"age\":50") != null);
    try testing.expect(std.mem.indexOf(u8, last, "\"salary\":50000") != null);
}

fn testJsonBigFile(allocator: std.mem.Allocator) !void {
    const output = try runParaJsonCommand(allocator, "./test/build-checks/big_file.para");
    defer allocator.free(output);

    var it = std.mem.tokenizeAny(u8, output, "\r\n");
    var last: []const u8 = "";
    while (it.next()) |line| {
        if (line.len != 0) last = line;
    }

    try testing.expect(last.len != 0);
    try testing.expect(std.mem.startsWith(u8, last, "{\""));

    try testing.expect(std.mem.indexOf(u8, last, "\"person\"") != null);
    try testing.expect(std.mem.indexOf(u8, last, "\"working\":true") != null);
    try testing.expect(std.mem.indexOf(u8, last, "\"name\":\"Bob\"") != null);
    try testing.expect(std.mem.indexOf(u8, last, "\"job\"") != null);
    try testing.expect(std.mem.indexOf(u8, last, "\"title\":\"Painter\"") != null);
}

fn testZonGroupings(allocator: std.mem.Allocator) !void {
    const output = try runParaZonCommand(allocator, "./test/build-checks/groupings.para");
    defer allocator.free(output);

    try testing.expect(std.mem.indexOf(u8, output, ".{") != null);
    try testing.expect(std.mem.indexOf(u8, output, ".person = .{") != null);
    try testing.expect(std.mem.indexOf(u8, output, ".newPersonAge = 50") != null);
    try testing.expect(std.mem.indexOf(u8, output, ".age = 50") != null);
    try testing.expect(std.mem.indexOf(u8, output, ".salary = 50000") != null);
}

fn testZonBigFile(allocator: std.mem.Allocator) !void {
    const output = try runParaZonCommand(allocator, "./test/build-checks/big_file.para");
    defer allocator.free(output);

    try testing.expect(std.mem.indexOf(u8, output, ".{") != null);
    try testing.expect(std.mem.indexOf(u8, output, ".person = .{") != null);
    try testing.expect(std.mem.indexOf(u8, output, ".working = true") != null);
    try testing.expect(std.mem.indexOf(u8, output, ".name = \"Bob\"") != null);
    try testing.expect(std.mem.indexOf(u8, output, ".job = .{") != null);
    try testing.expect(std.mem.indexOf(u8, output, ".title = \"Painter\"") != null);
}

fn testYamlGroupings(allocator: std.mem.Allocator) !void {
    const output = try runParaYamlCommand(allocator, "./test/build-checks/groupings.para");
    defer allocator.free(output);

    try testing.expect(std.mem.indexOf(u8, output, "newPersonAge: 50") != null);
    try testing.expect(std.mem.indexOf(u8, output, "person:") != null);
    try testing.expect(std.mem.indexOf(u8, output, "age: 50") != null);
    try testing.expect(std.mem.indexOf(u8, output, "salary: 50000") != null);
}

fn testYamlBigFile(allocator: std.mem.Allocator) !void {
    const output = try runParaYamlCommand(allocator, "./test/build-checks/big_file.para");
    defer allocator.free(output);

    try testing.expect(std.mem.indexOf(u8, output, "person:") != null);
    try testing.expect(std.mem.indexOf(u8, output, "working: true") != null);
    try testing.expect(std.mem.indexOf(u8, output, "name: \"Bob\"") != null);
    try testing.expect(std.mem.indexOf(u8, output, "job:") != null);
    try testing.expect(std.mem.indexOf(u8, output, "title: \"Painter\"") != null);
}

fn testTomlGroupings(allocator: std.mem.Allocator) !void {
    const output = try runParaTomlCommand(allocator, "./test/build-checks/groupings.para");
    defer allocator.free(output);

    try testing.expect(std.mem.indexOf(u8, output, "newPersonAge = 50") != null);
    try testing.expect(std.mem.indexOf(u8, output, "[person]") != null);
    try testing.expect(std.mem.indexOf(u8, output, "age = 50") != null);
    try testing.expect(std.mem.indexOf(u8, output, "[person.job]") != null);
    try testing.expect(std.mem.indexOf(u8, output, "salary = 50000") != null);
}

fn testTomlBigFile(allocator: std.mem.Allocator) !void {
    const output = try runParaTomlCommand(allocator, "./test/build-checks/big_file.para");
    defer allocator.free(output);

    try testing.expect(std.mem.indexOf(u8, output, "[person]") != null);
    try testing.expect(std.mem.indexOf(u8, output, "working = true") != null);
    try testing.expect(std.mem.indexOf(u8, output, "name = \"Bob\"") != null);
    try testing.expect(std.mem.indexOf(u8, output, "[person.job]") != null);
    try testing.expect(std.mem.indexOf(u8, output, "title = \"Painter\"") != null);
}

fn testRonGroupings(allocator: std.mem.Allocator) !void {
    const output = try runParaRonCommand(allocator, "./test/build-checks/groupings.para");
    defer allocator.free(output);

    try testing.expect(std.mem.indexOf(u8, output, "(") != null);
    try testing.expect(std.mem.indexOf(u8, output, "newPersonAge: 50") != null);
    try testing.expect(std.mem.indexOf(u8, output, "person: (") != null);
    try testing.expect(std.mem.indexOf(u8, output, "age: 50") != null);
    try testing.expect(std.mem.indexOf(u8, output, "salary: 50000") != null);
}

fn testRonBigFile(allocator: std.mem.Allocator) !void {
    const output = try runParaRonCommand(allocator, "./test/build-checks/big_file.para");
    defer allocator.free(output);

    try testing.expect(std.mem.indexOf(u8, output, "person: (") != null);
    try testing.expect(std.mem.indexOf(u8, output, "working: true") != null);
    try testing.expect(std.mem.indexOf(u8, output, "name: \"Bob\"") != null);
    try testing.expect(std.mem.indexOf(u8, output, "job: (") != null);
    try testing.expect(std.mem.indexOf(u8, output, "title: \"Painter\"") != null);
}

fn testDivisionOperator(allocator: std.mem.Allocator) !void {
    const output = try runParaCommand(allocator, "./test/build-checks/division.para");
    defer allocator.free(output);

    var outputs = try parseOutput(output, allocator);
    defer outputs.deinit(allocator);

    try testing.expectEqualStrings("div", outputs.items[0].name);
    try testing.expectEqualStrings("int", outputs.items[0].type);
    try testing.expectEqualStrings("3", outputs.items[0].value);
}

fn testUnterminatedMultilineCommentFails(allocator: std.mem.Allocator) !void {
    const output = try runParaCommandExpectFailure(allocator, "./test/build-checks/unterminated_comment.para");
    defer allocator.free(output);

    try testing.expect(
        std.mem.indexOf(u8, output, "Unterminated multiline comment") != null or
            std.mem.indexOf(u8, output, "UnterminatedMultilineComment") != null,
    );
}

fn testIntegerOverflowLiteralFails(allocator: std.mem.Allocator) !void {
    const output = try runParaCommandExpectFailure(allocator, "./test/build-checks/int_overflow.para");
    defer allocator.free(output);

    try testing.expect(std.mem.indexOf(u8, output, "Invalid integer literal") != null);
}

fn testJsonDeterministicOrdering(allocator: std.mem.Allocator) !void {
    const output = try runParaJsonCommand(allocator, "./test/build-checks/json_order.para");
    defer allocator.free(output);

    try testing.expectEqualStrings("{\"a\":2,\"b\":1,\"person\":{\"age\":1}}\n", output);
}

fn testJsonArrays(allocator: std.mem.Allocator) !void {
    const output = try runParaJsonCommand(allocator, "./test/build-checks/arrays.para");
    defer allocator.free(output);

    try testing.expect(std.mem.indexOf(u8, output, "\"empty\":[]") != null);
    try testing.expect(std.mem.indexOf(u8, output, "\"empty2\":[]") != null);
    try testing.expect(std.mem.indexOf(u8, output, "\"xs\":[1,2,3]") != null);
    try testing.expect(std.mem.indexOf(u8, output, "\"xss\":[[1,2],[3]]") != null);
    try testing.expect(std.mem.indexOf(u8, output, "\"person\":{\"scores\":[10,20]}") != null);
}

fn testZonArrays(allocator: std.mem.Allocator) !void {
    const output = try runParaZonCommand(allocator, "./test/build-checks/arrays.para");
    defer allocator.free(output);

    try testing.expect(std.mem.indexOf(u8, output, ".empty = .{") != null);
    try testing.expect(std.mem.indexOf(u8, output, ".empty2 = .{") != null);
    try testing.expect(std.mem.indexOf(u8, output, ".xs = .{") != null);
    try testing.expect(std.mem.indexOf(u8, output, ".xss = .{") != null);
    try testing.expect(std.mem.indexOf(u8, output, ".person = .{") != null);
    try testing.expect(std.mem.indexOf(u8, output, ".scores = .{") != null);
}

fn testYamlArrays(allocator: std.mem.Allocator) !void {
    const output = try runParaYamlCommand(allocator, "./test/build-checks/arrays.para");
    defer allocator.free(output);

    try testing.expect(std.mem.indexOf(u8, output, "empty: []") != null);
    try testing.expect(std.mem.indexOf(u8, output, "empty2: []") != null);
    try testing.expect(std.mem.indexOf(u8, output, "xs:") != null);
    try testing.expect(std.mem.indexOf(u8, output, "- 1") != null);
    try testing.expect(std.mem.indexOf(u8, output, "xss:") != null);
    try testing.expect(std.mem.indexOf(u8, output, "person:") != null);
    try testing.expect(std.mem.indexOf(u8, output, "scores:") != null);
    try testing.expect(std.mem.indexOf(u8, output, "- 10") != null);
}

fn testTomlArrays(allocator: std.mem.Allocator) !void {
    const output = try runParaTomlCommand(allocator, "./test/build-checks/arrays.para");
    defer allocator.free(output);

    try testing.expect(std.mem.indexOf(u8, output, "empty = []") != null);
    try testing.expect(std.mem.indexOf(u8, output, "empty2 = []") != null);
    try testing.expect(std.mem.indexOf(u8, output, "xs = [1, 2, 3]") != null);
    try testing.expect(std.mem.indexOf(u8, output, "xss = [[1, 2], [3]]") != null);
    try testing.expect(std.mem.indexOf(u8, output, "[person]") != null);
    try testing.expect(std.mem.indexOf(u8, output, "scores = [10, 20]") != null);
}

fn testRonArrays(allocator: std.mem.Allocator) !void {
    const output = try runParaRonCommand(allocator, "./test/build-checks/arrays.para");
    defer allocator.free(output);

    try testing.expect(std.mem.indexOf(u8, output, "empty: [") != null);
    try testing.expect(std.mem.indexOf(u8, output, "empty2: [") != null);
    try testing.expect(std.mem.indexOf(u8, output, "xs: [") != null);
    try testing.expect(std.mem.indexOf(u8, output, "xss: [") != null);
    try testing.expect(std.mem.indexOf(u8, output, "person: (") != null);
    try testing.expect(std.mem.indexOf(u8, output, "scores: [") != null);
}

test "IR build fails on OOM (no silent export data loss)" {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    var pre = Preprocessor.init(allocator);
    defer pre.deinit();

    const long_string = "this string is definitely longer than sixteen bytes";
    try pre.root_scope.variables.put("a", .{
        .name = "a",
        .value = token.Value{ .string = long_string },
        .type = .string,
        .array_depth = 0,
        .mutable = false,
        .temp = false,
        .has_decl_prefix = false,
        .line_number = 0,
        .token_number = 0,
    });

    var backing: [16]u8 = undefined;
    var fba = std.heap.FixedBufferAllocator.init(&backing);

    try testing.expectError(error.OutOfMemory, pre.buildIrProgramWithAllocator(fba.allocator()));
}

test "exporters reject invalid UTF-8 strings" {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    var program = ir.Program.init(allocator);
    defer program.deinit(allocator);

    const invalid = &[_]u8{ 0xC3, 0x28 };
    try program.globals.append(allocator, .{ .name = "s", .value = ir.Value{ .string = invalid } });

    var buf: [256]u8 = undefined;

    var json_stream = std.io.fixedBufferStream(&buf);
    try testing.expectError(error.InvalidUtf8String, json_backend.writeProgramJson(json_stream.writer(), &program));

    var yaml_stream = std.io.fixedBufferStream(&buf);
    try testing.expectError(error.InvalidUtf8String, yaml_backend.writeProgramYaml(yaml_stream.writer(), &program));

    var toml_stream = std.io.fixedBufferStream(&buf);
    try testing.expectError(error.InvalidUtf8String, toml_backend.writeProgramToml(toml_stream.writer(), &program));

    var zon_stream = std.io.fixedBufferStream(&buf);
    try testing.expectError(error.InvalidUtf8String, zon_backend.writeProgramZon(zon_stream.writer(), &program));
}

test "para language tests" {
    if (builtin.os.tag == .windows) {
        _ = std.os.windows.kernel32.SetConsoleOutputCP(65001);
    }

    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    print("\n🚀 Starting Para Language Test Suite\n");
    print("=" ** 50 ++ "\n");

    var runner = TestRunner.init(allocator);
    defer runner.deinit();

    runner.runTest("Variable Assignment", testBasicVariableAssignment);
    runner.runTest("Group Assignments", testGroupAssignments);
    runner.runTest("Big File Processing", testBigFile);
    runner.runTest("Sugar Syntax Test", testSugar);
    runner.runTest("Comment Newlines", testLineCommentsPreserveNewlines);
    runner.runTest("Time Type Test", testTimeType);
    runner.runTest("Invalid Time Test", testTimeTypeInvalid);
    runner.runTest("Division Operator", testDivisionOperator);
    runner.runTest("Unterminated /* */", testUnterminatedMultilineCommentFails);
    runner.runTest("Int Overflow Test", testIntegerOverflowLiteralFails);
    runner.runTest("JSON Grouping Test", testJsonGroupings);
    runner.runTest("JSON Big File Test", testJsonBigFile);
    runner.runTest("JSON Ordering Test", testJsonDeterministicOrdering);
    runner.runTest("JSON Arrays Test", testJsonArrays);
    runner.runTest("ZON Grouping Test", testZonGroupings);
    runner.runTest("ZON Big File Test", testZonBigFile);
    runner.runTest("ZON Arrays Test", testZonArrays);
    runner.runTest("YAML Grouping Test", testYamlGroupings);
    runner.runTest("YAML Big File Test", testYamlBigFile);
    runner.runTest("YAML Arrays Test", testYamlArrays);
    runner.runTest("TOML Grouping Test", testTomlGroupings);
    runner.runTest("TOML Big File Test", testTomlBigFile);
    runner.runTest("TOML Arrays Test", testTomlArrays);
    runner.runTest("RON Grouping Test", testRonGroupings);
    runner.runTest("RON Big File Test", testRonBigFile);
    runner.runTest("RON Arrays Test", testRonArrays);

    runner.generateReport();

    if (runner.passed_tests != runner.total_tests) {
        return error.TestsFailed;
    }
}
