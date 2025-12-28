const std = @import("std");
const builtin = @import("builtin");
const testing = std.testing;
const process = std.process;
const fs = std.fs;
const printf = std.debug.print;

// update this when adding tests
const TEST_TOTAL = 14;

fn print(comptime format: []const u8) void {
    printf(format, .{});
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
    // Start from current working directory and walk up until we find expected markers
    const cwd_dir = fs.cwd();
    var current = try cwd_dir.realpathAlloc(allocator, ".");
    // We will mutate current by repeatedly trimming the last path component
    while (true) : ({}) {
        // Try to open this directory and look for expected files
        var dir = fs.openDirAbsolute(current, .{ .iterate = true }) catch |err| {
            // If we cannot open, attempt to go up
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
            return current; // caller frees
        }

        // Move up a directory; stop if there is no parent
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

    // Discover project root regardless of runner CWD
    const project_root = try findProjectRoot(allocator);
    defer allocator.free(project_root);

    // Verify test file exists and read contents (sanity check)
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

    // Build absolute path to the para executable
    const exe_name = if (builtin.os.tag == .windows) "para.exe" else "para";
    const exe_rel = try fs.path.join(allocator, &[_][]const u8{ "zig-out", "bin", exe_name });
    defer allocator.free(exe_rel);
    const exe_path = try fs.path.join(allocator, &[_][]const u8{ project_root, exe_rel });
    defer allocator.free(exe_path);

    // Absolute path to input file for the child
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

fn runParaJsonCommand(allocator: std.mem.Allocator, path: []const u8) ![]const u8 {
    var arena = std.heap.ArenaAllocator.init(allocator);
    defer arena.deinit();
    const child_allocator = arena.allocator();

    const project_root = try findProjectRoot(allocator);
    defer allocator.free(project_root);

    var root_dir = try fs.openDirAbsolute(project_root, .{});
    defer root_dir.close();
    const file = root_dir.openFile(path, .{}) catch return error.FileNotFound;
    defer file.close();

    const exe_name = if (builtin.os.tag == .windows) "para.exe" else "para";
    const exe_rel = try fs.path.join(allocator, &[_][]const u8{ "zig-out", "bin", exe_name });
    defer allocator.free(exe_rel);
    const exe_path = try fs.path.join(allocator, &[_][]const u8{ project_root, exe_rel });
    defer allocator.free(exe_path);

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

    var root_dir = try fs.openDirAbsolute(project_root, .{});
    defer root_dir.close();
    const file = root_dir.openFile(path, .{}) catch return error.FileNotFound;
    defer file.close();

    const exe_name = if (builtin.os.tag == .windows) "para.exe" else "para";
    const exe_rel = try fs.path.join(allocator, &[_][]const u8{ "zig-out", "bin", exe_name });
    defer allocator.free(exe_rel);
    const exe_path = try fs.path.join(allocator, &[_][]const u8{ project_root, exe_rel });
    defer allocator.free(exe_path);

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

    var root_dir = try fs.openDirAbsolute(project_root, .{});
    defer root_dir.close();
    const file = root_dir.openFile(path, .{}) catch return error.FileNotFound;
    defer file.close();

    const exe_name = if (builtin.os.tag == .windows) "para.exe" else "para";
    const exe_rel = try fs.path.join(allocator, &[_][]const u8{ "zig-out", "bin", exe_name });
    defer allocator.free(exe_rel);
    const exe_path = try fs.path.join(allocator, &[_][]const u8{ project_root, exe_rel });
    defer allocator.free(exe_path);

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

    var root_dir = try fs.openDirAbsolute(project_root, .{});
    defer root_dir.close();
    const file = root_dir.openFile(path, .{}) catch return error.FileNotFound;
    defer file.close();

    const exe_name = if (builtin.os.tag == .windows) "para.exe" else "para";
    const exe_rel = try fs.path.join(allocator, &[_][]const u8{ "zig-out", "bin", exe_name });
    defer allocator.free(exe_rel);
    const exe_path = try fs.path.join(allocator, &[_][]const u8{ project_root, exe_rel });
    defer allocator.free(exe_path);

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

    var root_dir = try fs.openDirAbsolute(project_root, .{});
    defer root_dir.close();
    const file = root_dir.openFile(path, .{}) catch return error.FileNotFound;
    defer file.close();

    const exe_name = if (builtin.os.tag == .windows) "para.exe" else "para";
    const exe_rel = try fs.path.join(allocator, &[_][]const u8{ "zig-out", "bin", exe_name });
    defer allocator.free(exe_rel);
    const exe_path = try fs.path.join(allocator, &[_][]const u8{ project_root, exe_rel });
    defer allocator.free(exe_path);

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
        // First get everything after the ]
        const after_bracket = grabBetween(toParse, "]", "\n");
        if (after_bracket.len == 0) break;

        // Now parse the parts
        const name = std.mem.trim(u8, grabBetween(after_bracket, " ", ":"), &std.ascii.whitespace);
        const typ = std.mem.trim(u8, grabBetween(after_bracket, ":", "="), &std.ascii.whitespace);
        const value = std.mem.trim(u8, grabBetween(after_bracket, "=", "\n"), &std.ascii.whitespace);

        try outputs.append(allocator, Output{ .name = name, .type = typ, .value = value });

        // Find the next line by looking for the next [
        const next_line = std.mem.indexOf(u8, toParse, "\n[") orelse break;
        toParse = toParse[next_line + 1 ..];
    }
    return outputs;
}

fn grabBetween(output: []const u8, start: []const u8, end: []const u8) []const u8 {
    const start_index = std.mem.indexOf(u8, output, start) orelse return "";
    const end_index = std.mem.indexOf(u8, output[start_index + start.len ..], end) orelse {
        // If no end marker found, return everything after start to end of string
        return output[start_index + start.len ..];
    };
    return output[start_index + start.len .. start_index + start.len + end_index];
}

fn expectFloatStringApprox(expected: f64, actual: []const u8) !void {
    const parsed = try std.fmt.parseFloat(f64, actual);
    try testing.expectApproxEqAbs(expected, parsed, 1e-9);
}

// Test result tracking
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

        printf("[{d}/{d}] Running: {s}... \t", .{ self.total_tests, TEST_TOTAL, name }); // Update 3 to total when you know it

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

// Convert to regular functions (no test keyword)
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

fn testJsonGroupings(allocator: std.mem.Allocator) !void {
    const output = try runParaJsonCommand(allocator, "./test/build-checks/groupings.para");
    defer allocator.free(output);

    // JSON is the last line; split on newline and trim.
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

    // ZON output is a Zig object literal starting with .{ and
    // containing person, newPersonAge, age and salary fields.
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

    // YAML mapping with top-level newPersonAge and nested person.age and person.job.salary.
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

    // TOML root scalar and nested tables.
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

    // RON tuple-like root with nested person object.
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

// Single test function that Zig will run
test "para language tests" {
    // Set UTF-8 console output on Windows
    if (builtin.os.tag == .windows) {
        _ = std.os.windows.kernel32.SetConsoleOutputCP(65001); // UTF-8
    }

    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    print("\n🚀 Starting Para Language Test Suite\n");
    print("=" ** 50 ++ "\n");

    var runner = TestRunner.init(allocator);
    defer runner.deinit();

    // Run tests in whatever order/loop you want
    runner.runTest("Variable Assignment", testBasicVariableAssignment);
    runner.runTest("Group Assignments", testGroupAssignments);
    runner.runTest("Big File Processing", testBigFile);
    runner.runTest("Sugar Syntax Test", testSugar);
    runner.runTest("JSON Grouping Test", testJsonGroupings);
    runner.runTest("JSON Big File Test", testJsonBigFile);
    runner.runTest("ZON Grouping Test", testZonGroupings);
    runner.runTest("ZON Big File Test", testZonBigFile);
    runner.runTest("YAML Grouping Test", testYamlGroupings);
    runner.runTest("YAML Big File Test", testYamlBigFile);
    runner.runTest("TOML Grouping Test", testTomlGroupings);
    runner.runTest("TOML Big File Test", testTomlBigFile);
    runner.runTest("RON Grouping Test", testRonGroupings);
    runner.runTest("RON Big File Test", testRonBigFile);

    // Generate the report
    runner.generateReport();

    // Fail the overall test if any individual test failed
    if (runner.passed_tests != runner.total_tests) {
        return error.TestsFailed;
    }
}
