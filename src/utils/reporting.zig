const std = @import("std");

pub const DebugSource = enum {
    main,
    lexer,
    parser,
    preprocessor,
};

pub fn underline(line: []const u8, column: usize, length: usize) void {
    std.debug.print("{s}\n", .{line});
    for (0..column) |_| {
        std.debug.print(" ", .{});
    }
    for (0..length) |_| {
        std.debug.print("^", .{});
    }
    std.debug.print("\n", .{});
}

pub const Reporter = struct {
    debug_lexer: bool,
    debug_parser: bool,
    debug_preprocessor: bool,

    pub fn init(debug_lexer: bool, debug_parser: bool, debug_preprocessor: bool) Reporter {
        return Reporter{ .debug_lexer = debug_lexer, .debug_parser = debug_parser, .debug_preprocessor = debug_preprocessor };
    }

    pub fn logDebug(self: *Reporter, from: DebugSource, comptime format: []const u8, args: anytype) void {
        switch (from) {
            .main => {
                if (self.debug_lexer or self.debug_parser or self.debug_preprocessor) {
                    return;
                }
                logOutWithPrefix("main: ", format, args);
            },
            .lexer => {
                if (!self.debug_lexer) {
                    return;
                }
                logOutWithPrefix("lexer: ", format, args);
            },
            .parser => {
                if (!self.debug_parser) {
                    return;
                }
                logOutWithPrefix("parser: ", format, args);
            },
            .preprocessor => {
                if (!self.debug_preprocessor) {
                    return;
                }
                logOutWithPrefix("preprocessor: ", format, args);
            },
        }
    }
};

pub fn throwError(comptime format: []const u8, args: anytype) void {
    logErrWithPrefix("error: ", format, args);
    std.process.exit(1);
}

pub fn throwWarning(comptime format: []const u8, args: anytype) void {
    logErrWithPrefix("warning: ", format, args);
}

pub fn log(comptime format: []const u8, args: anytype) void {
    logOutWithPrefix("", format, args);
}

// internal functions
fn write(
    writer: anytype,
    comptime prefix: []const u8,
    comptime format: []const u8,
    args: anytype,
    comptime stream: []const u8,
) void {
    nosuspend {
        writer.print(prefix ++ format, args) catch |e| {
            std.debug.print("Failed to write {s}: {}\n", .{ stream, e });
            return;
        };
    }
}

fn logOutWithPrefix(comptime actual_prefix: []const u8, comptime format: []const u8, args: anytype) void {
    const writer = std.fs.File.stdout().deprecatedWriter();
    write(writer, actual_prefix, format, args, "stdout");
}

fn logErrWithPrefix(comptime actual_prefix: []const u8, comptime format: []const u8, args: anytype) void {
    const writer = std.fs.File.stderr().deprecatedWriter();

    std.debug.lockStdErr();
    defer std.debug.unlockStdErr();
    write(writer, actual_prefix, format, args, "stderr");
}
