const std = @import("std");

pub fn writeJsonString(writer: anytype, bytes: []const u8) @TypeOf(writer).Error!void {
    try writer.writeByte('"');
    for (bytes) |c| {
        switch (c) {
            '"' => try writer.writeAll("\\\""),
            '\\' => try writer.writeAll("\\\\"),
            // Optional but safe (and avoids `</script>` issues if embedded).
            '/' => try writer.writeAll("\\/"),
            0x08 => try writer.writeAll("\\b"),
            0x0C => try writer.writeAll("\\f"),
            '\n' => try writer.writeAll("\\n"),
            '\r' => try writer.writeAll("\\r"),
            '\t' => try writer.writeAll("\\t"),
            else => {
                if (c < 0x20) {
                    try writer.print("\\u00{X:0>2}", .{c});
                } else {
                    try writer.writeByte(c);
                }
            },
        }
    }
    try writer.writeByte('"');
}

pub fn writeYamlDoubleQuotedString(writer: anytype, bytes: []const u8) @TypeOf(writer).Error!void {
    try writer.writeByte('"');
    for (bytes) |c| {
        switch (c) {
            '"' => try writer.writeAll("\\\""),
            '\\' => try writer.writeAll("\\\\"),
            0x08 => try writer.writeAll("\\b"),
            0x0C => try writer.writeAll("\\f"),
            '\n' => try writer.writeAll("\\n"),
            '\r' => try writer.writeAll("\\r"),
            '\t' => try writer.writeAll("\\t"),
            else => {
                if (c < 0x20) {
                    try writer.print("\\u00{X:0>2}", .{c});
                } else {
                    try writer.writeByte(c);
                }
            },
        }
    }
    try writer.writeByte('"');
}

pub fn writeTomlBasicString(writer: anytype, bytes: []const u8) @TypeOf(writer).Error!void {
    try writer.writeByte('"');
    for (bytes) |c| {
        switch (c) {
            '"' => try writer.writeAll("\\\""),
            '\\' => try writer.writeAll("\\\\"),
            0x08 => try writer.writeAll("\\b"),
            0x0C => try writer.writeAll("\\f"),
            '\n' => try writer.writeAll("\\n"),
            '\r' => try writer.writeAll("\\r"),
            '\t' => try writer.writeAll("\\t"),
            else => {
                if (c < 0x20) {
                    try writer.print("\\u00{X:0>2}", .{c});
                } else {
                    try writer.writeByte(c);
                }
            },
        }
    }
    try writer.writeByte('"');
}

pub fn writeRonString(writer: anytype, bytes: []const u8) @TypeOf(writer).Error!void {
    try writer.writeByte('"');
    for (bytes) |c| {
        switch (c) {
            '"' => try writer.writeAll("\\\""),
            '\\' => try writer.writeAll("\\\\"),
            '\n' => try writer.writeAll("\\n"),
            '\r' => try writer.writeAll("\\r"),
            '\t' => try writer.writeAll("\\t"),
            0x00 => try writer.writeAll("\\0"),
            else => {
                if (c < 0x20) {
                    try writer.print("\\u{{{X}}}", .{c});
                } else {
                    try writer.writeByte(c);
                }
            },
        }
    }
    try writer.writeByte('"');
}

pub fn writeZigStringLiteral(writer: anytype, bytes: []const u8) @TypeOf(writer).Error!void {
    try writer.writeByte('"');
    for (bytes) |c| {
        switch (c) {
            '"' => try writer.writeAll("\\\""),
            '\\' => try writer.writeAll("\\\\"),
            '\n' => try writer.writeAll("\\n"),
            '\r' => try writer.writeAll("\\r"),
            '\t' => try writer.writeAll("\\t"),
            else => {
                if (c < 0x20) {
                    try writer.print("\\x{X:0>2}", .{c});
                } else {
                    try writer.writeByte(c);
                }
            },
        }
    }
    try writer.writeByte('"');
}

test "escape: json" {
    var buf: [256]u8 = undefined;
    var stream = std.io.fixedBufferStream(&buf);
    try writeJsonString(stream.writer(), "a\"b\\c/\n\r\t" ++ &[_]u8{ 0x01, 0x1F });
    try std.testing.expectEqualStrings("\"a\\\"b\\\\c\\/\\n\\r\\t\\u0001\\u001F\"", stream.getWritten());
}

test "escape: yaml" {
    var buf: [256]u8 = undefined;
    var stream = std.io.fixedBufferStream(&buf);
    try writeYamlDoubleQuotedString(stream.writer(), "a\"b\\c\n" ++ &[_]u8{0x1F});
    try std.testing.expectEqualStrings("\"a\\\"b\\\\c\\n\\u001F\"", stream.getWritten());
}

test "escape: toml" {
    var buf: [256]u8 = undefined;
    var stream = std.io.fixedBufferStream(&buf);
    try writeTomlBasicString(stream.writer(), "a\"b\\c\n" ++ &[_]u8{0x1F});
    try std.testing.expectEqualStrings("\"a\\\"b\\\\c\\n\\u001F\"", stream.getWritten());
}

test "escape: ron" {
    var buf: [256]u8 = undefined;
    var stream = std.io.fixedBufferStream(&buf);
    try writeRonString(stream.writer(), "a\"b\\c\n" ++ &[_]u8{ 0x00, 0x1F });
    try std.testing.expectEqualStrings("\"a\\\"b\\\\c\\n\\0\\u{1F}\"", stream.getWritten());
}

test "escape: zig" {
    var buf: [256]u8 = undefined;
    var stream = std.io.fixedBufferStream(&buf);
    try writeZigStringLiteral(stream.writer(), "a\"b\\c\n" ++ &[_]u8{0x1F});
    try std.testing.expectEqualStrings("\"a\\\"b\\\\c\\n\\x1F\"", stream.getWritten());
}
