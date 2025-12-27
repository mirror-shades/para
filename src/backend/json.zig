const std = @import("std");
const ir = @import("../ir.zig");

pub fn writeProgramJson(
    writer: anytype,
    program: *const ir.Program,
) !void {
    try writer.writeByte('{');

    var first: bool = true;
    for (program.globals.items) |binding| {
        if (!first) {
            try writer.writeByte(',');
        }
        first = false;

        try writeString(writer, binding.name);
        try writer.writeByte(':');
        writeValue(writer, binding.value);
    }

    try writer.writeByte('}');
}

fn writeValue(writer: anytype, value: ir.Value) void {
    switch (value) {
        .int => |v| std.fmt.format(writer, "{}", .{v}) catch unreachable,
        .float => |v| std.fmt.format(writer, "{}", .{v}) catch unreachable,
        .bool => |v| std.fmt.format(writer, "{}", .{v}) catch unreachable,
        .time => |v| std.fmt.format(writer, "{}", .{v}) catch unreachable,
        .string => |s| writeString(writer, s) catch unreachable,
        .null_ => |_| writer.writeAll("null") catch unreachable,
        .object => |obj| writeObject(writer, obj) catch unreachable,
    }
}

fn writeObject(writer: anytype, obj: *const ir.Object) !void {
    try writer.writeByte('{');

    var first: bool = true;
    for (obj.fields.items) |field| {
        if (!first) {
            try writer.writeByte(',');
        }
        first = false;

        try writeString(writer, field.name);
        try writer.writeByte(':');
        writeValue(writer, field.value);
    }

    try writer.writeByte('}');
}

fn writeString(writer: anytype, bytes: []const u8) !void {
    try writer.writeByte('"');
    // TODO: escape quotes and control characters if needed
    try writer.writeAll(bytes);
    try writer.writeByte('"');
}
