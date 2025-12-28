const std = @import("std");
const ir = @import("../ir.zig");

pub fn writeProgramYaml(
    writer: anytype,
    program: *const ir.Program,
) anyerror!void {
    var i: usize = 0;
    while (i < program.globals.items.len) : (i += 1) {
        const binding = program.globals.items[i];
        try writeBinding(writer, 0, binding.name, binding.value);
    }
}

fn writeBinding(
    writer: anytype,
    indent: usize,
    name: []const u8,
    value: ir.Value,
) anyerror!void {
    try writeIndent(writer, indent);
    try writer.writeAll(name);

    switch (value) {
        .object => |obj| {
            try writer.writeAll(":\n");
            try writeObject(writer, indent + 1, obj);
        },
        else => {
            try writer.writeAll(": ");
            try writeScalar(writer, value);
            try writer.writeByte('\n');
        },
    }
}

fn writeObject(
    writer: anytype,
    indent: usize,
    obj: *const ir.Object,
) anyerror!void {
    var i: usize = 0;
    while (i < obj.fields.items.len) : (i += 1) {
        const field = obj.fields.items[i];
        try writeBinding(writer, indent, field.name, field.value);
    }
}

fn writeScalar(
    writer: anytype,
    value: ir.Value,
) anyerror!void {
    switch (value) {
        .int => |v| try std.fmt.format(writer, "{}", .{v}),
        .float => |v| try std.fmt.format(writer, "{}", .{v}),
        .bool => |v| try std.fmt.format(writer, "{}", .{v}),
        .time => |v| try std.fmt.format(writer, "{}", .{v}),
        .string => |s| try writeString(writer, s),
        .null_ => |_| try writer.writeAll("null"),
        .object => |_| return error.UnexpectedObject,
    }
}

fn writeString(writer: anytype, bytes: []const u8) anyerror!void {
    try writer.writeByte('"');
    // TODO: escape quotes and control characters if needed
    try writer.writeAll(bytes);
    try writer.writeByte('"');
}

fn writeIndent(writer: anytype, count: usize) anyerror!void {
    var i: usize = 0;
    while (i < count) : (i += 1) {
        try writer.writeAll("  ");
    }
}
