const std = @import("std");
const ir = @import("../ir.zig");
const escape = @import("./escape.zig");

pub fn writeProgramZon(
    writer: anytype,
    program: *const ir.Program,
) anyerror!void {
    try writer.writeAll(".{\n");

    var i: usize = 0;
    while (i < program.globals.items.len) : (i += 1) {
        const binding = program.globals.items[i];
        try writeField(writer, 1, binding.name, binding.value);
    }

    try writer.writeAll("}\n");
}

fn writeField(
    writer: anytype,
    indent: usize,
    name: []const u8,
    value: ir.Value,
) anyerror!void {
    try writeIndent(writer, indent);
    try writer.writeByte('.');
    try writer.writeAll(name);
    try writer.writeAll(" = ");
    try writeValue(writer, indent, value);
    try writer.writeAll(",\n");
}

fn writeValue(
    writer: anytype,
    indent: usize,
    value: ir.Value,
) anyerror!void {
    switch (value) {
        .int => |v| try std.fmt.format(writer, "{}", .{v}),
        .float => |v| try std.fmt.format(writer, "{}", .{v}),
        .bool => |v| try std.fmt.format(writer, "{}", .{v}),
        .time => |v| try std.fmt.format(writer, "{}", .{v}),
        .string => |s| try writeString(writer, s),
        .null_ => |_| try writer.writeAll("null"),
        .object => |obj| try writeObject(writer, indent, obj),
    }
}

fn writeObject(
    writer: anytype,
    indent: usize,
    obj: *const ir.Object,
) anyerror!void {
    try writer.writeAll(".{\n");

    var i: usize = 0;
    const next_indent = indent + 1;
    while (i < obj.fields.items.len) : (i += 1) {
        const field = obj.fields.items[i];
        try writeField(writer, next_indent, field.name, field.value);
    }

    try writeIndent(writer, indent);
    try writer.writeByte('}');
}

fn writeString(writer: anytype, bytes: []const u8) anyerror!void {
    try escape.writeZigStringLiteral(writer, bytes);
}

fn writeIndent(writer: anytype, count: usize) anyerror!void {
    var i: usize = 0;
    while (i < count) : (i += 1) {
        try writer.writeAll("    ");
    }
}
