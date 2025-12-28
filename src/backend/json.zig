const std = @import("std");
const ir = @import("../ir.zig");
const escape = @import("./escape.zig");

pub fn writeProgramJson(
    writer: anytype,
    program: *const ir.Program,
) @TypeOf(writer).Error!void {
    try writer.writeByte('{');

    var first: bool = true;
    for (program.globals.items) |binding| {
        if (!first) {
            try writer.writeByte(',');
        }
        first = false;

        try writeString(writer, binding.name);
        try writer.writeByte(':');
        try writeValue(writer, binding.value);
    }

    try writer.writeByte('}');
}

fn writeValue(writer: anytype, value: ir.Value) @TypeOf(writer).Error!void {
    switch (value) {
        .int => |v| try std.fmt.format(writer, "{}", .{v}),
        .float => |v| try std.fmt.format(writer, "{}", .{v}),
        .bool => |v| try std.fmt.format(writer, "{}", .{v}),
        .time => |v| try std.fmt.format(writer, "{}", .{v}),
        .string => |s| try writeString(writer, s),
        .null_ => |_| try writer.writeAll("null"),
        .object => |obj| try writeObject(writer, obj),
    }
}

fn writeObject(writer: anytype, obj: *const ir.Object) @TypeOf(writer).Error!void {
    try writer.writeByte('{');

    var first: bool = true;
    for (obj.fields.items) |field| {
        if (!first) {
            try writer.writeByte(',');
        }
        first = false;

        try writeString(writer, field.name);
        try writer.writeByte(':');
        try writeValue(writer, field.value);
    }

    try writer.writeByte('}');
}

fn writeString(writer: anytype, bytes: []const u8) @TypeOf(writer).Error!void {
    try escape.writeJsonString(writer, bytes);
}
