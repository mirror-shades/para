const std = @import("std");
const ir = @import("../ir.zig");
const escape = @import("./escape.zig");

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
    try escape.ensureValidUtf8(name);
    try writer.writeAll(name);

    switch (value) {
        .object => |obj| {
            try writer.writeAll(":\n");
            try writeObject(writer, indent + 1, obj);
        },
        .array => |arr| {
            if (arr.items.items.len == 0) {
                try writer.writeAll(": []\n");
            } else {
                try writer.writeAll(":\n");
                try writeArray(writer, indent + 1, arr);
            }
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
        .array => |_| return error.UnexpectedArray,
        .object => |_| return error.UnexpectedObject,
    }
}

fn writeArray(
    writer: anytype,
    indent: usize,
    arr: *const ir.Array,
) anyerror!void {
    var i: usize = 0;
    while (i < arr.items.items.len) : (i += 1) {
        const item = arr.items.items[i];
        try writeIndent(writer, indent);
        switch (item) {
            .object => |obj| {
                try writer.writeAll("-\n");
                try writeObject(writer, indent + 1, obj);
            },
            .array => |child| {
                if (child.items.items.len == 0) {
                    try writer.writeAll("- []\n");
                } else {
                    try writer.writeAll("-\n");
                    try writeArray(writer, indent + 1, child);
                }
            },
            else => {
                try writer.writeAll("- ");
                try writeScalar(writer, item);
                try writer.writeByte('\n');
            },
        }
    }
}

fn writeString(writer: anytype, bytes: []const u8) anyerror!void {
    try escape.ensureValidUtf8(bytes);
    try escape.writeYamlDoubleQuotedString(writer, bytes);
}

fn writeIndent(writer: anytype, count: usize) anyerror!void {
    var i: usize = 0;
    while (i < count) : (i += 1) {
        try writer.writeAll("  ");
    }
}
