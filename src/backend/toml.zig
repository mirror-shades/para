const std = @import("std");
const ir = @import("../ir.zig");

pub fn writeProgramToml(
    writer: anytype,
    program: *const ir.Program,
) anyerror!void {
    var i: usize = 0;
    while (i < program.globals.items.len) : (i += 1) {
        const binding = program.globals.items[i];
        switch (binding.value) {
            .object => {},
            else => try writeKeyValue(writer, binding.name, binding.value),
        }
    }

    var first_table: bool = true;
    i = 0;
    while (i < program.globals.items.len) : (i += 1) {
        const binding = program.globals.items[i];
        switch (binding.value) {
            .object => |obj| {
                const path = [_][]const u8{binding.name};
                try writeTable(writer, &first_table, &path, obj);
            },
            else => {},
        }
    }
}

fn writeKeyValue(
    writer: anytype,
    name: []const u8,
    value: ir.Value,
) anyerror!void {
    try writer.writeAll(name);
    try writer.writeAll(" = ");
    try writeScalar(writer, value);
    try writer.writeByte('\n');
}

fn writeTable(
    writer: anytype,
    first_table: *bool,
    path: []const []const u8,
    obj: *const ir.Object,
) anyerror!void {
    if (!first_table.*) {
        try writer.writeByte('\n');
    } else {
        first_table.* = false;
    }

    try writer.writeByte('[');
    var i: usize = 0;
    while (i < path.len) : (i += 1) {
        if (i != 0) try writer.writeByte('.');
        try writer.writeAll(path[i]);
    }
    try writer.writeAll("]\n");

    i = 0;
    while (i < obj.fields.items.len) : (i += 1) {
        const field = obj.fields.items[i];
        switch (field.value) {
            .object => {},
            else => try writeKeyValue(writer, field.name, field.value),
        }
    }

    i = 0;
    while (i < obj.fields.items.len) : (i += 1) {
        const field = obj.fields.items[i];
        switch (field.value) {
            .object => |child_obj| {
                const child_path = try buildChildPath(path, field.name);
                defer std.heap.page_allocator.free(child_path);
                try writeTable(writer, first_table, child_path, child_obj);
            },
            else => {},
        }
    }
}

fn buildChildPath(
    parent: []const []const u8,
    name: []const u8,
) ![]const []const u8 {
    const child = try std.heap.page_allocator.alloc([]const u8, parent.len + 1);
    var i: usize = 0;
    while (i < parent.len) : (i += 1) {
        child[i] = parent[i];
    }
    child[parent.len] = name;
    return child;
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
