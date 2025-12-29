const std = @import("std");

pub const ValueKind = enum {
    int,
    float,
    bool,
    string,
    time,
    null_,
    array,
    object,
};

pub const Binding = struct {
    name: []const u8,
    value: Value,
};

pub const Object = struct {
    fields: std.ArrayList(Binding),

    pub fn init(allocator: std.mem.Allocator) Object {
        _ = allocator;
        return .{ .fields = .empty };
    }

    pub fn deinit(self: *Object, allocator: std.mem.Allocator) void {
        self.fields.deinit(allocator);
    }
};

pub const Array = struct {
    items: std.ArrayList(Value),

    pub fn init(allocator: std.mem.Allocator) Array {
        _ = allocator;
        return .{ .items = .empty };
    }

    pub fn deinit(self: *Array, allocator: std.mem.Allocator) void {
        self.items.deinit(allocator);
    }
};

pub const Value = union(ValueKind) {
    int: i64,
    float: f64,
    bool: bool,
    string: []const u8,
    time: i64,
    null_: void,
    array: *Array,
    object: *Object,
};

pub const Program = struct {
    globals: std.ArrayList(Binding),

    pub fn init(allocator: std.mem.Allocator) Program {
        _ = allocator;
        return .{ .globals = .empty };
    }

    pub fn deinit(self: *Program, allocator: std.mem.Allocator) void {
        self.globals.deinit(allocator);
    }
};
