const std = @import("std");

pub const ValueKind = enum {
    int,
    float,
    bool,
    string,
    time,
    null_,
    object,
};

pub const Binding = struct {
    name: []const u8,
    value: Value,
};

pub const Object = struct {
    fields: std.ArrayList(Binding),

    pub fn init(allocator: std.mem.Allocator) Object {
        return .{ .fields = std.ArrayList(Binding).init(allocator) };
    }

    pub fn deinit(self: *Object) void {
        self.fields.deinit();
    }
};

pub const Value = union(ValueKind) {
    int: i64,
    float: f64,
    bool: bool,
    string: []const u8,
    time: i64,
    null_: void,
    object: *Object,
};

pub const Program = struct {
    globals: std.ArrayList(Binding),

    pub fn init(allocator: std.mem.Allocator) Program {
        return .{ .globals = std.ArrayList(Binding).init(allocator) };
    }

    pub fn deinit(self: *Program) void {
        self.globals.deinit();
    }
};

