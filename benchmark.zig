const std = @import("std");

const Runs = 1000;
const Command = [_][]const u8{
    "./zig-out/bin/para.exe",
    "./test/suite/big_file.para",
};

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    const times = try allocator.alloc(u64, Runs);
    defer allocator.free(times);

    var min: u64 = std.math.maxInt(u64);
    var max: u64 = 0;
    var sum: f64 = 0;

    for (times, 0..) |*t, i| {
        const start = std.time.nanoTimestamp();
        var child = std.process.Child.init(&Command, allocator);
        _ = try child.spawnAndWait();
        const end = std.time.nanoTimestamp();

        const duration_ns = @as(u64, @intCast(end - start));
        t.* = duration_ns;

        min = @min(min, duration_ns);
        max = @max(max, duration_ns);
        sum += @as(f64, @floatFromInt(duration_ns));

        // Comment this out if you don't want per-run output
        std.debug.print("Run {d}: {d} ms\n", .{
            i + 1,
            duration_ns / 1_000_000,
        });
    }

    const avg = sum / Runs;

    // Standard deviation
    var variance_sum: f64 = 0;
    for (times) |t| {
        const diff = @as(f64, @floatFromInt(t)) - avg;
        variance_sum += diff * diff;
    }
    const stddev = std.math.sqrt(variance_sum / Runs);

    // Median (requires sorting)
    std.mem.sort(u64, times, {}, std.sort.asc(u64));
    const median = if (Runs % 2 == 0)
        (@as(f64, @floatFromInt(times[Runs / 2 - 1])) +
            @as(f64, @floatFromInt(times[Runs / 2]))) / 2.0
    else
        @as(f64, @floatFromInt(times[Runs / 2]));

    std.debug.print("\n=== Statistics ({d} runs) ===\n", .{Runs});
    std.debug.print("Average: {d:.2} ms\n", .{avg / 1_000_000});
    std.debug.print("Min:     {d} ms\n", .{min / 1_000_000});
    std.debug.print("Max:     {d} ms\n", .{max / 1_000_000});
    std.debug.print("Median:  {d:.2} ms\n", .{median / 1_000_000});
    std.debug.print("Stddev:  {d:.2} ms\n", .{stddev / 1_000_000});
}
