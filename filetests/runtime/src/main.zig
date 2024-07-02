const std = @import("std");

export fn assert_eq_u64(x: u64, y: u64) void {
    if (x != y) {
        std.debug.panic("expected: {}, actual: {}\n", .{ x, y });
    }
}

export fn assert_eq_i64(x: i64, y: i64) void {
    if (x != y) {
        std.debug.panic("expected: {}, actual: {}\n", .{ x, y });
    }
}

export fn assert_eq_u1(x: u64, y: u64) void {
    if (x != y) {
        std.debug.panic("expected: {}, actual: {}\n", .{ x, y });
    }
}

export fn print_u64(x: u64) void {
    std.debug.print("{}\n", .{x});
}

export fn print_i64(x: i64) void {
    std.debug.print("{}\n", .{x});
}

extern fn entry() void;

export fn main() void {
    entry();
    std.process.exit(0);
}
