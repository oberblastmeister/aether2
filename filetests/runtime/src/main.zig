const std = @import("std");

export fn assert_u64(x: u64, y: u64) void {
    if (x != y) {
        std.debug.panic("expected: {}, actual: {}\n", .{ x, y });
    }
}

export fn print_u64(x: u64) void {
    std.debug.print("{}\n", .{x});
}

extern fn entry() void;

export fn main() void {
    entry();
}
