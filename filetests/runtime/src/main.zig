const std = @import("std");
const process = std.process;
const mem = std.mem;
const debug = std.debug;
const testing = std.testing;

pub fn main() !void {
    const allocator = std.heap.c_allocator;
    const args = try std.process.argsAlloc(allocator);
    if (args.len != 3) {
        debug.print("Expected three arguments\n", .{});
        process.exit(1);
    }
    const run_type = args[1];
    const shared_library = args[2];
    if (mem.eql(u8, run_type, "u64")) {} else {
        debug.print("Invalid run type\n", .{});
        process.exit(1);
    }
    const dl = std.DynLib.open(shared_library);
    _ = dl;
    // std.debug.print("{}\n", .{result});
}
