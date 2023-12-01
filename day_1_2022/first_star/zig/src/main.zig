const std = @import("std");

pub fn main() !void {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();

    const file_path = "../../input.txt";
    const file = try std.fs.cwd().openFile(file_path, .{});
    defer file.close();
    var reader = file.reader();

    var maxCalories: u64 = 0;
    var currentCalories: u64 = 0;
    var buffer: [4096]u8 = undefined;
    while (try reader.readUntilDelimiterOrEof(&buffer, '\n')) |line| {
        if (line.len == 0) {
            maxCalories = @max(maxCalories, currentCalories);
            currentCalories = 0;
        } else {
            currentCalories += std.fmt.parseInt(u64, line, 10) catch continue;
        }
    }
    maxCalories = @max(maxCalories, currentCalories);
    const stdout = std.io.getStdOut().writer();
    try stdout.print("{}\n", .{maxCalories});
}