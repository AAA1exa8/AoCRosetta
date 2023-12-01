const std = @import("std");

pub fn main() !void {
    const allocator = std.heap.page_allocator;
    const input = try std.fs.cwd().openFile("../../input.txt", .{});
    defer input.close();

    var buffer: [4096]u8 = undefined;
    var reader = input.reader();

    var output: usize = 0;
    while (try reader.readUntilDelimiterOrEof(&buffer, '\n')) |line| {
        var b = std.ArrayList(u8).init(allocator);
        defer b.deinit();

        for (line) |c| {
            if (std.ascii.isDigit(c)) {
                try b.append(c - '0');
            }
        }

        if (b.items.len > 0) {
            output += b.items[0]*10 + b.items[b.items.len - 1];
        }
    }

    std.debug.print("{}\n", .{output});
}