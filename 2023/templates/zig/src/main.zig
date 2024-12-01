const std = @import("std");

pub fn main() !void {
    const input = try std.fs.cwd().openFile("../../input.txt", .{});
    defer input.close();

    var buffer: [4096]u8 = undefined;
    _ = buffer;
    var reader = input.reader();
    _ = reader;
}
