const std = @import("std");

pub fn main() !void {
    const input = try std.fs.cwd().openFile("../../input.txt", .{});
    defer input.close();

    var buffer: [4096]u8 = undefined;
    var reader = input.reader();

    const nums = [_][]const u8{
        "one", "two", "three", "four", "five", "six", "seven", "eight", "nine",
    };

    var output: usize = 0;
    while (try reader.readUntilDelimiterOrEof(&buffer, '\n')) |line| {
        var first: u32 = 0;
        var last: u32 = 0;
        var i: usize = 0;
        while (i < line.len) {
            var cur: ?u32 = null;
            // std.debug.print("{c}\n", .{line[i]});
            if (std.ascii.isDigit(line[i])) {
                cur = (line[i] - '0') % 10;
            } else {
                var j: usize = 0;
                while (j < nums.len) : (j += 1) {
                    if (std.mem.startsWith(u8, line[i..], nums[j])) {
                        cur = @as(u32, @truncate(j)) + 1;
                        break;
                    }
                }
            }
            if (cur) |c| {
                if (first == 0) {
                    first = c;
                    last = c;
                } else {
                    last = c;
                }
            }
            i += 1;
        }
        output += first * 10 + last;
    }

    std.debug.print("{}\n", .{output});
}
