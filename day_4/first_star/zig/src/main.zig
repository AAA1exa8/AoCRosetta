const std = @import("std");

pub fn main() !void {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();

    const allocator = arena.allocator();

    const input = try std.fs.cwd().openFile("../../input.txt", .{});
    defer input.close();

    var buffer: [4096]u8 = undefined;
    var reader = input.reader();

    var total: usize = 0;
    while (try reader.readUntilDelimiterOrEof(&buffer, '\n')) |line| {
        var parts = std.mem.split(u8, line, ":");
        const card = parts.next() orelse continue;
        _ = card;
        const nums = parts.next() orelse continue;

        var sections = std.mem.split(u8, nums, "|");
        const win = sections.next() orelse continue;
        const my = sections.next() orelse continue;

        var win_nums = std.ArrayList(i32).init(allocator);
        defer win_nums.deinit();
        var my_nums = std.ArrayList(i32).init(allocator);
        defer my_nums.deinit();

        var split = std.mem.split(u8, win, " ");

        while (split.next()) |num| {
            try win_nums.append(std.fmt.parseInt(i32, num, 10) catch continue);
        }

        var split_my = std.mem.split(u8, my, " ");

        while (split_my.next()) |num| {
            try my_nums.append(std.fmt.parseInt(i32, num, 10) catch continue);
        }

        var count: u32 = 0;
        for (my_nums.items) |*my_num| {
            for (win_nums.items) |*win_num| {
                if (my_num.* == win_num.*) {
                    count += 1;
                    break;
                }
            }
        }

        total += if (count == 0) 0 else std.math.pow(usize, 2, count - 1);
    }

    std.debug.print("{}\n", .{total});
}
