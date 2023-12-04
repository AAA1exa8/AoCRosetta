const std = @import("std");

pub fn main() !void {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();

    const allocator = arena.allocator();

    const input = try std.fs.cwd().openFile("../../input.txt", .{});
    defer input.close();

    var buffer: [4096]u8 = undefined;
    var reader = input.reader();

    var cards = std.ArrayList(struct { card: i32, winning: usize }).init(allocator);
    defer cards.deinit();

    while (try reader.readUntilDelimiterOrEof(&buffer, '\n')) |line| {
        var parts = std.mem.split(u8, line, ": ");
        const card_str = std.mem.trim(u8, parts.next().?, " ");
        const nums = parts.next().?;
        var d = std.mem.split(u8, card_str, " ");
        _ = d.next().?;
        const card_num = std.mem.trim(u8, d.rest(), " ");
        const card = try std.fmt.parseInt(i32, card_num, 10);

        var sections = std.mem.split(u8, nums, "|");
        const win_str = sections.next().?;
        const my_str = sections.next().?;

        var win_nums = std.ArrayList(i32).init(allocator);
        defer win_nums.deinit();
        var my_nums = std.ArrayList(i32).init(allocator);
        defer my_nums.deinit();

        var split = std.mem.split(u8, win_str, " ");
        while (split.next()) |num_str| {
            try win_nums.append(std.fmt.parseInt(i32, std.mem.trim(u8, num_str, " "), 10) catch continue);
        }

        var split_my = std.mem.split(u8, my_str, " ");
        while (split_my.next()) |num_str| {
            try my_nums.append(std.fmt.parseInt(i32, std.mem.trim(u8, num_str, " "), 10) catch continue);
        }

        var winning: usize = 0;
        for (my_nums.items) |*my_num| {
            for (win_nums.items) |*win_num| {
                if (my_num.* == win_num.*) {
                    winning += 1;
                    break;
                }
            }
        }

        try cards.append(.{ .card = card, .winning = winning });
    }

    var copies = std.ArrayList(usize).init(allocator);
    defer copies.deinit();
    try copies.resize(cards.items.len);

    for (cards.items) |*card| {
        copies.items[@as(usize, @intCast(card.card)) - 1] = 1;
    }
    for (cards.items) |*card| {
        var j: usize = 0;
        while (j < card.winning) : (j += 1) {
            copies.items[@as(usize, j) + @as(usize, @intCast(card.card))] += copies.items[@as(usize, @intCast(card.card)) - 1];
        }
    }

    var total: usize = 0;
    for (copies.items) |*copy| {
        total += copy.*;
    }

    std.debug.print("{}\n", .{total});
}
