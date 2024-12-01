const std = @import("std");

pub const Round = struct {
    red: ?u8,
    blue: ?u8,
    green: ?u8,
};

pub const Game = struct {
    id: u32,
    rounds: std.ArrayList(Round),
};

pub fn parseLine(line: []const u8) Game {
    var game = Game{
        .id = 0,
        .rounds = std.ArrayList(Round).init(std.heap.page_allocator),
    };

    var parts = std.mem.split(u8, line, ":");
    var gameParts = std.mem.split(u8, parts.next().?, " ");
    _ = gameParts.next();
    var gameNumber = gameParts.next().?;
    game.id = std.fmt.parseInt(u32, gameNumber, 10) catch {
        std.debug.print("Invalid id: {s}\n", .{gameNumber});
        @panic("Invalid id");
    };
    var rounds = std.mem.split(u8, parts.next().?, ";");

    while (rounds.next()) |round| {
        var r = Round{
            .red = null,
            .blue = null,
            .green = null,
        };
        var colors = std.mem.split(u8, round, ",");
        while (colors.next()) |color| {
            var colorParts = std.mem.split(u8, std.mem.trim(u8, color, " "), " ");
            const number = std.fmt.parseInt(u8, std.mem.trim(u8, colorParts.next().?, " "), 10) catch 0;
            const colorName = std.mem.trim(u8, colorParts.next().?, " ");

            if (std.mem.eql(u8, colorName, "red")) {
                r.red = number;
            } else if (std.mem.eql(u8, colorName, "blue")) {
                r.blue = number;
            } else if (std.mem.eql(u8, colorName, "green")) {
                r.green = number;
            } else {
                std.debug.print("Invalid color: {s}\n", .{colorName});
                @panic("Invalid color");
            }
        }
        game.rounds.append(r) catch {};
    }
    return game;
}

pub fn main() !void {
    const input = try std.fs.cwd().openFile("../../input.txt", .{});
    defer input.close();

    var buffer: [4096]u8 = undefined;
    var reader = input.reader();
    var output: u32 = 0;

    while (try reader.readUntilDelimiterOrEof(&buffer, '\n')) |line| {
        var game = parseLine(line);
        if (game.rounds.items.len > 0) {
            var i: usize = 0;
            while (i < game.rounds.items.len) : (i += 1) {
                const round = game.rounds.items[i];
                if ((round.red orelse 0 > 12) or (round.blue orelse 0 > 14) or (round.green orelse 0 > 13)) {
                    break;
                }
                if (i == game.rounds.items.len - 1) {
                    output += game.id;
                }
            }
        }
    }
    std.debug.print("{}\n", .{output});
}
