const std = @import("std");

pub const Round = struct {
    red: ?u32,
    blue: ?u32,
    green: ?u32,
};

pub const Game = struct {
    id: u32,
    max_red: u32,
    max_blue: u32,
    max_green: u32,
};

pub fn parseLine(line: []const u8) Game {
    var game = Game{
        .id = 0,
        .max_red = 0,
        .max_blue = 0,
        .max_green = 0,
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
    var rr = std.ArrayList(Round).init(std.heap.page_allocator);
    defer rr.deinit();
    
    while (rounds.next()) |round| {
        var r = Round{
            .red = null,
            .blue = null,
            .green = null,
        };
        var colors = std.mem.split(u8, round, ",");
        while (colors.next()) |color| {
            var colorParts = std.mem.split(u8, std.mem.trim(u8, color, " "), " ");
            const number = std.fmt.parseInt(u32, std.mem.trim(u8, colorParts.next().?, " "), 10) catch 0;
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
        rr.append(r) catch {
            std.debug.print("Failed to append round\n", .{});
            @panic("Failed to append round");
        };
    }
    for (rr.items) |round| {
        game.max_red = @max(game.max_red, round.red orelse 0);
        game.max_blue = @max(game.max_blue, round.blue orelse 0);
        game.max_green = @max(game.max_green, round.green orelse 0);
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
        output += game.max_red * game.max_blue * game.max_green;
    }
    std.debug.print("{}\n", .{output});
}
