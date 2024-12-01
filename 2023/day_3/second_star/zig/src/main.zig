const std = @import("std");

const Position = struct {
    x: isize,
    y: isize,
};

const Number = struct {
    value: isize,
    position: Position,
    num_of_digits: isize,
};

const Symbol = struct {
    position: Position,
};

const Gear = struct {
    position: Position,
};

const Engine = struct {
    nums: std.ArrayList(Number),
    symbols: std.ArrayList(Symbol),
    gears: std.ArrayList(Gear),

    fn parseManualLine(self: *Engine, line: []const u8, line_num: isize) !void {
        var recording: bool = false;
        for (line, 0..) |c, i| {
            if (c == '.') {
                recording = false;
                continue;
            }
            if (std.ascii.isDigit(c)) {
                if (!recording) {
                    try self.nums.append(Number{
                        .value = c - '0',
                        .position = Position{ .y = line_num, .x = @as(u32, @truncate(i)) },
                        .num_of_digits = 1,
                    });
                    recording = true;
                } else {
                    var last_num = &self.nums.items[self.nums.items.len - 1];
                    last_num.value = last_num.value * 10 + (c - '0');
                    last_num.num_of_digits += 1;
                }
            } else if (c == '*') {
                try self.gears.append(Gear{
                    .position = Position{ .y = line_num, .x = @as(u32, @truncate(i)) },
                });
                recording = false;
            } else {
                try self.symbols.append(Symbol{
                    .position = Position{ .y = line_num, .x = @as(u32, @truncate(i)) },
                });
                recording = false;
            }
        }
    }

    fn calculateGearRatios(self: *Engine) isize {
    var total: isize = 0;
    for (self.gears.items) |g| {
        var nums: [2]isize = [_]isize{0} ** 2;
        var count: isize = 0;
        for (self.nums.items) |n| {
            if ((g.position.x >= (n.position.x - 1)) and 
                (g.position.x <= (n.position.x + n.num_of_digits)) and 
                std.math.absCast(g.position.y - n.position.y) < 2) {
                nums[@as(usize, @intCast(count))] = n.value;
                count += 1;
            }
        }
        if (count == 2) {
            total += nums[0] * nums[1];
        }
    }
    return total;
}
};

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    var allocator = gpa.allocator();

    var engine = Engine{
        .nums = std.ArrayList(Number).init(allocator),
        .symbols = std.ArrayList(Symbol).init(allocator),
        .gears = std.ArrayList(Gear).init(allocator),
    };
    defer {
        engine.nums.deinit();
        engine.symbols.deinit();
        engine.gears.deinit();
    }

    const input_file = try std.fs.cwd().openFile("../../input.txt", .{});
    defer input_file.close();
    var reader = input_file.reader();
    var buffer: [4096]u8 = undefined;
    var line_num: isize = 0;
    while (try reader.readUntilDelimiterOrEof(&buffer, '\n')) |line| {
        try engine.parseManualLine(line, line_num);
        line_num += 1;
    }

    const output = engine.calculateGearRatios();
    std.debug.print("{}\n", .{output});
}