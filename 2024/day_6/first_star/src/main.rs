use core::panic;

#[derive(Eq, PartialEq)]
enum Tile {
    Empty,
    Building,
    Visited,
}

enum Direction {
    Up,
    Down,
    Left,
    Right,
}

struct Position {
    x: usize,
    y: usize,
    direction: Direction,
}

fn main() {
    let input = "../input.txt";
    let input_file = std::fs::read_to_string(input).expect("Error reading input file");
    let mut guard = (0, 0);
    let mut map = input_file
        .lines()
        .enumerate()
        .map(|(i, line)| {
            line.chars()
                .enumerate()
                .map(|(j, c)| match c {
                    '.' => Tile::Empty,
                    '#' => Tile::Building,
                    '^' => {
                        guard = (j, i);
                        Tile::Visited
                    }
                    _ => panic!("invalid char"),
                })
                .collect::<Vec<Tile>>()
        })
        .collect::<Vec<Vec<Tile>>>();
    let mut position = Position {
        x: guard.0,
        y: guard.1,
        direction: Direction::Up,
    };
    loop {
        match position.direction {
            Direction::Up => {
                if position.y == 0 {
                    break;
                } else if Tile::Empty == map[position.y - 1][position.x]
                    || Tile::Visited == map[position.y - 1][position.x]
                {
                    position.y -= 1;
                    map[position.y][position.x] = Tile::Visited;
                } else {
                    position.direction = Direction::Right;
                }
            }
            Direction::Down => {
                if position.y == map.len() - 1 {
                    break;
                } else if Tile::Empty == map[position.y + 1][position.x]
                    || Tile::Visited == map[position.y + 1][position.x]
                {
                    position.y += 1;
                    map[position.y][position.x] = Tile::Visited;
                } else {
                    position.direction = Direction::Left;
                }
            }
            Direction::Left => {
                if position.x == 0 {
                    break;
                } else if Tile::Empty == map[position.y][position.x - 1]
                    || Tile::Visited == map[position.y][position.x - 1]
                {
                    position.x -= 1;
                    map[position.y][position.x] = Tile::Visited;
                } else {
                    position.direction = Direction::Up;
                }
            }
            Direction::Right => {
                if position.x == map[0].len() - 1 {
                    break;
                } else if Tile::Empty == map[position.y][position.x + 1]
                    || Tile::Visited == map[position.y][position.x + 1]
                {
                    position.x += 1;
                    map[position.y][position.x] = Tile::Visited;
                } else {
                    position.direction = Direction::Down;
                }
            }
        }
    }
    println!(
        "{}",
        map.iter()
            .map(|row| row.iter().filter(|t| t == &&Tile::Visited).count())
            .sum::<usize>()
    );
}

fn print_map(map: &Vec<Vec<Tile>>) {
    for row in map {
        for tile in row {
            match tile {
                Tile::Empty => print!("."),
                Tile::Building => print!("#"),
                Tile::Visited => print!("X"),
            }
        }
        println!();
    }
    println!();
}
