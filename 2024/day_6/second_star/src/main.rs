use std::collections::HashSet;

#[derive(Eq, PartialEq, Debug, Copy, Clone)]
enum Tile {
    Empty,
    Building,
    Visited,
}

#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
enum Direction {
    Up,
    Down,
    Left,
    Right,
}

#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
struct Position {
    x: usize,
    y: usize,
    direction: Direction,
}

fn main() {
    let input = "../input.txt";
    let input_file = std::fs::read_to_string(input).expect("Error reading input file");
    let mut guard = (0, 0);
    let original_map = input_file
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
    let starting_position = Position {
        x: guard.0,
        y: guard.1,
        direction: Direction::Up,
    };
    let mut position = starting_position;
    let mut map = original_map.clone();
    let mut distinct = 0;
    for y_c in 0..map.len() {
        'xloop: for x_c in 0..map[0].len() {
            map = original_map.clone();
            position = starting_position;
            map[y_c][x_c] = Tile::Building;
            if y_c == starting_position.y && x_c == starting_position.x {
                continue 'xloop;
            }
            let mut visited = HashSet::new();
            'inner: loop {
                if visited.contains(&position) {
                    distinct += 1;
                    break 'inner;
                }
                match position.direction {
                    Direction::Up => {
                        if position.y == 0 {
                            break;
                        } else if Tile::Empty == map[position.y - 1][position.x]
                            || Tile::Visited == map[position.y - 1][position.x]
                        {
                            visited.insert(position);
                            position.y -= 1;
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
                            visited.insert(position);
                            position.y += 1;
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
                            visited.insert(position);
                            position.x -= 1;
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
                            visited.insert(position);
                            position.x += 1;
                        } else {
                            position.direction = Direction::Down;
                        }
                    }
                }
            }
        }
    }
    println!("{}", distinct);
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
