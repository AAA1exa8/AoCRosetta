use rayon::prelude::*;
use std::collections::HashSet;

#[derive(Eq, PartialEq, Debug, Copy, Clone)]
enum Tile {
    Empty,
    Building,
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
                        Tile::Empty
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
    let simulate_movement = |x_c: usize, y_c: usize| -> bool {
        let mut map_clone = original_map.clone();
        let mut position = starting_position.clone();

        if y_c == starting_position.y && x_c == starting_position.x {
            return false;
        }
        map_clone[y_c][x_c] = Tile::Building;

        let mut visited = HashSet::new();

        loop {
            if visited.contains(&position) {
                return true;
            }
            visited.insert(position.clone());

            match position.direction {
                Direction::Up => {
                    if position.y == 0 {
                        return false;
                    } else if map_clone[position.y - 1][position.x] != Tile::Empty {
                        position.direction = Direction::Right;
                    } else {
                        position.y -= 1;
                    }
                }
                Direction::Down => {
                    if position.y == map_clone.len() - 1 {
                        return false;
                    } else if map_clone[position.y + 1][position.x] != Tile::Empty {
                        position.direction = Direction::Left;
                    } else {
                        position.y += 1;
                    }
                }
                Direction::Left => {
                    if position.x == 0 {
                        return false;
                    } else if map_clone[position.y][position.x - 1] != Tile::Empty {
                        position.direction = Direction::Up;
                    } else {
                        position.x -= 1;
                    }
                }
                Direction::Right => {
                    if position.x == map_clone[0].len() - 1 {
                        return false;
                    } else if map_clone[position.y][position.x + 1] != Tile::Empty {
                        position.direction = Direction::Down;
                    } else {
                        position.x += 1;
                    }
                }
            }
        }
    };

    let distinct = (0..original_map.len())
        .into_par_iter()
        .map(|y_c| {
            (0..original_map[0].len())
                .into_par_iter()
                .filter(|&x_c| simulate_movement(x_c, y_c))
                .count()
        })
        .sum::<usize>();
    println!("{}", distinct);
}
