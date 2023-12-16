use std::collections::HashSet;
use Direction::*;

fn main() {
    let input: &str = "../../input.txt";
    let input_file = std::fs::read_to_string(input).expect("Error reading input file");
    let lines = input_file.lines().map(|s| s.to_string()).collect::<Vec<_>>();

    let initial_beam = (0, 0, Direction::Right);
    let grid: Vec<Vec<char>> = lines.iter().map(|s| s.chars().collect()).collect();

    let output = energize(initial_beam, grid);
    println!("{}", output);
}

fn energize(initial_beam: (usize, usize, Direction), grid: Vec<Vec<char>>) -> usize {
    let mut visited: HashSet<(usize, usize, Direction)> = HashSet::new();
    let mut queue: Vec<(usize, usize, Direction)> = Vec::new();
    queue.push(initial_beam);
    let mut energized: HashSet<(usize, usize)> = HashSet::new();

    let width = grid[0].len() - 1;
    let height = grid.len() - 1;
    while !queue.is_empty() {
        let (x, y, beam_direction) = queue.remove(0);
        if !visited.insert((x, y, beam_direction)) {
            continue;
        }
        energized.insert((x, y));
        let current_cell = grid[y][x];

        match (beam_direction, current_cell) {
            (Up, '/') => if x < width { queue.push((x + 1, y, Right)) },
            (Down, '/') => if x > 0 { queue.push((x - 1, y, Left)) },
            (Left, '/') => if y < height { queue.push((x, y + 1, Down)) },
            (Right, '/') => if y > 0 { queue.push((x, y - 1, Up)) },
            (Up, '\\') => if x > 0 { queue.push((x - 1, y, Left)) },
            (Down, '\\') => if x < width { queue.push((x + 1, y, Right)) },
            (Left, '\\') => if y > 0 { queue.push((x, y - 1, Up)) },
            (Right, '\\') => if y < height { queue.push((x, y + 1, Down)) },
            (Up, '|') => if y > 0 { queue.push((x, y - 1, Up)) },
            (Down, '|') => if y < height { queue.push((x, y + 1, Down)) },
            (Left, '|') => {
                if y > 0 { queue.push((x, y - 1, Up)) };
                if y < height { queue.push((x, y + 1, Down)) };
            },
            (Right, '|') => {
                if y > 0 { queue.push((x, y - 1, Up)) };
                if y < height { queue.push((x, y + 1, Down)) };
            },
            (Up, '-') => {
                if x > 0 { queue.push((x - 1, y, Left)) };
                if x < width { queue.push((x + 1, y, Right)) };
            },
            (Down, '-') => {
                if x > 0 { queue.push((x - 1, y, Left)) };
                if x < width { queue.push((x + 1, y, Right)) };
            },
            (Left, '-') => if x > 0 { queue.push((x - 1, y, Left)) },
            (Right, '-') => if x < width { queue.push((x + 1, y, Right)) },
            (Up, '.') => if y > 0 { queue.push((x, y - 1, Up)) },
            (Down, '.') => if y < height { queue.push((x, y + 1, Down)) },
            (Left, '.') => if x > 0 { queue.push((x - 1, y, Left)) },
            (Right, '.') => if x < width { queue.push((x + 1, y, Right)) },

            _ => panic!("Invalid beam direction: {:?} and/or cell: {}", beam_direction, current_cell),
        }
    }
    energized.len()
}

#[derive(Debug, PartialEq, Eq, Hash, Clone, Copy)]
enum Direction {
    Up,
    Down,
    Left,
    Right,
}