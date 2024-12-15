use std::collections::VecDeque;

#[derive(Debug, Eq, PartialEq, PartialOrd, Ord, Clone, Copy)]
struct Point {
    x: i32,
    y: i32,
}

impl Point {
    fn move_point(&mut self, direction: &Direction) {
        match direction {
            Direction::Up => self.y -= 1,
            Direction::Down => self.y += 1,
            Direction::Left => self.x -= 1,
            Direction::Right => self.x += 1,
        }
    }
}

#[derive(Debug)]
struct Game {
    walls: Vec<Point>,
    packages: Vec<Point>,
    player: Point,
    moves: VecDeque<Direction>,
}

impl Game {
    fn move_direction(&mut self) -> Direction {
        let direction = self.moves.pop_front().unwrap();
        let can_move = {
            let mut position = self.player;
            loop {
                position.move_point(&direction);
                if let Ok(_) = self.walls.binary_search(&position) {
                    break false;
                }
                if let Ok(_) = self.packages.binary_search(&position) {
                    continue;
                }
                break true;
            }
        };
        if !can_move {
            return direction;
        }
        let mut position = self.player;
        self.player.move_point(&direction);
        let mut packages_to_move = vec![];
        loop {
            position.move_point(&direction);
            if let Ok(i) = self.packages.binary_search(&position) {
                packages_to_move.push(i);
            } else {
                break;
            }
        }
        for i in packages_to_move {
            self.packages[i].move_point(&direction);
        }
        self.packages.sort();
        direction
    }

    fn calculate_score(&self) -> i32 {
        let mut score = 0;
        for package in &self.packages {
            score += package.x + 100 * package.y;
        }
        score
    }

    #[allow(dead_code)]
    fn build_map(&self) -> String {
        let mut map = String::new();
        let width = self.walls.iter().map(|p| p.x).max().unwrap() as usize + 1;
        let height = self.walls.iter().map(|p| p.y).max().unwrap() as usize + 1;
        for y in 0..height {
            for x in 0..width {
                let point = Point {
                    x: x as i32,
                    y: y as i32,
                };
                if self.walls.binary_search(&point).is_ok() {
                    map.push('#');
                } else if self.packages.binary_search(&point).is_ok() {
                    map.push('O');
                } else if self.player == point {
                    map.push('@');
                } else {
                    map.push('.');
                }
            }
            map.push('\n');
        }
        map
    }
}

#[derive(Debug)]
enum Direction {
    Up,
    Down,
    Left,
    Right,
}

fn main() {
    let input = "../input.txt";
    let input_file = std::fs::read_to_string(input).expect("Error reading input file");
    let mut game = parse_input(&input_file);
    for _ in 0..game.moves.len() {
        let _ = game.move_direction();
    }
    println!("{}", game.calculate_score());
}

fn parse_input(input: &str) -> Game {
    let (map, moves) = input.split_once("\n\n").unwrap();
    let width = map.lines().next().unwrap().len();
    let mut walls = map
        .chars()
        .filter(|c| *c != '\n')
        .enumerate()
        .filter_map(|(i, c)| {
            if c == '#' {
                Some(Point {
                    x: (i % width) as i32,
                    y: (i / width) as i32,
                })
            } else {
                None
            }
        })
        .collect::<Vec<_>>();
    let mut packages = map
        .chars()
        .filter(|c| *c != '\n')
        .enumerate()
        .filter_map(|(i, c)| {
            if c == 'O' {
                Some(Point {
                    x: (i % width) as i32,
                    y: (i / width) as i32,
                })
            } else {
                None
            }
        })
        .collect::<Vec<_>>();
    let player = map
        .chars()
        .filter(|c| *c != '\n')
        .enumerate()
        .find_map(|(i, c)| {
            if c == '@' {
                Some(Point {
                    x: (i % width) as i32,
                    y: (i / width) as i32,
                })
            } else {
                None
            }
        })
        .unwrap();
    let moves = moves
        .chars()
        .filter_map(|c| match c {
            '<' => Some(Direction::Left),
            '>' => Some(Direction::Right),
            '^' => Some(Direction::Up),
            'v' => Some(Direction::Down),
            _ => None,
        })
        .collect::<VecDeque<_>>();
    walls.sort();
    packages.sort();
    Game {
        walls,
        packages,
        player,
        moves,
    }
}
