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
            let position = self.player;
            let moved_position = {
                let mut moved_position = position;
                moved_position.move_point(&direction);
                moved_position
            };
            if let Some(_) = self.walls.iter().find(|&p| *p == moved_position) {
                false
            } else if let Some(i) = self.packages.iter().position(|&p| p == moved_position) {
                if i % 2 == 0 {
                    if direction == Direction::Left {
                        self.can_move(i, &direction)
                    } else {
                        self.can_move(i, &direction) && self.can_move(i + 1, &direction)
                    }
                } else {
                    if direction == Direction::Right {
                        self.can_move(i, &direction)
                    } else {
                        self.can_move(i, &direction) && self.can_move(i - 1, &direction)
                    }
                }
            } else {
                true
            }
        };
        if !can_move {
            return direction;
        }
        let position = self.player;
        self.player.move_point(&direction);

        if let Some(_) = self.packages.iter().position(|&p| p == self.player) {
            let mut packages_to_move = vec![];
            self.to_move(position, &direction, &mut packages_to_move);
            packages_to_move.sort();
            packages_to_move.dedup();
            for i in packages_to_move {
                self.packages[i].move_point(&direction);
            }
            self.sort_packages();
        }
        direction
    }

    fn can_move(&self, index: usize, direction: &Direction) -> bool {
        let mut position = self.packages[index];
        position.move_point(&direction);
        if let Some(_) = self.walls.iter().find(|&p| *p == position) {
            return false;
        }
        if let Some(i) = self.packages.iter().position(|&p| p == position) {
            if i % 2 == 0 {
                if direction == &Direction::Left {
                    return self.can_move(i, direction);
                } else {
                    return self.can_move(i, direction) && self.can_move(i + 1, direction);
                }
            } else {
                if direction == &Direction::Right {
                    return self.can_move(i, direction);
                } else {
                    return self.can_move(i, direction) && self.can_move(i - 1, direction);
                }
            }
        }
        true
    }

    fn to_move(
        &self,
        mut position: Point,
        direction: &Direction,
        packages_to_move: &mut Vec<usize>,
    ) {
        position.move_point(&direction);
        if let Some(i) = self.packages.iter().position(|&p| p == position) {
            if i % 2 == 0 {
                packages_to_move.push(i);
                packages_to_move.push(i + 1);
                self.to_move(position, direction, packages_to_move);
                if direction == &Direction::Left {
                    return;
                }
                let new_position = self.packages[i + 1];
                self.to_move(new_position, direction, packages_to_move);
            } else {
                packages_to_move.push(i - 1);
                packages_to_move.push(i);
                self.to_move(position, direction, packages_to_move);
                if direction == &Direction::Right {
                    return;
                }
                let new_position = self.packages[i - 1];
                self.to_move(new_position, direction, packages_to_move);
            }
        }
        return;
    }

    fn calculate_score(&self) -> i32 {
        let mut score = 0;
        for package in self.packages.chunks(2).map(|p| p[0]) {
            score += package.x + 100 * package.y;
        }
        score
    }

    #[allow(dead_code)]
    fn build_map(&self) -> String {
        let width = self.walls.iter().map(|p| p.x).max().unwrap() as usize + 1;
        let height = self.walls.iter().map(|p| p.y).max().unwrap() as usize + 1;
        let mut map = vec![vec!['.'; width]; height];
        for wall in &self.walls {
            map[wall.y as usize][wall.x as usize] = '#';
        }
        for p in self.packages.chunks(2) {
            map[p[0].y as usize][p[0].x as usize] = '[';
            map[p[1].y as usize][p[1].x as usize] = ']';
        }
        map[self.player.y as usize][self.player.x as usize] = '@';
        map.iter()
            .map(|row| row.iter().collect::<String>())
            .collect::<Vec<_>>()
            .join("\n")
    }

    fn sort_packages(&mut self) {
        let mut packages = self
            .packages
            .chunks(2)
            .map(|p| (p[0], p[1]))
            .collect::<Vec<_>>();
        packages.sort();
        let packages = packages
            .iter()
            .map(|(p1, p2)| [*p1, *p2])
            .flatten()
            .collect::<Vec<_>>();
        self.packages = packages;
    }
}

#[derive(Debug, PartialEq, Eq)]
enum Direction {
    Up,
    Down,
    Left,
    Right,
}

fn main() {
    let input = "../input.txt";
    let input_file = std::fs::read_to_string(input).expect("Error reading input file");
    let input_file = input_file.replace("#", "##");
    let input_file = input_file.replace(".", "..");
    let input_file = input_file.replace("O", "[]");
    let input_file = input_file.replace("@", "@.");
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
        .collect::<Vec<_>>()
        .chunks(2)
        .enumerate()
        .filter_map(|(i, c)| {
            let i = i * 2;
            if c == ['#', '#'] {
                Some([
                    Point {
                        x: (i % width) as i32,
                        y: (i / width) as i32,
                    },
                    Point {
                        x: (i % width) as i32 + 1,
                        y: (i / width) as i32,
                    },
                ])
            } else {
                None
            }
        })
        .flatten()
        .collect::<Vec<_>>();
    let packages = map
        .chars()
        .filter(|c| *c != '\n')
        .collect::<Vec<_>>()
        .chunks(2)
        .enumerate()
        .filter_map(|(i, c)| {
            let i = i * 2;
            if c == ['[', ']'] {
                Some([
                    Point {
                        x: (i % width) as i32,
                        y: (i / width) as i32,
                    },
                    Point {
                        x: (i % width) as i32 + 1,
                        y: (i / width) as i32,
                    },
                ])
            } else {
                None
            }
        })
        .flatten()
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
    let mut game = Game {
        walls,
        packages,
        player,
        moves,
    };
    game.sort_packages();
    game
}
