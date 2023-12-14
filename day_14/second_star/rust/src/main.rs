fn main() {
    let input = "../../input.txt";
    let input_file = std::fs::read_to_string(input).expect("Error reading input file");
    let stones: Vec<String> = input_file.lines().map(|line| line.to_string()).collect();
    let rocks: Vec<Vec<Rock>> = stones[0].chars().enumerate().map(|(i, _)| {
        stones.iter().enumerate().filter_map(|(j, stone)| {
            match stone.chars().nth(i).unwrap() {
                'O' => Some(Rock{rock_type: RockType::Round, x: i, y: j}),
                '#' => Some(Rock{rock_type: RockType::Cube, x: i, y: j}),
                _ => None,
            }
        }).collect()
    }).collect();
    let dirs = [Direction::North, Direction::West, Direction::South, Direction::East];
    let rocks = shift_stones(rocks, Direction::East);
    let output = rocks.iter().map(|rock| {
        match rock.rock_type {
            RockType::Round => stones.len() - rock.y,
            RockType::Cube => 0,
            _ => panic!("Not a stone")
        }
    }).sum::<usize>();
    println!("{}", output)
}

fn shift_stones(stones: Vec<Vec<Rock>>, dir: Direction) -> Vec<Rock> {
    let mut new_stones: Vec<Rock> = Vec::new();
    for stoness in stones {
        for (i, stone) in stoness.into_iter().enumerate() {
            match stone.rock_type {
                RockType::Round if i == 0 => {
                    new_stones.push(Rock{rock_type: RockType::Round, x: stone.x, y: 0});
                },
                RockType::Cube => {
                    new_stones.push(Rock { rock_type: RockType::Cube, x: stone.x, y: stone.y});
                },
                RockType::Round => {
                    new_stones.push(Rock { rock_type: RockType::Round, x: stone.x, y: new_stones.last().unwrap().y + 1});
                }
            }
        }
    }
    new_stones
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
enum RockType {
    Round,
    Cube
}

enum Direction {
    North,
    West,
    South,
    East
}

#[derive(Debug, Clone, Copy)]
struct Rock {
    rock_type: RockType,
    x: usize,
    y: usize
}