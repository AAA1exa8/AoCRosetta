use rayon::prelude::*;

#[derive(Debug, Clone, PartialEq, Eq)]
struct Robot {
    position: Coord,
    velocity: Coord,
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct Coord {
    x: isize,
    y: isize,
}

impl Coord {
    fn new_from_str(s: &str) -> Self {
        let mut parts = s.split('=').nth(1).unwrap().split(",");
        let x = parts.next().unwrap().parse().unwrap();
        let y = parts.next().unwrap().parse().unwrap();
        Self { x, y }
    }
}

const WIDTH: isize = 101;
const HEIGHT: isize = 103;

fn mod_positive(x: isize, modulus: isize) -> isize {
    ((x % modulus) + modulus) % modulus
}

fn main() {
    let input = "../input.txt";
    let input_file = std::fs::read_to_string(input).expect("Error reading input file");
    let robots = input_file
        .lines()
        .map(|line| {
            let mut parts = line.split_whitespace();
            let position = Coord::new_from_str(parts.next().unwrap());
            let velocity = Coord::new_from_str(parts.next().unwrap());
            Robot { position, velocity }
        })
        .collect::<Vec<_>>();
    let time = (0..)
        .par_bridge()
        .map(|time| {
            let mut robots = robots.clone();
            robots.iter_mut().for_each(|robot| {
                robot.position.x = mod_positive(robot.position.x + robot.velocity.x * time, WIDTH);
                robot.position.y = mod_positive(robot.position.y + robot.velocity.y * time, HEIGHT);
            });
            let no_overlap = robots.iter().all(|robot| {
                robots
                    .iter()
                    .filter(|&r| r != robot)
                    .all(|r| r.position.x != robot.position.x || r.position.y != robot.position.y)
            });
            (time, no_overlap)
        })
        .find_any(|(_, no_overlap)| *no_overlap)
        .unwrap()
        .0;
    println!("{}", time)
}
