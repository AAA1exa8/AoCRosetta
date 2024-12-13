use std::{
    cmp::Reverse,
    collections::{BinaryHeap, HashSet},
};

#[derive(Debug)]
struct Button {
    x: usize,
    y: usize,
}

impl Button {
    fn from_str(s: &str) -> Button {
        let mut parts = s.split(": ");
        parts.next();
        let coords = parts.next().unwrap().split(", ").collect::<Vec<&str>>();
        let x = coords[0].split("+").collect::<Vec<&str>>()[1]
            .parse()
            .unwrap();
        let y = coords[1].split("+").collect::<Vec<&str>>()[1]
            .parse()
            .unwrap();

        Button { x, y }
    }
}

#[derive(Debug)]
struct Prize {
    x: usize,
    y: usize,
}

impl Prize {
    fn from_str(s: &str) -> Prize {
        let mut parts = s.split(": ");
        parts.next();
        let coords = parts.next().unwrap().split(", ").collect::<Vec<&str>>();
        let x = coords[0].split("=").collect::<Vec<&str>>()[1]
            .parse()
            .unwrap();
        let y = coords[1].split("=").collect::<Vec<&str>>()[1]
            .parse()
            .unwrap();

        Prize { x, y }
    }
}

#[derive(Debug)]
struct Game {
    button_a: Button,
    button_b: Button,
    prize: Prize,
}

#[derive(Debug, Eq, PartialEq, PartialOrd)]
struct State {
    x: usize,
    y: usize,
    cost: usize,
    step_a: usize,
    step_b: usize,
}

impl Ord for State {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        other.cost.cmp(&self.cost)
    }
}

impl Game {
    fn dijkstra(&self) -> usize {
        let mut queue = BinaryHeap::new();
        let mut visited = HashSet::new();

        queue.push(Reverse(State {
            x: 0,
            y: 0,
            cost: 0,
            step_a: 0,
            step_b: 0,
        }));

        while let Some(Reverse(State {
            x,
            y,
            cost,
            step_a,
            step_b,
        })) = queue.pop()
        {
            if (x, y) == (self.prize.x, self.prize.y) {
                return cost;
            }

            if visited.contains(&(x, y)) {
                continue;
            }

            visited.insert((x, y));

            if x > self.prize.x || y > self.prize.y {
                continue;
            }
            if step_a <= 100 {
                queue.push(Reverse(State {
                    x: x + self.button_a.x,
                    y: y + self.button_a.y,
                    cost: cost + 3,
                    step_a: step_a + 1,
                    step_b,
                }));
            }

            if step_b <= 100 {
                queue.push(Reverse(State {
                    x: x + self.button_b.x,
                    y: y + self.button_b.y,
                    cost: cost + 1,
                    step_a,
                    step_b: step_b + 1,
                }));
            }
        }
        0
    }
}

fn main() {
    let input = "../input.txt";
    let input_file = std::fs::read_to_string(input).expect("Error reading input file");
    let games: Vec<Game> = input_file
        .split("\n\n")
        .map(|game| {
            let mut lines = game.lines();
            let button_a = Button::from_str(lines.next().unwrap());
            let button_b = Button::from_str(lines.next().unwrap());
            let prize = Prize::from_str(lines.next().unwrap());
            Game {
                button_a,
                button_b,
                prize,
            }
        })
        .collect();
    let mut acc = 0;
    for game in games {
        acc += game.dijkstra();
    }
    println!("{}", acc);
}
