#[derive(Debug)]
struct Button {
    x: isize,
    y: isize,
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
    x: isize,
    y: isize,
}

impl Prize {
    fn from_str(s: &str) -> Prize {
        let mut parts = s.split(": ");
        parts.next();
        let coords = parts.next().unwrap().split(", ").collect::<Vec<&str>>();
        let x = coords[0].split("=").collect::<Vec<&str>>()[1]
            .parse::<isize>()
            .unwrap()
            + 10000000000000;
        let y = coords[1].split("=").collect::<Vec<&str>>()[1]
            .parse::<isize>()
            .unwrap()
            + 10000000000000;

        Prize { x, y }
    }
}

#[derive(Debug)]
struct Game {
    button_a: Button,
    button_b: Button,
    prize: Prize,
}

impl Game {
    fn solve(&self) -> isize {
        let b = (self.prize.y * self.button_a.x - self.prize.x * self.button_a.y)
            / (self.button_b.y * self.button_a.x - self.button_b.x * self.button_a.y);
        let a = (self.prize.x - b * self.button_b.x) / self.button_a.x;
        if (
            self.button_a.x * a + self.button_b.x * b,
            self.button_a.y * a + self.button_b.y * b,
        ) != (self.prize.x, self.prize.y)
        {
            0
        } else {
            a * 3 + b
        }
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
        acc += game.solve();
    }
    println!("{}", acc);
}
