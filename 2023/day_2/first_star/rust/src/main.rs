fn main() {
    let input = "../../input.txt";
    let input_file = std::fs::read_to_string(input).expect("Error reading input file");
    let output: u32 = input_file
        .lines()
        .map(|line| parse_line(line))
        .filter(|game| {
            !game.rounds.iter().any(|round| {
                if let Some(red) = round.red {
                    if red > 12 {
                        return true;
                    }
                }
                if let Some(blue) = round.blue {
                    if blue > 14 {
                        return true;
                    }
                }
                if let Some(green) = round.green {
                    if green > 13 {
                        return true;
                    }
                }
                return false;
            })
        })
        .map(|game| game.id)
        .sum();
    println!("{}", output);
}

fn parse_line(line: &str) -> Game {
    let mut game = Game {
        id: 0,
        rounds: Vec::new(),
    };

    let (id, rounds) = line.split_once(":").unwrap();
    game.id = id.split_once(" ").unwrap().1.parse().unwrap();
    let rounds = rounds.split(";").collect::<Vec<&str>>();
    for round in rounds {
        let round = round.split(',').collect::<Vec<&str>>();
        let mut r = Round {
            red: None,
            blue: None,
            green: None,
        };
        for color in round {
            let (number, color) = color.trim().split_once(" ").unwrap();
            match color.trim() {
                "red" => r.red = Some(number.parse().unwrap()),
                "blue" => r.blue = Some(number.parse().unwrap()),
                "green" => r.green = Some(number.parse().unwrap()),
                x => panic!("Invalid color: {}", x),
            }
        }
        game.rounds.push(r);
    }
    game
}

struct Game {
    id: u32,
    rounds: Vec<Round>,
}

struct Round {
    red: Option<u8>,
    blue: Option<u8>,
    green: Option<u8>,
}
