fn main() {
    let input = "../../input.txt";
    let input_file = std::fs::read_to_string(input).expect("Error reading input file");
    let output: u64 = input_file
        .lines()
        .map(|line| parse_line(line))
        .map(|game| game.max_blue * game.max_red * game.max_green)
        .sum();
    println!("{}", output);
}

fn parse_line(line: &str) -> Game {
    let mut game = Game {
        id: 0,
        max_red: 0,
        max_blue: 0,
        max_green: 0,
    };

    let mut round_vec: Vec<Round> = Vec::new();

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
        round_vec.push(r);
    }
    game.max_blue = round_vec.iter().flat_map(|round| round.blue).max().unwrap();
    game.max_red = round_vec.iter().flat_map(|round| round.red).max().unwrap();
    game.max_green = round_vec.iter().flat_map(|round| round.green).max().unwrap();
    game
}

struct Game {
    id: u32,
    max_red: u64,
    max_blue: u64,
    max_green: u64,
}

struct Round {
    red: Option<u64>,
    blue: Option<u64>,
    green: Option<u64>,
}