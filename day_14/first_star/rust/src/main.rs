fn main() {
    let input = "../../test.txt";
    let input_file = std::fs::read_to_string(input).expect("Error reading input file");
    // let mut stones = input_file.lines().enumerate().flat_map(|(y, line)| {
    //     line.chars().enumerate().filter_map(move |(x, c)| {
    //         match c {
    //             'O' => Some(Rock { rock_type: RockType::Round, x, y }),
    //             '#' => Some(Rock { rock_type: RockType::Cube, x, y }),
    //             _ => None
    //         }
    //     })
    // }).collect::<Vec<_>>();
    let mut stones: Vec<String> = input_file.lines().map(|line| line.to_string()).collect();
    let mut rocks: Vec<Vec<Rock>> = Vec::new();
    for i in 0..stones[0].len() {
        let mut rock: Vec<Rock> = Vec::new();
        for j in 0..stones.len() {
            match stones[j].chars().nth(i).unwrap() {
                'O' => rock.push(Rock{rock_type: RockType::Round, x: i, y: j}),
                '#' => rock.push(Rock{rock_type: RockType::Cube, x: i, y: j}),
                _ => {}
            }
        }
        rocks.push(rock);
    }
    println!("{:?}", rocks);

}

#[derive(Debug)]
enum RockType {
    Round,
    Cube
}

#[derive(Debug)]
struct Rock {
    rock_type: RockType,
    x: usize,
    y: usize
}