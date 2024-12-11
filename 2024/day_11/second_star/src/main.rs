use std::collections::HashMap;

fn main() {
    let input = "../input.txt";
    let input_file = std::fs::read_to_string(input).expect("Error reading input file");
    let mut stones = input_file
        .split_whitespace()
        .map(|s| (s.parse::<usize>().unwrap(), 1))
        .collect::<HashMap<usize, u64>>();
    let mut new_stones = HashMap::new();
    for _ in 0..75 {
        for (&stone, &v) in &stones {
            if stone == 0 {
                *new_stones.entry(1).or_default() += v;
            } else if stone.ilog10() % 2 == 1 {
                let digits = stone.ilog10() + 1;
                let half = digits / 2;
                let left = stone / 10usize.pow(half as u32);
                let right = stone % 10usize.pow(half as u32);
                *new_stones.entry(left).or_default() += v;
                *new_stones.entry(right).or_default() += v;
            } else {
                *new_stones.entry(stone * 2024).or_default() += v;
            }
        }
        stones = new_stones.clone();
        new_stones.clear();
    }
    println!("{}", stones.values().sum::<u64>());
}
