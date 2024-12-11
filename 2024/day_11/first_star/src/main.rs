use std::usize;

fn main() {
    let input = "../input.txt";
    let input_file = std::fs::read_to_string(input).expect("Error reading input file");
    let mut stones = input_file
        .split_whitespace()
        .map(|s| s.parse::<usize>().unwrap())
        .collect::<Vec<usize>>();
    let mut new_stones = Vec::new();
    for _ in 0..25 {
        for &stone in &stones {
            if stone == 0 {
                new_stones.push(1);
            } else if stone.ilog10() % 2 == 1 {
                let digits = stone.ilog10() + 1;
                let half = digits / 2;
                let left = stone / 10usize.pow(half as u32);
                let right = stone % 10usize.pow(half as u32);
                new_stones.push(left);
                new_stones.push(right);
            } else {
                new_stones.push(stone * 2024);
            }
        }
        stones = new_stones.clone();
        new_stones.clear();
    }
    println!("{}", stones.len());
}
