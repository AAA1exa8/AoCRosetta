use std::cmp::{max, min};

fn main() {
    let input = "../../input.txt";
    let input_file = std::fs::read_to_string(input).expect("Error reading input file");
    let time = input_file
        .lines()
        .next()
        .unwrap()
        .replace(' ', "")
        .split(':')
        .skip(1)
        .map(|c| c.parse::<u64>().unwrap())
        .next()
        .unwrap();
    let distance = input_file
        .lines()
        .nth(1)
        .unwrap()
        .replace(' ', "")
        .split(':')
        .skip(1)
        .map(|c| c.parse::<u64>().unwrap())
        .next()
        .unwrap();
    let disc: f64 = ((time*time) as f64 - 4.0*(distance as f64)).sqrt();
    let output: u64 = (((time as f64 + disc) / 2.0).floor() as u64) - (((time as f64 - disc) / 2.0).ceil() as u64) + 1;
    println!("{}", output);
}
