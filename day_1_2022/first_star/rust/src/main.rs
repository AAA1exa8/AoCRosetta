use std::{fs::File, io::{BufRead, BufReader}, str::FromStr};

fn main() {
    let input = "../../input.txt";
    let file = File::open(input).expect("Unable to open file");
    let reader = BufReader::new(file);

    let mut max_calories = 0;
    let mut current_calories = 0;

    for line in reader.lines() {
        let line = line.expect("Unable to read line");
        if line.is_empty() {
            max_calories = max_calories.max(current_calories);
            current_calories = 0;
        } else {
            current_calories += i32::from_str(&line).expect("Unable to parse number");
        }
    }

    max_calories = max_calories.max(current_calories);

    println!("{}", max_calories);
}