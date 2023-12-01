use std::{fs::File, io::{BufRead, BufReader}, str::FromStr};

fn main() {
    let input = "../../input.txt";
    let file = File::open(input).expect("Unable to open file");
    let reader = BufReader::new(file);

    let mut calories_per_elf = Vec::new();
    let mut current_calories = 0;

    for line in reader.lines() {
        let line = line.expect("Unable to read line");
        if line.is_empty() {
            calories_per_elf.push(current_calories);
            current_calories = 0;
        } else {
            current_calories += i32::from_str(&line).expect("Unable to parse number");
        }
    }

    calories_per_elf.push(current_calories);

    calories_per_elf.sort_unstable_by(|a, b| b.cmp(a));

    let top_three_calories: i32 = calories_per_elf.iter().take(3).sum();

    println!("{}", top_three_calories);
}