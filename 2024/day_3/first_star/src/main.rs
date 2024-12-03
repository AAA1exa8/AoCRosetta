use regex;

fn main() {
    let input = "../input.txt";
    let input_file = std::fs::read_to_string(input).expect("Error reading input file");
    let regex = regex::Regex::new(r"mul\((\d+),(\d+)\)").unwrap();

    let mut result = 0;
    for cap in regex.captures_iter(&input_file) {
        let a = cap[1].parse::<i64>().unwrap();
        let b = cap[2].parse::<i64>().unwrap();
        result += a * b;
    }
    println!("{}", result);
}
