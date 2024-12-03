use regex;

fn main() {
    let input = "../input.txt";
    let input_file = std::fs::read_to_string(input).expect("Error reading input file");
    let regex = regex::Regex::new(r"mul\((\d+),(\d+)\)|do\(\)|don't\(\)").unwrap();

    let mut result = 0;
    let mut apply = true;
    for cap in regex.captures_iter(&input_file) {
        match &cap[0] {
            "do()" => apply = true,
            "don't()" => apply = false,
            _ => {
                if apply {
                    let a = cap[1].parse::<i64>().unwrap();
                    let b = cap[2].parse::<i64>().unwrap();
                    result += a * b;
                }
            }
        }
    }
    println!("{}", result);
}
