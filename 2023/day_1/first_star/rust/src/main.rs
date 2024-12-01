fn main() {
    let input = "../../input.txt";
    let input_file = std::fs::read_to_string(input).expect("Error reading input file");
    let input_lines: Vec<&str> = input_file.lines().collect();
    let output = input_lines.iter().map(|line| {
        let b = line.chars().filter(|c| c.is_numeric()).collect::<Vec<char>>();
        let c = b.iter().map(|c| c.to_digit(10).unwrap() as usize).collect::<Vec<usize>>();
        return c[0]*10 + c[c.len() - 1]
    }).sum::<usize>();
    println!("{}", output);
}
