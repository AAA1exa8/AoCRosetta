fn main() {
    let input = "../../input.txt";
    let input_file = std::fs::read_to_string(input).expect("Error reading input file");
    let output = input_file
        .trim()
        .split(',')
        .map(|s| {
            s.chars()
                .map(|c| c as i32)
                .fold(0, |acc, val| ((acc + val) * 17) % 256)
        })
        //.collect::<Vec<_>>();
        .sum::<i32>();
    println!("{:?}", output);
}
