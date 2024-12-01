fn main() {
    let input = "../../input.txt";
    let input_file = std::fs::read_to_string(input).expect("Error reading input file");
    let lines = input_file.lines();
    let output = lines.map(|line| get_numbers(line)).sum::<usize>();
    println!("{}", output);
}

fn get_numbers(line: &str) -> usize {
    let nums = [
        "one", "two", "three", "four", "five", "six", "seven", "eight", "nine",
    ];
    let mut first = 0;
    let mut last = 0;
    for (i, ch) in line.chars().enumerate() {
        let mut cur: Option<u32> = None;
        if ch.is_numeric() {
            cur = ch.to_digit(10);
        } else {
            for (j, num) in nums.iter().enumerate() {
                if line[i..].starts_with(num) {
                    cur = Some(j as u32 + 1);
                    break;
                }
            }
        }
        if let Some(cur) = cur {
            if first == 0 {
                first = cur;
                last = cur;
            } else {
                last = cur;
            }
        }
    }
    assert_ne!(first, 0);
    assert_ne!(last, 0);
    return first as usize * 10 + last as usize;
}
