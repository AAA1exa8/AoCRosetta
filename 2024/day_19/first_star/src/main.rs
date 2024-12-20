fn main() {
    let input = "../input.txt";
    let input_file = std::fs::read_to_string(input).expect("Error reading input file");

    let (pattern_part, design_part) = input_file.split_once("\n\n").unwrap();
    let patterns: Vec<&str> = pattern_part.split(',').map(|s| s.trim()).collect();

    let designs: Vec<&str> = design_part.lines().collect();

    let possible_count = designs.iter().filter(|d| can_build(d, &patterns)).count();

    println!("{}", possible_count);
}

fn can_build(design: &str, patterns: &[&str]) -> bool {
    let n = design.len();
    let mut dp = vec![false; n + 1];
    dp[0] = true;

    for i in 1..=n {
        for pattern in patterns.iter() {
            let plen = pattern.len();
            if i >= plen {
                let slice = &design[i - plen..i];
                if slice == *pattern && dp[i - plen] {
                    dp[i] = true;
                    break;
                }
            }
        }
    }

    dp[n]
}
