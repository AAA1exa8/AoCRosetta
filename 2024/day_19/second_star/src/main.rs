fn main() {
    let input = "../input.txt";
    let input_file = std::fs::read_to_string(input).expect("Error reading input file");

    let (pattern_part, design_part) = input_file.split_once("\n\n").unwrap();
    let patterns: Vec<&str> = pattern_part.split(',').map(|s| s.trim()).collect();

    let designs: Vec<&str> = design_part.lines().collect();

    let total_ways: u64 = designs
        .iter()
        .map(|&design| count_ways(design, &patterns))
        .sum();

    println!("{}", total_ways);
}

fn count_ways(design: &str, patterns: &[&str]) -> u64 {
    let n = design.len();
    let mut dp = vec![0u64; n + 1];
    dp[0] = 1;

    for i in 1..=n {
        for pattern in patterns.iter() {
            let plen = pattern.len();
            if i >= plen {
                let slice = &design[i - plen..i];
                if slice == *pattern {
                    dp[i] += dp[i - plen];
                }
            }
        }
    }

    dp[n]
}
