fn main() {
    let input = "../input.txt";
    let input_file = std::fs::read_to_string(input).expect("Error reading input file");
    let input: Vec<Vec<u32>> = input_file
        .lines()
        .filter(|line| !line.trim().is_empty())
        .map(|line| {
            line.chars()
                .map(|c| c.to_digit(10).expect("All characters must be digits 0-9"))
                .collect()
        })
        .collect();
    if input.is_empty() {
        println!("0");
        return;
    }
    let rows = input.len();
    let cols = input[0].len();
    let mut trailheads = Vec::new();
    for r in 0..rows {
        for c in 0..cols {
            if input[r][c] == 0 {
                trailheads.push((r, c));
            }
        }
    }
    let mut paths = vec![vec![0u64; cols]; rows];
    for r in 0..rows {
        for c in 0..cols {
            if input[r][c] == 9 {
                paths[r][c] = 1;
            }
        }
    }
    let directions = [(-1isize, 0isize), (1, 0), (0, -1), (0, 1)];
    for h in (0..9).rev() {
        for r in 0..rows {
            for c in 0..cols {
                if input[r][c] == h {
                    for &(dr, dc) in &directions {
                        let new_r = r as isize + dr;
                        let new_c = c as isize + dc;
                        if new_r < 0
                            || new_r >= rows as isize
                            || new_c < 0
                            || new_c >= cols as isize
                        {
                            continue;
                        }
                        let new_r = new_r as usize;
                        let new_c = new_c as usize;
                        if input[new_r][new_c] == h + 1 {
                            paths[r][c] += paths[new_r][new_c];
                        }
                    }
                }
            }
        }
    }
    let mut total_rating: u64 = 0;
    for &(trail_r, trail_c) in &trailheads {
        total_rating += paths[trail_r][trail_c];
    }
    println!("{}", total_rating);
}
