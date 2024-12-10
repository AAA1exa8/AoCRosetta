use std::collections::{HashSet, VecDeque};

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
    let mut height_nines = HashSet::new();
    for r in 0..rows {
        for c in 0..cols {
            if input[r][c] == 9 {
                height_nines.insert((r, c));
            }
        }
    }
    let directions = [(-1isize, 0isize), (1, 0), (0, -1), (0, 1)];
    let mut total_score = 0;
    for &(trail_r, trail_c) in &trailheads {
        let mut visited = vec![vec![false; cols]; rows];
        let mut queue = VecDeque::new();
        visited[trail_r][trail_c] = true;
        queue.push_back((trail_r, trail_c, 0));
        let mut reachable_nines = HashSet::new();
        while let Some((r, c, h)) = queue.pop_front() {
            if h == 9 {
                reachable_nines.insert((r, c));
                continue;
            }
            for &(dr, dc) in &directions {
                let new_r = r as isize + dr;
                let new_c = c as isize + dc;
                if new_r < 0 || new_r >= rows as isize || new_c < 0 || new_c >= cols as isize {
                    continue;
                }
                let new_r = new_r as usize;
                let new_c = new_c as usize;
                let new_height = input[new_r][new_c];
                if new_height == h + 1 && !visited[new_r][new_c] {
                    visited[new_r][new_c] = true;
                    queue.push_back((new_r, new_c, new_height));
                }
            }
        }
        total_score += reachable_nines.len();
    }
    println!("{}", total_score);
}

