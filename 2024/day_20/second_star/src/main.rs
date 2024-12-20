use std::collections::{HashMap, HashSet, VecDeque};

fn main() {
    let input = "../input.txt";
    let input_file = std::fs::read_to_string(input).expect("Error reading input file");

    let mut start = (0, 0);
    let grid: HashSet<(i32, i32)> = input_file
        .lines()
        .enumerate()
        .flat_map(|(y, line)| {
            line.chars()
                .enumerate()
                .filter_map(|(x, c)| match c {
                    '#' => None,
                    'S' => {
                        start = (x as i32, y as i32);
                        Some(start)
                    }
                    _ => Some((x as i32, y as i32)),
                })
                .collect::<Vec<_>>()
        })
        .collect();

    let mut dist_map: HashMap<(i32, i32), i32> = HashMap::new();
    let mut queue: VecDeque<(i32, i32)> = VecDeque::new();

    dist_map.insert(start, 0);
    queue.push_back(start);

    while let Some(pos) = queue.pop_front() {
        let current_dist = dist_map[&pos];

        let neighbors = [
            (pos.0, pos.1 - 1),
            (pos.0, pos.1 + 1),
            (pos.0 - 1, pos.1),
            (pos.0 + 1, pos.1),
        ];

        for &new_pos in &neighbors {
            if grid.contains(&new_pos) && !dist_map.contains_key(&new_pos) {
                dist_map.insert(new_pos, current_dist + 1);
                queue.push_back(new_pos);
            }
        }
    }

    let mut a: usize = 0;

    let positions: Vec<(i32, i32)> = dist_map.keys().map(|&k| k).collect();

    for i in 0..positions.len() {
        for j in 0..positions.len() {
            if i == j {
                continue;
            }
            let p1 = positions[i];
            let p2 = positions[j];

            let manhattan = (p1.0 - p2.0).abs() + (p1.1 - p2.1).abs();

            if manhattan < 21 {
                if (dist_map[&p1] - dist_map[&p2] - manhattan) >= 100 {
                    a += 1;
                }
            }
        }
    }

    println!("{}", a);
}
