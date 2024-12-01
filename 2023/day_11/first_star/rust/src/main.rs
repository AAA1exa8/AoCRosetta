use std::vec;

fn main() {
    let input = "../../input.txt";
    let input_file = std::fs::read_to_string(input).expect("Error reading input file");
    let mut galaxies = input_file
        .lines()
        .map(|f| f.chars().collect::<Vec<_>>())
        .collect::<Vec<_>>();
    expand(&mut galaxies);
    let galaxies = galaxies
        .iter()
        .enumerate()
        .flat_map(|(i, g)| {
            g.iter()
                .enumerate()
                .filter(|(_, c)| **c == '#')
                .map(|(j, c)| {
                    if *c == '#' {
                        (j as i32, i as i32)
                    } else {
                        (-1, -1)
                    }
                })
                .collect::<Vec<_>>()
        })
        .collect::<Vec<_>>();
        let mut output = 0;
        for i in 0..galaxies.len() {
            for j in (i + 1)..galaxies.len() {
                output += {
                    let (x1, y1) = galaxies[i];
                    let (x2, y2) = galaxies[j];
                    (x2 - x1).abs() + y2 - y1
                };
            }
        }
        println!("{}", output);
}

fn expand(galaxies: &mut Vec<Vec<char>>) {
    let mut empty = Vec::new();
    galaxies.iter().enumerate().for_each(|g| {
        if g.1.iter().all(|c| *c == '.') {
            empty.push(g.0);
        }
    });
    let mut modifier = 0;
    empty.iter().for_each(|e| {
        galaxies.insert(e + modifier, vec!['.'; galaxies[0].len()]);
        modifier += 1;
    });
    let mut empty = Vec::new();
    for i in 0..(galaxies[0].len() - 1) {
        if galaxies.iter().all(|g| g[i] == '.') {
            empty.push(i);
        }
    }
    let mut modifier = 0;
    empty.iter().for_each(|e| {
        galaxies.iter_mut().for_each(|g| {
            g.insert(*e + modifier, '.');
        });
        modifier += 1;
    });
}
