use std::vec;

fn main() {
    let input = "../../input.txt";
    let input_file = std::fs::read_to_string(input).expect("Error reading input file");
    let galaxies = input_file
        .lines()
        .map(|f| f.chars().collect::<Vec<_>>())
        .collect::<Vec<_>>();
    let mut galaxies = galaxies
        .iter()
        .enumerate()
        .flat_map(|(i, g)| {
            g.iter()
                .enumerate()
                .filter(|(_, c)| **c == '#')
                .map(|(j, c)| {
                    if *c == '#' {
                        (j as i64, i as i64)
                    } else {
                        (-1, -1)
                    }
                })
                .collect::<Vec<_>>()
        })
        .collect::<Vec<_>>();
        expand(&mut galaxies);
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

fn expand(galaxies: &mut Vec<(i64, i64)>) {
    const mul: i64 = 1000000;
    let x = galaxies.iter().map(|(x, _)| *x).collect::<Vec<_>>();
    let max_x = *x.iter().max().unwrap();
    let mut no_x = Vec::new();
    for i in 0..max_x {
        if !x.contains(&&i) {
            no_x.push(i);
        }
    }
    galaxies.iter_mut().for_each(|g| {
        let spaces = no_x.iter().filter(|x| **x < g.0).count();
        g.0 += (spaces as i64) * (mul - 1); 
    });
    let y = galaxies.iter().map(|(_, y)| *y).collect::<Vec<_>>();
    let max_y = *y.iter().max().unwrap();
    let mut no_y = Vec::new();
    for i in 0..max_y {
        if !y.contains(&&i) {
            no_y.push(i);
        }
    }
    galaxies.iter_mut().for_each(|g| {
        let spaces = no_y.iter().filter(|y| **y < g.1).count();
        g.1 += (spaces as i64) * (mul - 1); 
    });
}
