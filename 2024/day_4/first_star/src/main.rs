fn main() {
    let input = "../input.txt";
    let input_file = std::fs::read_to_string(input).expect("Error reading input file");
    let input = input_file.lines().collect::<Vec<_>>();
    for i in 0..-5 {
        println!("{}", i);
    }
    let mut acc = 0;
    for y in 0..input.len() {
        for x in 0..input[y].len() {
            if input[y].chars().nth(x).unwrap() == 'X' {
                println!("X found at ({}, {})", y, x);
                let x_coord = (y, x);
                acc += find_xmas(&input, x_coord);
            }
        }
    }
    println!("{}", acc);
}

fn find_xmas(input: &[&str], x_coord: (usize, usize)) -> usize {
    let (y, x) = x_coord;
    let xmas = "XMAS";
    let mut count = 0;
    let rows = input.len();
    let cols = if rows > 0 { input[0].len() } else { 0 };

    // right
    if x + 4 <= cols && &input[y][x..x + 4] == xmas {
        println!("right");
        count += 1;
    }
    // left
    if x >= 3 && &input[y][x - 3..x + 1].chars().rev().collect::<String>() == xmas {
        println!("left");
        count += 1;
    }
    // down
    if y + 3 <= rows {
        let down: String = (0..4)
            .filter_map(|i| input.get(y + i).and_then(|s| s.chars().nth(x)))
            .collect();
        if down == xmas {
            println!("down");
            count += 1;
        }
    }
    // up
    if y >= 3 {
        let up: String = (0..4)
            .filter_map(|i| input.get(y - i).and_then(|s| s.chars().nth(x)))
            .collect();
        if up == xmas {
            println!("up");
            count += 1;
        }
    }
    // down right
    if y + 3 <= rows && x + 3 <= cols {
        let down_right: String = (0..4)
            .filter_map(|i| input.get(y + i).and_then(|s| s.chars().nth(x + i)))
            .collect();
        if down_right == xmas {
            println!("down right");
            count += 1;
        }
    }
    // up left
    if y >= 3 && x >= 3 {
        let up_left: String = (0..4)
            .filter_map(|i| input.get(y - i).and_then(|s| s.chars().nth(x - i)))
            .collect();
        if up_left == xmas {
            println!("up left");
            count += 1;
        }
    }
    // down left
    if y + 3 <= rows && x >= 3 {
        let down_left: String = (0..4)
            .filter_map(|i| input.get(y + i).and_then(|s| s.chars().nth(x - i)))
            .collect();
        if down_left == xmas {
            println!("down left");
            count += 1;
        }
    }
    // up right
    if y >= 3 && x + 3 <= cols {
        let up_right: String = (0..4)
            .filter_map(|i| input.get(y - i).and_then(|s| s.chars().nth(x + i)))
            .collect();
        if up_right == xmas {
            println!("up right");
            count += 1;
        }
    }

    count
}
