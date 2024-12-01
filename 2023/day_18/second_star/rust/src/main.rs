fn main() {
    let input = "../../test.txt";
    let input_file = std::fs::read_to_string(input).expect("Error reading input file");
    let instructions: Vec<(&str, i32)> = input_file
        .lines()
        .map(|line| {
            let (_, value) = line.split_once('#').unwrap();
            let value = value.trim_end_matches(')');
            let (val, action) = value.split_at(5);
            let val = i32::from_str_radix(val, 16).unwrap();
            (action, val)
        })
        .collect();
    let mut holes = vec![(0,0)];
    for instruction in instructions {
        match instruction.0 {
            "2" => {
                for _ in 0..instruction.1 {
                    holes.push((holes.last().unwrap().0 - 1, holes.last().unwrap().1));
                }
            },
            "0" => {
                for _ in 0..instruction.1 {
                    holes.push((holes.last().unwrap().0 + 1, holes.last().unwrap().1));
                }
            },
            "3" => {
                for _ in 0..instruction.1 {
                    holes.push((holes.last().unwrap().0, holes.last().unwrap().1 - 1));
                }
            },
            "1" => {
                for _ in 0..instruction.1 {
                    holes.push((holes.last().unwrap().0, holes.last().unwrap().1 + 1));
                }
            },
            x => panic!("Unknown instruction {}", x)
        }
    }
    holes.pop();
    let min_y = holes.iter().min_by_key(|(_,y)| y).unwrap().1;
    holes.iter_mut().for_each(|(_,y)| *y -= min_y);
    let count = shoelace_area(&holes);
    println!("{}", count - (holes.len() as i64 / 2) + 1 + holes.len() as i64);
}

fn shoelace_area(vertices: &[(i32, i32)]) -> i64 {
    let mut sum = 0;
    for i in 0..vertices.len() {
        let (x1, y1) = vertices[i];
        let (x2, y2) = vertices[(i + 1) % vertices.len()];
        sum += (x1 as i64) * (y2 as i64) - (y1 as i64) * (x2 as i64);
    }
    sum / 2
}