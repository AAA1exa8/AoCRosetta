fn main() {
    let input = "../../input.txt";
    let input_file = std::fs::read_to_string(input).expect("Error reading input file");
    let instructions: Vec<(char, i32)> = input_file
        .lines()
        .map(|line| {
            let (action, value) = line.split_once(' ').unwrap();
            (action.chars().next().unwrap(), value.split_once(' ').unwrap().0.parse::<i32>().unwrap())
        })
        .collect();
    let mut holes = vec![(0,0)];
    for instruction in instructions {
        match instruction.0 {
            'L' => {
                for _ in 0..instruction.1 {
                    holes.push((holes.last().unwrap().0 - 1, holes.last().unwrap().1));
                }
            },
            'R' => {
                for _ in 0..instruction.1 {
                    holes.push((holes.last().unwrap().0 + 1, holes.last().unwrap().1));
                }
            },
            'U' => {
                for _ in 0..instruction.1 {
                    holes.push((holes.last().unwrap().0, holes.last().unwrap().1 - 1));
                }
            },
            'D' => {
                for _ in 0..instruction.1 {
                    holes.push((holes.last().unwrap().0, holes.last().unwrap().1 + 1));
                }
            },
            _ => panic!("Unknown instruction")
        }
    }
    holes.pop();
    let min_y = holes.iter().min_by_key(|(_,y)| y).unwrap().1;
    holes.iter_mut().for_each(|(_,y)| *y -= min_y);
    let count = shoelace(&holes);
    println!("{}", count - (holes.len() as i64 / 2) + 1 + holes.len() as i64);
}

fn shoelace(vertices: &[(i32, i32)]) -> i64 {
    let mut sum = 0;
    for i in 0..vertices.len() {
        let (x1, y1) = vertices[i];
        let (x2, y2) = vertices[(i + 1) % vertices.len()];
        sum += (x1 as i64) * (y2 as i64) - (y1 as i64) * (x2 as i64);
    }
    sum / 2
}