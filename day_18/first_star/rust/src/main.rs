fn main() {
    let input = "../../test.txt";
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
    holes.sort_by_key(|(x,y)| *x);
    let mut count = holes.len() as i32;
    let max_x = holes.iter().max_by_key(|(x,y)| x).unwrap().0;
    let max_y = holes.iter().max_by_key(|(x,y)| y).unwrap().1;
    println!("{:?}", holes);
    for y in (0..=max_y).rev() {
        for x in 0..=max_x {
            if holes.contains(&(x, y)) {
                continue;
            }
            println!("{}, {}", x, y);
            if point_is_contained((x, y), &holes) {
                count += 1;
            }
        }
        println!();
    }
    println!("{}", count);
}

fn point_is_contained(point: (i32, i32), holes: &Vec<(i32, i32)>) -> bool {
    let border = holes.iter().filter(|(x,y)| point.1 == *y).collect::<Vec<_>>();
    println!("{:?}", border);
    let border = border.iter().enumerate().filter(|(i, (x,y))| {
        if *i == 0 {
            return true;
        }
        if x - border[i - 1].0 == 1 {
            return false;
        }
        true
    })
    .map(|(i, (x, y))| (x, y))
    .filter(|(x, y)| point.0 < **x)
    .count();
    println!("{:?}", border);
    return border % 2 == 1;

}