#[derive(Clone)]
struct Lens {
    label: String,
    focal_length: i32,
}

#[derive(Clone)]
struct Box {
    number: i32,
    lenses: Vec<Lens>,
}

fn main() {
    let input = "../../input.txt";
    let input_file = std::fs::read_to_string(input).expect("Error reading input file");
    let operations = input_file
        .trim()
        .split(',')
        .map(|s| {
            let label = s.chars().take_while(|&c| c != '=' && c != '-').collect::<String>();
            let op = s.chars().nth(label.len()).unwrap();
            let focal_length = if op == '=' {
                s.chars().skip(label.len() + 1).collect::<String>().parse::<i32>().unwrap()
            } else {
                0
            };
            (label, op, focal_length)
        })
        .collect::<Vec<_>>();
    
    let mut boxes = Vec::new();
    for i in 0..256 {
        boxes.push(Box { number: i, lenses: Vec::new() });
    }
    
    for (label, op, focal_length) in operations {
        let box_number = label.chars()
            .map(|c| c as i32)
            .fold(0, |acc, val| ((acc + val) * 17) % 256);
        let bbox = &mut boxes[box_number as usize];
        
        match op {
            '=' => {
                if let Some(lens) = bbox.lenses.iter_mut().find(|lens| lens.label == label) {
                    lens.focal_length = focal_length;
                } else {
                    bbox.lenses.push(Lens { label: label.clone(), focal_length });
                }
            }
            '-' => {
                bbox.lenses.retain(|lens| lens.label != label);
            }
            _ => panic!("Invalid operation"),
        }
    }
    
    let total_focusing_power: i32 = boxes.iter()
        .flat_map(|bbox| bbox.lenses.iter().enumerate().map(move |(i, lens)| (bbox.number + 1) * (i as i32 + 1) * lens.focal_length))
        .sum();
    
    println!("{:?}", total_focusing_power);
}