fn main() {
    let input = "../../test.txt";
    let input_file = std::fs::read_to_string(input).expect("Error reading input file");
    let output = input_file
        .split("\n\n")
        .map(|n| {
            n.lines()
                .map(|l| {
                    l.chars()
                        .enumerate()
                        .filter(|(_, c)| *c == '#')
                        .map(move |(x, _)| x)
                        .collect::<Vec<_>>()
                })
                .collect::<Vec<_>>()
        })
        .map(|n| find_reflection(&n))
        .sum::<usize>();
    println!("{}", output);
}

fn find_reflection(notes: &[Vec<usize>]) -> usize {
    let mut prev_note = &Vec::new();
    let mut reflections = 0;
    for (y, note) in notes.iter().enumerate() {
        if y == 0 {
            prev_note = note;
            continue;
        }
        if reflection_eq(&note, &prev_note) {
            // let mut found = false;
            // for i in 0..y {
            //     if y + y - i - 1 >= notes.len() {
            //         continue;
            //     }
            //     if reflection_eq(&notes[i], &notes[y + y - i - 1]) {
            //         println!("{:?} {:?}", notes[i],notes[y + y - i - 1]);
            //     }else{
            //         println!("No reflection found at {}", i);
            //     }
            // }
            if (0..y)
                .filter(|&i| y + y - i - 1 < notes.len())
                .all(|i| reflection_eq(&notes[i], &notes[y + y - i - 1]))
            {
                reflections += y*100;
            }
        }
        prev_note = note;
    }
    reflections
}

fn reflection_eq(a: &[usize], b: &[usize]) -> bool {
    a.iter().zip(b).all(|(a, b)| a == b)
}
