fn main() {
    let input = "../../input.txt";
    let input_file = std::fs::read_to_string(input).expect("Error reading input file");
    let output = input_file
        .split("\n\n")
        .map(|n| n.lines().map(|s| s.to_string()).collect::<Vec<_>>())
        .map(|mut n| {
            let original_reflection = find_reflection(&n, 0);
            for y in 0..n.len() {
                for x in 0..n[y].len() {
                    fix_smudge(x, y, &mut n);
                    let reflections = find_reflection(&n, original_reflection);
                    if reflections != 0 && reflections != original_reflection {
                        return reflections;
                    }
                    fix_smudge(x, y, &mut n);
                }
            }
            panic!("No reflection found");
        })
        .sum::<usize>();
    println!("{}", output);
}

fn find_reflection(notes: &[String], ignore: usize) -> usize {
    let mut reflections = 0;
    for y in 1..notes.len() {
        if (0..y)
            .filter(|&i| y + y - i - 1 < notes.len())
            .all(|i| &notes[i] == &notes[y + y - i - 1])
        {
            if y * 100 == ignore {
                continue;
            }
            reflections += y * 100;
            return reflections;
        }
    }
    for x in 1..notes[0].len() {
        if (0..x).filter(|&i| x + x - i - 1 < notes[0].len()).all(|i| {
            (0..notes.len()).all(|j| notes[j].chars().nth(i) == notes[j].chars().nth(x + x - i - 1))
        }) {
            if x == ignore {
                continue;
            }
            reflections += x;
            return reflections;
        }
    }
    reflections
}

fn fix_smudge(x: usize, y: usize, notes: &mut [String]) {
    let mut chars: Vec<char> = notes[y].chars().collect();
    match chars[x] {
        '.' => {
            chars[x] = '#';
        }
        '#' => {
            chars[x] = '.';
        }
        x => panic!("reeeeeeeeee {x}"),
    }
    notes[y] = chars.into_iter().collect();
}
