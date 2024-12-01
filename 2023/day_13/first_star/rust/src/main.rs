fn main() {
    let input = "../../input.txt";
    let input_file = std::fs::read_to_string(input).expect("Error reading input file");
    let output = input_file
        .split("\n\n")
        .map(|n| n.lines().collect::<Vec<_>>())
        .map(|n| find_reflection(&n))
        .sum::<usize>();
    println!("{}", output);
}

fn find_reflection(notes: &[&str]) -> usize {
    let mut reflections = 0;
    for y in 1..notes.len() {
        if (0..y)
            .filter(|&i| y + y - i - 1 < notes.len())
            .all(|i| &notes[i] == &notes[y + y - i - 1])
        {
            reflections += y * 100;
            return reflections;
        }
    }
    for x in 1..notes[0].len() {
        if (0..x)
            .filter(|&i| x + x - i - 1 < notes[0].len())
            .all(|i| {
                (0..notes.len()).all(|y| {
                    &notes[y][i..i + 1] == &notes[y][x + x - i - 1..x + x - i]
                })
            })
        {
            reflections += x;
            return reflections;
        }
    }
    reflections
}