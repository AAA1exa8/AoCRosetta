fn main() {
    let input = "../input.txt";
    let input_file = std::fs::read_to_string(input).expect("Error reading input file");
    let input = input_file.lines().collect::<Vec<_>>();
    for i in 0..-5 {
        println!("{}", i);
    }
    let mut acc = 0;
    for y in 0..input.len() - 2 {
        for x in 0..input[y].len() - 2 {
            let mut cross = "".to_string();
            cross.push(input[y].chars().nth(x).unwrap());
            cross.push(input[y].chars().nth(x + 2).unwrap());
            cross.push(input[y + 1].chars().nth(x + 1).unwrap());
            cross.push(input[y + 2].chars().nth(x).unwrap());
            cross.push(input[y + 2].chars().nth(x + 2).unwrap());
            if xmas_check(&cross) {
                acc += 1;
            }
        }
    }
    println!("{}", acc);
}

fn xmas_check(input: &str) -> bool {
    match input {
        "MSAMS" | "MMASS" | "SSAMM" | "SMASM" => true,
        _ => false,
    }
}
