fn main() {
    let input = "../input.txt";
    let input_file = std::fs::read_to_string(input).expect("Error reading input file");
    let mut first = Vec::new();
    let mut second = Vec::new();
    input_file
        .lines()
        .map(|l| l.split("   "))
        .for_each(|mut l| {
            first.push(l.next().unwrap());
            second.push(l.next().unwrap());
        });
    first.sort();
    second.sort();
    let output: i32 = first
        .iter()
        .zip(second.iter())
        .map(|(f, s)| {
            let f = f.parse::<i32>().unwrap();
            let s = s.parse::<i32>().unwrap();
            (s - f).abs()
        })
        .sum();
    println!("{}", output);
}
