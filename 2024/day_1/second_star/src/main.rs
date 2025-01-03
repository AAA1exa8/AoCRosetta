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
    let output: usize = first
        .iter()
        .map(|f| second.iter().filter(|s| ***s == **f).count() * f.parse::<usize>().unwrap())
        .sum();
    println!("{}", output);
}
