const MAGIC_MODULO: u64 = 16777216;

fn main() {
    let input = "../input.txt";
    let input_file = std::fs::read_to_string(input).expect("Error reading input file");
    let mut numbers = input_file
        .lines()
        .map(|line| line.parse::<u64>().unwrap())
        .collect::<Vec<u64>>();
    for _ in 0..2000 {
        numbers.iter_mut().for_each(|n| {
            let mut x = ((*n * 64) ^ *n) % MAGIC_MODULO;
            x = ((x / 32) ^ x) % MAGIC_MODULO;
            x = ((x * 2048) ^ x) % MAGIC_MODULO;
            *n = x;
        });
    }
    let res = numbers.iter().sum::<u64>();
    println!("{}", res);
}
