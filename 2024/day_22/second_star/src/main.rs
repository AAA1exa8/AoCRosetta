use std::collections::{HashMap, HashSet};

const MAGIC_MODULO: u64 = 16777216;

fn main() {
    let input = "../input.txt";
    let input_file = std::fs::read_to_string(input).expect("Error reading input file");
    let initial_numbers: Vec<u64> = input_file
        .lines()
        .map(|line| line.parse::<u64>().unwrap())
        .collect();

    let mut buyers_prices: Vec<Vec<i8>> = Vec::with_capacity(initial_numbers.len());

    for &initial in &initial_numbers {
        let mut secret = initial;
        let mut prices = Vec::with_capacity(2001);
        prices.push((secret % 10) as i8);
        for _ in 0..2000 {
            let mut x = ((secret * 64) ^ secret) % MAGIC_MODULO;
            x = ((x / 32) ^ x) % MAGIC_MODULO;
            x = ((x * 2048) ^ x) % MAGIC_MODULO;
            secret = x;
            prices.push((x % 10) as i8);
        }
        buyers_prices.push(prices);
    }

    let mut sums: HashMap<(i8, i8, i8, i8), u64> = HashMap::new();
    for prices in &buyers_prices {
        let changes: Vec<i8> = prices.windows(2).map(|w| w[1] - w[0]).collect();
        let mut seen: HashSet<(i8, i8, i8, i8)> = HashSet::new();

        for (i, change) in changes.windows(4).enumerate() {
            let seq = (change[0], change[1], change[2], change[3]);

            if !seen.contains(&seq) {
                let selling_price = prices[i + 4] as u64;
                *sums.entry(seq).or_insert(0) += selling_price;
                seen.insert(seq);
            }
        }
    }
    println!("{}", sums.values().max().unwrap());
}
