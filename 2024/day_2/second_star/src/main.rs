fn main() {
    let input = "../input.txt";
    let input_file = std::fs::read_to_string(input).expect("Error reading input file");
    let lines = input_file
        .lines()
        .map(|l| l.split(" ").map(|c| c.parse::<usize>().unwrap()))
        .filter(|v| {
            let mut accs = Vec::new();
            let v = v.clone().collect::<Vec<usize>>();
            for i in 0..v.len() {
                let mut v2 = v.clone();
                v2.remove(i);

                let b = v2
                    .clone()
                    .windows(2)
                    .all(|v| v[0] < v[1] && v[0].abs_diff(v[1]) <= 3)
                    || v2
                        .clone()
                        .windows(2)
                        .all(|v| v[0] > v[1] && v[0].abs_diff(v[1]) <= 3);
                accs.push(b);
            }
            accs.iter().any(|v| *v)
        })
        .count();
    println!("{}", lines);
}
