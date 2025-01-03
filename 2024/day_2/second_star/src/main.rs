fn main() {
    let input = "../input.txt";
    let input_file = std::fs::read_to_string(input).expect("Error reading input file");
    let lines = input_file
        .lines()
        .map(|l| l.split(" ").map(|c| c.parse::<usize>().unwrap()))
        .filter(|v| {
            let v = v.clone().collect::<Vec<usize>>();
            (0..v.len()).any(|i| {
                let mut v2 = v.clone();
                v2.remove(i);

                v2.windows(2)
                    .all(|v| v[0] < v[1] && v[0].abs_diff(v[1]) <= 3)
                    || v2
                        .windows(2)
                        .all(|v| v[0] > v[1] && v[0].abs_diff(v[1]) <= 3)
            })
        })
        .count();
    println!("{}", lines);
}
