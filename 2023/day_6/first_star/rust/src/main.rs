fn main() {
    let input = "../../input.txt";
    let input_file = std::fs::read_to_string(input).expect("Error reading input file");
    let times = input_file
        .lines()
        .next()
        .unwrap()
        .split(' ')
        .skip(1)
        .filter(|c| c != &"")
        .map(|c| c.parse::<i32>().unwrap())
        .collect::<Vec<i32>>();
    let distances = input_file
        .lines()
        .nth(1)
        .unwrap()
        .split(' ')
        .skip(1)
        .filter(|c| c != &"")
        .map(|c| c.parse::<i32>().unwrap())
        .collect::<Vec<i32>>();
    let output: i32 = times.iter().zip(distances.iter()).map(|(time, dist)|find_speeds(*time, *dist)).product();
    println!("{}", output);
}

fn find_speeds(time: i32, dist: i32) -> i32 {
    let mut ans = 0;
    for i in 0..=time {
        if (time - i) * i > dist {
            ans += 1;
        }
    }
    ans
}