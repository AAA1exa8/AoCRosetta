fn main() {
    let input = "../../input.txt";
    let input_file = std::fs::read_to_string(input).expect("Error reading input file");
    let output: usize = input_file.lines().map(|line| {
        let (_, nums) = line.split_once(':').unwrap();
        let (win, my) = nums.split_once('|').unwrap();
        let win = win.trim().split(' ').filter(|n| n.trim() != "").map(|n| n.parse::<i32>().unwrap()).collect::<Vec<_>>();
        let my = my.trim().split(' ').filter(|n| n.trim() != "").map(|n| n.parse::<i32>().expect(&format!("NaN {n} Nan"))).collect::<Vec<_>>();
        let b = my.iter().filter(|n| win.contains(n)).count();
        if b == 0 {
            return 0;
        } else {
            return 2_usize.pow((b as u32) - 1)
        }
    }).sum();
    println!("{}", output);
}
