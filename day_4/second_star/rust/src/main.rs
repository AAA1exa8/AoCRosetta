fn main() {
    let input = "../../input.txt";
    let input_file = std::fs::read_to_string(input).expect("Error reading input file");
    let cards = input_file
        .lines()
        .map(|line| {
            let (card, nums) = line.split_once(':').unwrap();
            let card: i32 = card.split(' ').last().unwrap().parse().unwrap();
            let (win, my) = nums.split_once('|').unwrap();
            let win = win
                .trim()
                .split(' ')
                .filter(|n| n.trim() != "")
                .map(|n| n.parse::<i32>().unwrap())
                .collect::<Vec<_>>();
            let my = my
                .trim()
                .split(' ')
                .filter(|n| n.trim() != "")
                .map(|n| n.parse::<i32>().expect(&format!("NaN {n} Nan")))
                .collect::<Vec<_>>();
            let winning = win.iter().filter(|n| my.contains(n)).count();
            (card, winning)
        })
        .collect::<Vec<_>>();
    let mut copies = vec![1; cards.len()];
    for i in 0..cards.len() {
        let (card, win) = cards[i];
        for j in 0..win {
            copies[j+card as usize] += copies[card as usize-1];
        }
    }
    println!("{}", copies.iter().sum::<i32>());
}