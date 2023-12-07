fn main() {
    let input = "../../input.txt";
    let input_file = std::fs::read_to_string(input).expect("Error reading input file");
    let hands = input_file
        .lines()
        .map(|line| line.split_once(" ").unwrap())
        .map(|(a, b)| {
            (
                a.chars().map(|c| card_to_num(c)).collect::<Vec<u64>>(),
                b.parse::<u64>().unwrap(),
            )
        })
        .collect::<Vec<_>>();
    let mut t_hands = Vec::new();
    for (hand, _) in &hands {
        let mut hand = hand.clone();
        hand.sort();
        hand.reverse();
        let mut t_hand: Vec<u64> = Vec::new();
        t_hand.resize(13, 0);
        for card in &hand {
            if *card == 1 {
                // Find the index of the maximum value in t_hand
                let max_index = t_hand.iter().enumerate().max_by_key(|&(_, item)| item).map(|(index, _)| index).unwrap_or(0);
                // Add 1 to the highest element in t_hand
                t_hand[max_index] += 1;
                continue;
            }
            t_hand[*card as usize - 2] += 1;
        }
        t_hand = t_hand.into_iter().filter(|x| *x != 0).collect();
        t_hand.sort_unstable();
        let t_hand = match &t_hand[..] {
            [5] => 6,
            [1, 4] => 5,
            [2, 3] => 4,
            [1, 1, 3] => 3,
            [1, 2, 2] => 2,
            [1, 1, 1, 2] => 1,
            [1, 1, 1, 1, 1] => 0,
            x => {
                println!("{:?}", x);
                panic!("Invalid hand");
            },
        };
        t_hands.push(t_hand);
    }
    let mut hands = hands
        .into_iter()
        .zip(t_hands.into_iter())
        .collect::<Vec<_>>();
    hands.sort_by(|((cards, _), typed), ((cards2, _), typed2)| {
        let mut ord = typed.cmp(&typed2);
        if ord == std::cmp::Ordering::Equal {
            for (card, card2) in cards.iter().zip(cards2.iter()) {
                ord = card.cmp(card2);
                if ord != std::cmp::Ordering::Equal {
                    break;
                }
            }
        }
        ord
    });
    let output: usize = hands.iter().enumerate().map(|(g,((cards, bet),_))|{
        (*bet as usize)*(g + 1)
    }).sum();
    println!("{}", output);
    
}

fn card_to_num(card: char) -> u64 {
    match card {
        'J' => 1,
        '2' => 2,
        '3' => 3,
        '4' => 4,
        '5' => 5,
        '6' => 6,
        '7' => 7,
        '8' => 8,
        '9' => 9,
        'T' => 10,
        'Q' => 12,
        'K' => 13,
        'A' => 14,
        _ => panic!("Invalid card"),
    }
}
