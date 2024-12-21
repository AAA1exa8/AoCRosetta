use std::{collections::HashMap, hash::Hash};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
struct Position {
    x: usize,
    y: usize,
}

type Sequence = Vec<NumValue>;

#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq)]
enum NumValue {
    Number(usize),
    Accept,
}

#[derive(Debug, Clone, Copy, PartialEq, PartialOrd, Eq, Ord, Hash)]
enum PadValue {
    Up,
    Down,
    Left,
    Right,
    Accept,
}

fn move_to<T>(start: T, end: T, pad: &HashMap<T, Position>) -> Vec<PadValue>
where
    T: Hash + Eq + Copy,
{
    let t_pos = pad[&end];
    let s_pos = pad[&start];
    let dy = t_pos.y as isize - s_pos.y as isize;
    let dx = t_pos.x as isize - s_pos.x as isize;
    let vert = if dy > 0 {
        vec![PadValue::Down; dy as usize]
    } else {
        vec![PadValue::Up; -dy as usize]
    };
    let horiz = if dx > 0 {
        vec![PadValue::Right; dx as usize]
    } else {
        vec![PadValue::Left; -dx as usize]
    };
    if dx > 0 && pad.values().any(|pos| pos.y == t_pos.y && pos.x == s_pos.x) {
        return vert
            .into_iter()
            .chain(horiz.into_iter())
            .chain(vec![PadValue::Accept].into_iter())
            .collect();
    } else if pad.values().any(|pos| pos.y == s_pos.y && pos.x == t_pos.x) {
        return horiz
            .into_iter()
            .chain(vert.into_iter())
            .chain(vec![PadValue::Accept].into_iter())
            .collect();
    } else if pad.values().any(|pos| pos.y == t_pos.y && pos.x == s_pos.x) {
        return vert
            .into_iter()
            .chain(horiz.into_iter())
            .chain(vec![PadValue::Accept].into_iter())
            .collect();
    }
    panic!("Invalid move");
}

fn main() {
    let input = "../input.txt";
    let input_file = std::fs::read_to_string(input).expect("Error reading input file");
    let sequences = input_file
        .lines()
        .map(|line| {
            line.chars()
                .map(|c| match c {
                    'A' => NumValue::Accept,
                    _ => NumValue::Number(c.to_digit(10).unwrap() as usize),
                })
                .collect::<Sequence>()
        })
        .collect::<Vec<Sequence>>();
    let numpad = vec![
        vec![
            Option::Some(NumValue::Number(7)),
            Option::Some(NumValue::Number(8)),
            Option::Some(NumValue::Number(9)),
        ],
        vec![
            Option::Some(NumValue::Number(4)),
            Option::Some(NumValue::Number(5)),
            Option::Some(NumValue::Number(6)),
        ],
        vec![
            Option::Some(NumValue::Number(1)),
            Option::Some(NumValue::Number(2)),
            Option::Some(NumValue::Number(3)),
        ],
        vec![
            Option::None,
            Option::Some(NumValue::Number(0)),
            Option::Some(NumValue::Accept),
        ],
    ];
    let numpad = numpad
        .iter()
        .enumerate()
        .map(|(y, row)| {
            row.iter()
                .enumerate()
                .filter_map(|(x, num)| match num {
                    Some(num) => Some((*num, Position { x, y })),
                    None => None,
                })
                .collect::<Vec<(NumValue, Position)>>()
        })
        .flatten()
        .collect::<HashMap<NumValue, Position>>();
    let dirpad = vec![
        vec![
            Option::None,
            Option::Some(PadValue::Up),
            Option::Some(PadValue::Accept),
        ],
        vec![
            Option::Some(PadValue::Left),
            Option::Some(PadValue::Down),
            Option::Some(PadValue::Right),
        ],
    ];
    let dirpad = dirpad
        .iter()
        .enumerate()
        .map(|(y, row)| {
            row.iter()
                .enumerate()
                .filter_map(|(x, dir)| match dir {
                    Some(dir) => Some((*dir, Position { x, y })),
                    None => None,
                })
                .collect::<Vec<(PadValue, Position)>>()
        })
        .flatten()
        .collect::<HashMap<PadValue, Position>>();

    let result = solve(sequences, numpad, dirpad);
    println!("{}", result);
}

fn solve(
    sequences: Vec<Sequence>,
    numpad: HashMap<NumValue, Position>,
    dirpad: HashMap<PadValue, Position>,
) -> usize {
    fn get_route(
        path: Vec<PadValue>,
        pad: &HashMap<PadValue, Position>,
    ) -> HashMap<Vec<PadValue>, usize> {
        let mut route = Vec::new();
        let mut start = PadValue::Accept;
        for but in path {
            let sr = move_to(start, but, pad);
            route.push(sr);
            start = but;
        }
        let mut b = HashMap::new();
        for r in route {
            *b.entry(r).or_insert(0) += 1;
        }
        b
    }
    let mut l1_paths = sequences
        .iter()
        .map(|sequence| {
            let mut l1_path = Vec::new();
            let mut start = NumValue::Accept;
            for num in sequence {
                let path = move_to(start, *num, &numpad);
                l1_path.extend(path);
                start = *num;
            }
            let mut b = HashMap::new();
            b.insert(l1_path, 1);
            b
        })
        .collect::<Vec<HashMap<Vec<PadValue>, usize>>>();
    for _ in 0..25 {
        let mut new_paths = Vec::new();
        for path in l1_paths {
            let mut new_route = HashMap::new();
            for (sub_path, qty) in path {
                let new_counts = get_route(sub_path, &dirpad);
                for (k, v) in new_counts {
                    *new_route.entry(k).or_insert(0) += v * qty;
                }
            }
            new_paths.push(new_route);
        }
        l1_paths = new_paths;
    }
    l1_paths
        .into_iter()
        .zip(sequences.into_iter().map(|seq| {
            seq.iter()
                .filter_map(|num| match num {
                    NumValue::Number(n) => Some(n),
                    NumValue::Accept => None,
                })
                .fold(0, |acc, n| acc * 10 + n)
        }))
        .map(|(route, seq)| {
            route
                .iter()
                .map(|(r, qty)| {
                    let len = r.len();
                    len * qty
                })
                .sum::<usize>()
                * seq
        })
        .sum()
}
