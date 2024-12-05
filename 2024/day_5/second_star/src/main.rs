use std::usize;

#[derive(Debug, PartialEq, Eq)]
struct Page<'a> {
    number: u32,
    order: &'a [(u32, u32)],
}

impl<'a> Page<'a> {
    fn new(number: u32, order: &'a [(u32, u32)]) -> Self {
        Page { number, order }
    }
}

impl PartialOrd for Page<'_> {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        let includes = self.order.iter().find(|&&(one, two)| {
            (one == self.number || two == self.number)
                && (one == other.number || two == other.number)
        });
        match includes {
            Some(x) => {
                if x.0 == self.number {
                    Some(std::cmp::Ordering::Less)
                } else {
                    Some(std::cmp::Ordering::Greater)
                }
            }
            None => None,
        }
    }
}

impl Ord for Page<'_> {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.partial_cmp(other).unwrap_or(std::cmp::Ordering::Equal)
    }
}

fn main() {
    let input = "../input.txt";
    let input_file = std::fs::read_to_string(input).expect("Error reading input file");
    let (sorting, pages_s) = input_file.split_once("\n\n").unwrap();
    let sorting = sorting
        .lines()
        .map(|line| line.split_once("|").unwrap())
        .map(|(a, b)| (a.parse::<u32>().unwrap(), b.parse::<u32>().unwrap()))
        .collect::<Vec<(u32, u32)>>();
    let mut pages = pages_s
        .lines()
        .map(|line| {
            line.split(",")
                .map(|x| x.parse::<u32>().unwrap())
                .map(|x| Page::new(x, &sorting))
                .collect::<Vec<Page>>()
        })
        .collect::<Vec<Vec<Page>>>();
    let sum = pages
        .iter_mut()
        .filter(|x| !is_sorted(x))
        .map(|x| {
            x.sort();
            x
        })
        .map(|x| x[(x.len() / 2) as usize].number)
        .sum::<u32>();
    println!("{}", sum);
}

fn is_sorted(slice: &[Page]) -> bool {
    let mut acc = true;
    for i in 0..slice.len() - 1 {
        let a = &slice[i];
        acc &= slice.iter().skip(i + 1).all(|b| a <= b);
    }
    acc
}
