#[derive(Debug, Eq, PartialEq, PartialOrd, Ord, Clone, Copy)]
enum Anthena {
    Anthena(char),
    AntiAnthena,
}

#[derive(Debug, Clone, Copy)]
struct Loc {
    anthena: Anthena,
    x: isize,
    y: isize,
}

impl std::cmp::PartialEq for Loc {
    fn eq(&self, other: &Self) -> bool {
        self.x == other.x && self.y == other.y
    }
}

impl std::cmp::PartialOrd for Loc {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        if self.y < other.y {
            Some(std::cmp::Ordering::Less)
        } else if self.y > other.y {
            Some(std::cmp::Ordering::Greater)
        } else {
            if self.x < other.x {
                Some(std::cmp::Ordering::Less)
            } else if self.x > other.x {
                Some(std::cmp::Ordering::Greater)
            } else {
                Some(std::cmp::Ordering::Equal)
            }
        }
    }
}

impl std::cmp::Ord for Loc {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.partial_cmp(other).unwrap()
    }
}

impl std::cmp::Eq for Loc {}

fn main() {
    let input = "../input.txt";
    let input_file = std::fs::read_to_string(input).expect("Error reading input file");
    let mut anthenas = Vec::new();
    let _ = input_file.lines().enumerate().for_each(|(y, line)| {
        line.chars().enumerate().for_each(|(x, c)| match c {
            '.' => (),
            c => {
                anthenas.push(Loc {
                    anthena: Anthena::Anthena(c),
                    x: x as isize,
                    y: y as isize,
                });
            }
        })
    });
    anthenas.sort();
    let mut antiloc = Vec::new();
    let width = input_file.lines().next().unwrap().len() as isize;
    let height = input_file.lines().count() as isize;
    for anthena in anthenas.iter() {
        let same_anthenas = anthenas
            .iter()
            .filter(|a| a.anthena == anthena.anthena)
            .collect::<Vec<_>>();
        for &&a in same_anthenas.iter() {
            for &&b in same_anthenas.iter() {
                if a == b {
                    continue;
                }
                let dx = a.x - b.x;
                let dy = a.y - b.y;

                for i in 0.. {
                    let a1x = a.x + dx * i;
                    let a1y = a.y + dy * i;
                    if a1x < 0 || a1x >= width || a1y < 0 || a1y >= height {
                        break;
                    }
                    let loc = Loc {
                        anthena: Anthena::AntiAnthena,
                        x: a1x,
                        y: a1y,
                    };
                    if let Err(_) = antiloc.binary_search(&loc) {
                        antiloc.push(loc);
                        antiloc.sort();
                    }
                }

                for i in 0.. {
                    let a2x = b.x - dx * i;
                    let a2y = b.y - dy * i;
                    if a2x < 0 || a2x >= width || a2y < 0 || a2y >= height {
                        break;
                    }
                    let loc = Loc {
                        anthena: Anthena::AntiAnthena,
                        x: a2x,
                        y: a2y,
                    };
                    if let Err(_) = antiloc.binary_search(&loc) {
                        antiloc.push(loc);
                        antiloc.sort();
                    }
                }
            }
        }
    }
    println!("{:?}", antiloc.len());
}
