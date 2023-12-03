use std::collections::VecDeque;

fn main() {
    let input = "../../input.txt";
    let input_file = std::fs::read_to_string(input).expect("Error reading input file");
    let mut engine = Engine {
        gears: Vec::new(),
        lines: VecDeque::new(),
    };
    engine.lines.push_back(("".to_string(), 0));
    for (l, line) in input_file.lines().enumerate() {
        let line = line.trim();
        if engine.lines.len() < 3 {
            engine.lines.push_back((line.to_string(), l));
            continue;
        }
        engine.parse_manual_line(line, l);
    }
    engine.gears.sort();
    engine.gears.dedup();
    println!(
        "{}",
        engine
            .gears
            .iter()
            .filter(|g| g.adjecent.len() == 2 && {
                !engine.gears.iter().any(|g2| {
                    g2.adjecent.len() > g.adjecent.len() && g2.position == g.position
                })
            })
            .map(|g| g.adjecent[0] * g.adjecent[1])
            .sum::<usize>()
    );
}
#[derive(Debug)]
struct Engine {
    gears: Vec<Gear>,
    lines: VecDeque<(String, usize)>,
}

#[derive(Debug)]
struct Number {
    value: usize,
    position: (usize, usize),
    num_of_digits: usize,
}

#[derive(Debug, PartialEq, Ord, Eq, PartialOrd)]
struct Gear {
    position: (usize, usize),
    adjecent: Vec<usize>,
}

impl Engine {
    fn parse_manual_line(&mut self, line: &str, line_num: usize) {
        self.lines.pop_front();
        self.lines.push_back((line.to_string(), line_num));
        let mut recording: bool = false;
        let mut current_nums = Vec::new();
        let mut current_gears = Vec::new();
        for (line, l) in self.lines.iter() {
            for (i, c) in line.chars().enumerate() {
                if c.is_digit(10) {
                    if !recording {
                        current_nums.push(Number {
                            value: c.to_digit(10).unwrap() as usize,
                            position: (*l, i),
                            num_of_digits: 1,
                        });
                        recording = true;
                    } else {
                        let last_num = current_nums.last_mut().unwrap();
                        last_num.value = last_num.value * 10 + c.to_digit(10).unwrap() as usize;
                        last_num.num_of_digits += 1;
                    }
                } else if c == '*' {
                    current_gears.push(Gear {
                        position: (*l, i),
                        adjecent: Vec::new(),
                    });
                    recording = false;
                } else {
                    recording = false;
                }
            }
        }
        for gear in current_gears.iter_mut() {
            for num in current_nums.iter() {
                if (gear.position.1 as i32 >= (num.position.1 as i32 - 1))
                    && ((gear.position.1 as i32) <= (num.position.1 + num.num_of_digits) as i32)
                    && ((gear.position.0 as i32) - (num.position.0 as i32)).abs() < 2
                {
                    gear.adjecent.push(num.value);
                }
            }
        }
        self.gears.extend(current_gears);
    }
}
