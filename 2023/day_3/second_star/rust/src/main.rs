fn main() {
    let input = "../../input.txt";
    let input_file = std::fs::read_to_string(input).expect("Error reading input file");
    let mut engine = Engine {
        nums: Vec::new(),
        gears: Vec::new(),
    };
    input_file.lines().enumerate().for_each(|(i, l)| {
        engine.parse_manual_line(l.trim(), i);
    });
    let output = engine.gears.iter().map(|g| {
        engine.nums.iter().filter(|n| {
            (g.position.1 as i32 >= (n.position.1 as i32 - 1))
                && ((g.position.1 as i32) <= (n.position.1 + n.num_of_digits) as i32)
                && ((g.position.0 as i32) - (n.position.0 as i32)).abs() < 2
        }).collect::<Vec<&Number>>()
    })
    .filter(|n| n.len() == 2)
    .map(|n| n.iter().map(|n| n.value).product::<usize>())
    .sum::<usize>();
    println!("{}", output);
}

#[derive(Debug)]
struct Engine {
    nums: Vec<Number>,
    gears: Vec<Gear>,
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
}

impl Engine {
    fn parse_manual_line(&mut self, line: &str, line_num: usize) {
        let mut recording: bool = false;
        for (i, c) in line.chars().enumerate() {
            if c.is_digit(10) {
                if !recording {
                    self.nums.push(Number {
                        value: c.to_digit(10).unwrap() as usize,
                        position: (line_num, i),
                        num_of_digits: 1,
                    });
                    recording = true;
                } else {
                    let last_num = self.nums.last_mut().unwrap();
                    last_num.value = last_num.value * 10 + c.to_digit(10).unwrap() as usize;
                    last_num.num_of_digits += 1;
                }
            } else if c == '*' {
                self.gears.push(Gear {
                    position: (line_num, i),
                });
                recording = false;
            } else {
                recording = false;
            }
        }
    }
}
