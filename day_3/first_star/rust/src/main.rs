fn main() {
    let input = "../../input.txt";
    let input_file = std::fs::read_to_string(input).expect("Error reading input file");
    let mut engine = Engine {
        nums: Vec::new(),
        symbols: Vec::new(),
    };
    input_file.lines().enumerate().for_each(|(i, l)| {
        engine.parse_manual_line(l.trim(), i);
    });
    let output = engine
        .nums
        .iter()
        .filter(|n| {
            engine.symbols.iter().any(|s| {
                (s.position.1 as i32 >= (n.position.1 as i32 - 1))
                    && ((s.position.1 as i32) <= (n.position.1 + n.num_of_digits) as i32)
                    && ((s.position.0 as i32) - (n.position.0 as i32)).abs() <= 1
            })
        })
        .map(|n| n.value)
        .sum::<usize>();
    println!("{}", output);
}

#[derive(Debug)]
struct Engine {
    nums: Vec<Number>,
    symbols: Vec<Symbol>,
}

#[derive(Debug)]
struct Number {
    value: usize,
    position: (usize, usize),
    num_of_digits: usize,
}

#[derive(Debug)]
struct Symbol {
    position: (usize, usize),
}

impl Engine {
    fn parse_manual_line(&mut self, line: &str, line_num: usize) {
        let mut recording: bool = false;
        for (i, c) in line.chars().enumerate() {
            if c == '.' {
                recording = false;
                continue;
            }
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
            } else {
                self.symbols.push(Symbol {
                    position: (line_num, i),
                });
                recording = false;
            }
        }
    }
}
