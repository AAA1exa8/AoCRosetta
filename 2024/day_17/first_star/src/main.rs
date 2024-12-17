#[derive(Debug)]
struct CPU {
    registers: [usize; 3],
    pc: usize,
    program: Vec<usize>,
    out: String,
}

impl CPU {
    fn cycle(&mut self) -> bool {
        let opcode = self.program[self.pc * 2];
        let operand = self.program[self.pc * 2 + 1];
        match opcode {
            0 => match operand {
                0..=3 => {
                    self.registers[0] = self.registers[0] / (2usize.pow(operand as u32));
                }
                4..=6 => {
                    self.registers[0] =
                        self.registers[0] / (2usize.pow(self.registers[operand - 4] as u32));
                }
                x => unreachable!("Invalid operand for 0 {}", x),
            },
            1 => self.registers[1] ^= operand,
            2 => match operand {
                0..=3 => self.registers[1] = operand % 8,
                4..=6 => {
                    self.registers[1] = self.registers[operand - 4] % 8;
                }
                x => unreachable!("Invalid operand for 2 {}", x),
            },
            3 => {
                if self.registers[0] != 0 {
                    self.pc = operand;
                    return true;
                }
            }
            4 => {
                self.registers[1] = self.registers[1] ^ self.registers[2];
            }
            5 => match operand {
                0..=3 => {
                    self.out.push_str(&format!(",{}", operand % 8));
                    if self.out.len() < 3 {
                        self.out = self.out[1..].to_string();
                    }
                }
                4..=6 => {
                    self.out
                        .push_str(&format!(",{}", self.registers[operand - 4] % 8));
                    if self.out.len() < 3 {
                        self.out = self.out[1..].to_string();
                    }
                }
                x => unreachable!("Invalid operand for 5 {}", x),
            },
            6 => match operand {
                0..=3 => {
                    self.registers[1] = self.registers[0] / (2usize.pow(operand as u32));
                }
                4..=6 => {
                    self.registers[1] =
                        self.registers[0] / (2usize.pow(self.registers[operand - 4] as u32));
                }
                x => unreachable!("Invalid operand for 6 {}", x),
            },
            7 => match operand {
                0..=3 => {
                    self.registers[2] = self.registers[0] / (2usize.pow(operand as u32));
                }
                4..=6 => {
                    self.registers[2] =
                        self.registers[0] / (2usize.pow(self.registers[operand - 4] as u32));
                }
                x => unreachable!("Invalid operand for 7 {}", x),
            },
            x => unreachable!("Invalid opcode {}", x),
        }
        self.pc += 1;
        self.pc * 2 < self.program.len()
    }
}

fn main() {
    let input = "../input.txt";
    let input_file = std::fs::read_to_string(input).expect("Error reading input file");
    let (registers, program) = input_file.split_once("\n\n").unwrap();
    let registers: Vec<usize> = registers
        .lines()
        .map(|line| {
            let (_, value) = line.split_once(": ").unwrap();
            value.parse().unwrap()
        })
        .collect();
    let program: Vec<usize> = program
        .split_once(": ")
        .unwrap()
        .1
        .split(",")
        .map(|c| c.trim().parse::<usize>().expect(c))
        .collect();
    let mut cpu = CPU {
        registers: [registers[0], registers[1], registers[2]],
        pc: 0,
        program,
        out: String::new(),
    };
    while cpu.cycle() {}
    println!("{}", cpu.out);
}
