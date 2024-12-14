#[derive(Debug)]
struct Robot {
    position: Coord,
    velocity: Coord,
}

#[derive(Debug)]
struct Coord {
    x: isize,
    y: isize,
}

impl Coord {
    fn new(x: isize, y: isize) -> Self {
        Self { x, y }
    }

    fn new_from_str(s: &str) -> Self {
        let mut parts = s.split('=').nth(1).unwrap().split(",");
        let x = parts.next().unwrap().parse().unwrap();
        let y = parts.next().unwrap().parse().unwrap();
        Self { x, y }
    }
}

const WIDTH: isize = 101;
const HEIGHT: isize = 103;

fn mod_positive(x: isize, modulus: isize) -> isize {
    ((x % modulus) + modulus) % modulus
}

fn main() {
    let input = "../input.txt";
    let input_file = std::fs::read_to_string(input).expect("Error reading input file");
    let mut quads = [0; 4];
    let mut map = vec![vec!['.'; WIDTH as usize]; HEIGHT as usize];
    input_file
        .lines()
        .map(|line| {
            let mut parts = line.split_whitespace();
            let position = Coord::new_from_str(parts.next().unwrap());
            let velocity = Coord::new_from_str(parts.next().unwrap());
            Robot { position, velocity }
        })
        .map(|robot| {
            let position = Coord::new(
                mod_positive(robot.position.x + robot.velocity.x * 100, WIDTH),
                mod_positive(robot.position.y + robot.velocity.y * 100, HEIGHT),
            );
            Robot {
                position,
                velocity: robot.velocity,
            }
        })
        .for_each(|robot| {
            let hb = WIDTH / 2;
            let vb = HEIGHT / 2;
            map[robot.position.y as usize][robot.position.x as usize] = '1';
            if robot.position.x < hb && robot.position.y < vb {
                quads[0] += 1;
            } else if robot.position.x > hb && robot.position.y < vb {
                quads[1] += 1;
            } else if robot.position.x < hb && robot.position.y > vb {
                quads[2] += 1;
            } else if robot.position.x > hb && robot.position.y > vb {
                quads[3] += 1;
            }
        });
    println!("{:?}", quads.iter().product::<usize>());
}
