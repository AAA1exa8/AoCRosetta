use std::collections::{HashSet, VecDeque};

#[derive(Debug, PartialEq, Eq, Hash, Clone, Copy)]
struct Coord {
    x: usize,
    y: usize,
}

#[derive(Debug)]
struct Map(Vec<Coord>);

impl Map {
    #[allow(dead_code)]
    fn print_map(&self, dims: Coord) {
        let mut map = vec![vec!['.'; dims.x]; dims.y];
        for coord in self.0.iter() {
            map[coord.y][coord.x] = '#';
        }
        for row in map.iter() {
            for cell in row.iter() {
                print!("{}", cell);
            }
            println!();
        }
    }

    fn bfs(&self, start: Coord, end: Coord, bounds: Coord) -> Option<usize> {
        let mut visited = HashSet::new();
        let mut queue = VecDeque::new();
        queue.push_back((start, 0));
        while let Some((coord, dist)) = queue.pop_front() {
            if coord == end {
                return Some(dist);
            }
            if visited.contains(&coord) {
                continue;
            }
            visited.insert(coord);
            for (dx, dy) in &[(0, 1), (0, -1), (1, 0), (-1, 0)] {
                let new_x = coord.x as isize + dx;
                let new_y = coord.y as isize + dy;
                if new_x < 0
                    || new_x >= bounds.x as isize
                    || new_y < 0
                    || new_y >= bounds.y as isize
                {
                    continue;
                }
                let new_coord = Coord {
                    x: new_x as usize,
                    y: new_y as usize,
                };
                if self.0.contains(&new_coord) {
                    continue;
                }
                queue.push_back((new_coord, dist + 1));
            }
        }
        None
    }
}

fn main() {
    let input = "../input.txt";
    let input_file = std::fs::read_to_string(input).expect("Error reading input file");
    let mut bytes = input_file
        .lines()
        .take(1024)
        .map(|line| {
            let (x, y) = line.split_once(",").unwrap();
            Coord {
                x: x.parse().unwrap(),
                y: y.parse().unwrap(),
            }
        })
        .collect::<Vec<Coord>>();
    let map = Map(bytes);
    let start = Coord { x: 0, y: 0 };
    let end = Coord { x: 70, y: 70 };
    let bounds = Coord { x: 71, y: 71 };
    let len = map.bfs(start, end, bounds);
    println!("{}", len.unwrap());
}
