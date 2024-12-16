use std::{
    cmp::Ordering,
    collections::{BinaryHeap, HashMap},
    hash::Hash,
};

#[derive(PartialEq, Eq, Debug, Clone)]
enum Tile {
    Empty,
    Wall,
    RainDear(Direction),
}

#[derive(PartialEq, Eq, Debug, Hash, Clone, Copy)]
enum Direction {
    Left,
    Right,
    Up,
    Down,
}

#[derive(Debug, Clone)]
struct Node {
    x: i32,
    y: i32,
    g: i32,
    dir: Direction,
}

impl PartialEq for Node {
    fn eq(&self, other: &Self) -> bool {
        self.x == other.x && self.y == other.y && self.dir == other.dir
    }
}

impl Eq for Node {}

#[derive(Debug, PartialEq, Eq)]
struct State {
    node: Node,
    f: i32,
}

impl Ord for State {
    fn cmp(&self, other: &Self) -> Ordering {
        other.f.cmp(&self.f)
    }
}

impl PartialOrd for State {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.f.cmp(&other.f).reverse())
    }
}

fn heuristic(x: i32, y: i32, end: (i32, i32)) -> i32 {
    (x - end.0).abs() + (y - end.1).abs()
}

fn get_neighbors(map: &[Vec<Tile>], node: &Node) -> Vec<(i32, i32, Direction, i32)> {
    let mut neighbors = Vec::new();
    match node.dir {
        Direction::Left => {
            if node.x > 0 && map[node.y as usize][(node.x - 1) as usize] == Tile::Empty {
                neighbors.push((node.x - 1, node.y, Direction::Left, 1));
            }

            neighbors.push((node.x, node.y, Direction::Down, 1000));

            neighbors.push((node.x, node.y, Direction::Up, 1000));
        }
        Direction::Right => {
            if node.x < map[0].len() as i32 - 1
                && map[node.y as usize][(node.x + 1) as usize] == Tile::Empty
            {
                neighbors.push((node.x + 1, node.y, Direction::Right, 1));
            }

            neighbors.push((node.x, node.y, Direction::Up, 1000));

            neighbors.push((node.x, node.y, Direction::Down, 1000));
        }
        Direction::Up => {
            if node.y > 0 && map[(node.y - 1) as usize][node.x as usize] == Tile::Empty {
                neighbors.push((node.x, node.y - 1, Direction::Up, 1));
            }

            neighbors.push((node.x, node.y, Direction::Left, 1000));

            neighbors.push((node.x, node.y, Direction::Right, 1000));
        }
        Direction::Down => {
            if node.y < map.len() as i32 - 1
                && map[(node.y + 1) as usize][node.x as usize] == Tile::Empty
            {
                neighbors.push((node.x, node.y + 1, Direction::Down, 1));
            }

            neighbors.push((node.x, node.y, Direction::Right, 1000));

            neighbors.push((node.x, node.y, Direction::Left, 1000));
        }
    }
    neighbors
}

fn build_path(
    parents: &HashMap<(i32, i32, Direction), (i32, i32, Direction)>,
    end_node: &Node,
) -> Vec<Node> {
    let mut path = Vec::new();
    let mut current = end_node.clone();
    loop {
        path.push(current.clone());
        let current_key = (current.x, current.y, current.dir);
        let parent_key = parents.get(&current_key).cloned();
        match parent_key {
            Some((px, py, pdir)) => {
                if px == current.x && py == current.y && pdir == current.dir {
                    break;
                }
                current = Node {
                    x: px,
                    y: py,
                    g: 0,
                    dir: pdir,
                };
            }
            None => break,
        }
    }
    path.reverse();
    path
}

fn a_star(map: &[Vec<Tile>], start: Node, end: (i32, i32)) -> (i32, Vec<Node>) {
    let mut open_set = BinaryHeap::new();
    let start_f = start.g + heuristic(start.x, start.y, end);
    open_set.push(State {
        node: start.clone(),
        f: start_f,
    });

    let mut came_from: HashMap<(i32, i32, Direction), (i32, i32, Direction)> = HashMap::new();
    came_from.insert((start.x, start.y, start.dir), (start.x, start.y, start.dir));

    let mut g_score: HashMap<(i32, i32, Direction), i32> = HashMap::new();
    g_score.insert((start.x, start.y, start.dir), start.g);

    while let Some(State { node: current, .. }) = open_set.pop() {
        if (current.x, current.y) == end {
            let path = build_path(&came_from, &current);
            return (current.g, path);
        }

        for (nx, ny, ndir, step_cost) in get_neighbors(map, &current) {
            let tentative_g_score = current.g + step_cost;
            let neighbor_key = (nx, ny, ndir);

            if tentative_g_score < *g_score.get(&neighbor_key).unwrap_or(&i32::MAX) {
                came_from.insert(neighbor_key, (current.x, current.y, current.dir));
                g_score.insert(neighbor_key, tentative_g_score);
                let f_score = tentative_g_score + heuristic(nx, ny, end);
                let neighbor_node = Node {
                    x: nx,
                    y: ny,
                    g: tentative_g_score,
                    dir: ndir,
                };
                open_set.push(State {
                    node: neighbor_node,
                    f: f_score,
                });
            }
        }
    }

    (0, Vec::new())
}

#[allow(dead_code)]
fn build_map(original_map: &[Vec<Tile>], path: &[Node]) -> String {
    let mut map = original_map.to_vec();
    for node in path {
        map[node.y as usize][node.x as usize] = Tile::RainDear(node.dir);
    }
    map.iter()
        .map(|row| {
            row.iter()
                .map(|tile| match tile {
                    Tile::Empty => '.',
                    Tile::Wall => '#',
                    Tile::RainDear(dir) => match dir {
                        Direction::Left => '<',
                        Direction::Right => '>',
                        Direction::Up => '^',
                        Direction::Down => 'v',
                    },
                })
                .collect::<String>()
        })
        .collect::<Vec<String>>()
        .join("\n")
}

fn main() {
    let input = "../input.txt";
    let input_file = std::fs::read_to_string(input).expect("Error reading input file");

    let mut start = Node {
        x: 0,
        y: 0,
        g: 0,
        dir: Direction::Right,
    };
    let mut end = (0, 0);

    let map = input_file
        .lines()
        .enumerate()
        .map(|(y, line)| {
            line.chars()
                .enumerate()
                .map(|(x, c)| match c {
                    '.' => Tile::Empty,
                    '#' => Tile::Wall,
                    'S' => {
                        start = Node {
                            x: x as i32,
                            y: y as i32,
                            g: 0,
                            dir: Direction::Right,
                        };
                        Tile::Empty
                    }
                    'E' => {
                        end = (x as i32, y as i32);
                        Tile::Empty
                    }
                    _ => panic!("Invalid character in map"),
                })
                .collect::<Vec<Tile>>()
        })
        .collect::<Vec<Vec<Tile>>>();

    let (result, _) = a_star(&map, start.clone(), end);
    println!("{}", result)
}
