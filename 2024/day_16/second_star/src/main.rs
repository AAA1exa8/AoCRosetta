use std::{
    cmp::Ordering,
    collections::{BinaryHeap, HashMap, HashSet, VecDeque},
    hash::{Hash, Hasher},
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

impl Hash for Node {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.x.hash(state);
        self.y.hash(state);
        self.dir.hash(state);
    }
}

#[derive(Debug, PartialEq, Eq)]
struct State {
    node: Node,
    cost: i32,
}

impl Ord for State {
    fn cmp(&self, other: &Self) -> Ordering {
        other.cost.cmp(&self.cost)
    }
}

impl PartialOrd for State {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
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

fn collect_unique_tiles(
    came_from: &HashMap<(i32, i32, Direction), Vec<(i32, i32, Direction)>>,
    end_node: &Node,
) -> HashSet<(i32, i32)> {
    let mut unique_tiles = HashSet::new();
    let mut queue = VecDeque::new();
    let end_key = (end_node.x, end_node.y, end_node.dir);
    queue.push_back(end_key);

    while let Some(current_key) = queue.pop_front() {
        unique_tiles.insert((current_key.0, current_key.1));
        if let Some(parents) = came_from.get(&current_key) {
            for parent in parents {
                queue.push_back(*parent);
            }
        }
    }

    unique_tiles
}

fn dijkstra(map: &[Vec<Tile>], start: Node, end: (i32, i32)) -> (i32, HashSet<(i32, i32)>) {
    let mut open_set = BinaryHeap::new();
    open_set.push(State {
        node: start.clone(),
        cost: start.g,
    });

    let mut came_from: HashMap<(i32, i32, Direction), Vec<(i32, i32, Direction)>> = HashMap::new();

    let mut g_score: HashMap<(i32, i32, Direction), i32> = HashMap::new();
    g_score.insert((start.x, start.y, start.dir), start.g);

    let mut minimal_cost_to_end: Option<i32> = None;

    while let Some(State {
        node: current,
        cost,
    }) = open_set.pop()
    {
        if let Some(min_cost) = minimal_cost_to_end {
            if cost > min_cost {
                break;
            }
        }

        if (current.x, current.y) == end {
            if minimal_cost_to_end.is_none() || current.g < minimal_cost_to_end.unwrap() {
                minimal_cost_to_end = Some(current.g);
            }

            continue;
        }

        for (nx, ny, ndir, step_cost) in get_neighbors(map, &current) {
            let tentative_g_score = current.g + step_cost;
            let neighbor_key = (nx, ny, ndir);

            let entry = g_score.get(&neighbor_key);
            if entry.is_none() || tentative_g_score < *entry.unwrap() {
                g_score.insert(neighbor_key, tentative_g_score);
                came_from
                    .entry(neighbor_key)
                    .or_insert_with(Vec::new)
                    .clear();
                came_from
                    .get_mut(&neighbor_key)
                    .unwrap()
                    .push((current.x, current.y, current.dir));

                open_set.push(State {
                    node: Node {
                        x: nx,
                        y: ny,
                        g: tentative_g_score,
                        dir: ndir,
                    },
                    cost: tentative_g_score,
                });
            } else if tentative_g_score == *entry.unwrap() {
                came_from
                    .entry(neighbor_key)
                    .or_insert_with(Vec::new)
                    .push((current.x, current.y, current.dir));
            }
        }
    }

    if let Some(min_cost) = minimal_cost_to_end {
        let end_nodes: Vec<Node> = g_score
            .keys()
            .filter(|&&(x, y, _)| (x, y) == end)
            .filter_map(|&(x, y, dir)| {
                if g_score.get(&(x, y, dir)).copied() == Some(min_cost) {
                    Some(Node {
                        x,
                        y,
                        g: min_cost,
                        dir,
                    })
                } else {
                    None
                }
            })
            .collect();

        let mut all_unique_tiles = HashSet::new();
        for end_node in &end_nodes {
            let unique_tiles = collect_unique_tiles(&came_from, end_node);
            all_unique_tiles.extend(unique_tiles);
        }

        (min_cost, all_unique_tiles)
    } else {
        (0, HashSet::new())
    }
}

#[allow(dead_code)]
fn build_map(original_map: &[Vec<Tile>], unique_tiles: &HashSet<(i32, i32)>) -> String {
    let mut map = original_map.to_vec();
    for &(x, y) in unique_tiles {
        map[y as usize][x as usize] = Tile::RainDear(Direction::Down);
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

    let (_, unique_tiles) = dijkstra(&map, start.clone(), end);

    println!("{}", unique_tiles.len())
}
