use std::collections::{HashMap, HashSet, VecDeque};

struct Region {
    plant_type: char,
    area: usize,
    cells: HashSet<(usize, usize)>,
}

impl Region {
    fn sides(&self) -> usize {
        let mut sides = 0;
        for &(x, y) in &self.cells {
            if !self.cells.contains(&(x - 1, y)) && !self.cells.contains(&(x, y - 1)) {
                sides += 1;
            }
            if !self.cells.contains(&(x + 1, y)) && !self.cells.contains(&(x, y - 1)) {
                sides += 1;
            }
            if !self.cells.contains(&(x - 1, y)) && !self.cells.contains(&(x, y + 1)) {
                sides += 1;
            }
            if !self.cells.contains(&(x + 1, y)) && !self.cells.contains(&(x, y + 1)) {
                sides += 1;
            }
            if self.cells.contains(&(x - 1, y))
                && self.cells.contains(&(x, y - 1))
                && !self.cells.contains(&(x - 1, y - 1))
            {
                sides += 1;
            }
            if self.cells.contains(&(x + 1, y))
                && self.cells.contains(&(x, y - 1))
                && !self.cells.contains(&(x + 1, y - 1))
            {
                sides += 1;
            }
            if self.cells.contains(&(x - 1, y))
                && self.cells.contains(&(x, y + 1))
                && !self.cells.contains(&(x - 1, y + 1))
            {
                sides += 1;
            }
            if self.cells.contains(&(x + 1, y))
                && self.cells.contains(&(x, y + 1))
                && !self.cells.contains(&(x + 1, y + 1))
            {
                sides += 1;
            }
        }
        sides
    }
}

fn main() {
    let input = "../input.txt";
    let input_file = std::fs::read_to_string(input).expect("Error reading input file");
    let garden: Vec<Vec<char>> = input_file
        .lines()
        .map(|line| line.chars().collect())
        .collect();
    let rows = garden.len();
    let cols = garden[0].len();
    let mut visited = vec![vec![false; cols]; rows];
    let directions: [(isize, isize); 4] = [(0, 1), (0, -1), (1, 0), (-1, 0)];
    let mut regions = Vec::new();
    for row in 0..rows {
        for col in 0..cols {
            if visited[row][col] {
                continue;
            }
            let plant_type = garden[row][col];
            let mut area = 0;
            let mut cells = HashSet::new();
            let mut queue = VecDeque::new();
            queue.push_back((row, col));
            visited[row][col] = true;
            while let Some((r, c)) = queue.pop_front() {
                area += 1;
                cells.insert((r, c));

                for (dr, dc) in directions.iter() {
                    let new_r = r as isize + dr;
                    let new_c = c as isize + dc;

                    if new_r >= 0 && new_r < rows as isize && new_c >= 0 && new_c < cols as isize {
                        let new_r = new_r as usize;
                        let new_c = new_c as usize;

                        if !visited[new_r][new_c] && garden[new_r][new_c] == plant_type {
                            visited[new_r][new_c] = true;
                            queue.push_back((new_r, new_c));
                        }
                    }
                }
            }

            regions.push(Region {
                plant_type,
                area,
                cells,
            });
        }
    }
    println!(
        "{}",
        regions
            .iter()
            .map(|region| region.area * region.sides())
            .sum::<usize>()
    );
}
