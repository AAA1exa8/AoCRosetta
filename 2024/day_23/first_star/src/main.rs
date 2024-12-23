use std::collections::{HashMap, HashSet};

fn find_triangles(graph: &HashMap<String, HashSet<String>>) -> HashSet<Vec<String>> {
    let mut triangles = HashSet::new();
    let mut nodes: Vec<&String> = graph.keys().collect();
    nodes.sort();

    for &node in &nodes {
        let mut neighbors: Vec<&String> = graph[node].iter().collect();
        neighbors.sort();

        for i in 0..neighbors.len() {
            for j in (i + 1)..neighbors.len() {
                let neighbor1 = neighbors[i];
                let neighbor2 = neighbors[j];

                if graph[neighbor1].contains(neighbor2) {
                    let mut triangle = vec![node.clone(), neighbor1.clone(), neighbor2.clone()];
                    triangle.sort();
                    triangles.insert(triangle);
                }
            }
        }
    }
    triangles
}

fn main() {
    let input = "../input.txt";
    let input_file = std::fs::read_to_string(input).unwrap();
    let mut graph: HashMap<String, HashSet<String>> = HashMap::new();
    input_file.lines().for_each(|line| {
        let trimmed = line.trim();
        if trimmed.is_empty() {
            return;
        }
        let parts: Vec<&str> = trimmed.split('-').collect();
        let node1 = parts[0].to_string();
        let node2 = parts[1].to_string();
        graph
            .entry(node1.clone())
            .or_default()
            .insert(node2.clone());
        graph.entry(node2).or_default().insert(node1);
    });

    let triangles = find_triangles(&graph);
    let res = triangles
        .iter()
        .filter(|x| x.iter().any(|v| v.starts_with("t")))
        .count();
    println!("{}", res);
}
