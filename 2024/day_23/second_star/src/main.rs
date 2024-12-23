use std::collections::HashMap;

struct Graph {
    adjacency: Vec<Vec<usize>>,
    node_map: HashMap<String, usize>,
    node_names: Vec<String>,
}

fn bron_kerbosch(
    graph: &Graph,
    current_clique: &mut Vec<usize>,
    p: &[usize],
    x: &[usize],
    max_clique: &mut Vec<usize>,
) {
    if p.is_empty() && x.is_empty() {
        if current_clique.len() > max_clique.len() {
            *max_clique = current_clique.clone();
        }
        return;
    }
    let pivot = p
        .iter()
        .chain(x.iter())
        .max_by_key(|&&u| graph.adjacency[u].len())
        .cloned()
        .unwrap();
    let pivot_neighbors = &graph.adjacency[pivot];
    let candidates: Vec<usize> = p
        .iter()
        .copied()
        .filter(|v| !pivot_neighbors.contains(v))
        .collect();

    for v in candidates {
        current_clique.push(v);
        let neighbors_v = &graph.adjacency[v];
        let p_new = intersect_sorted(p, neighbors_v);
        let x_new = intersect_sorted(x, neighbors_v);
        bron_kerbosch(graph, current_clique, &p_new, &x_new, max_clique);
        current_clique.pop();
    }
}

fn intersect_sorted(a: &[usize], b: &[usize]) -> Vec<usize> {
    let mut intersection = Vec::new();
    let mut i = 0;
    let mut j = 0;
    while i < a.len() && j < b.len() {
        if a[i] == b[j] {
            intersection.push(a[i]);
            i += 1;
            j += 1;
        } else if a[i] < b[j] {
            i += 1;
        } else {
            j += 1;
        }
    }
    intersection
}

fn main() {
    let input = "../input.txt";
    let input_file = std::fs::read_to_string(input).expect("Failed to read input file");
    let mut graph = Graph {
        adjacency: Vec::new(),
        node_map: HashMap::new(),
        node_names: Vec::new(),
    };
    input_file.lines().for_each(|line| {
        let mut or_insert = |node: &str| {
            let idx = graph.node_names.len();
            graph.node_names.push(node.to_string());
            graph.adjacency.push(Vec::new());
            idx
        };
        let insert_sorted = |adj: &mut Vec<usize>, node: usize| match adj.binary_search(&node) {
            Ok(_) => {}
            Err(pos) => adj.insert(pos, node),
        };
        let (node1, node2) = line.split_once('-').unwrap();
        let idx1 = *graph
            .node_map
            .entry(node1.to_string())
            .or_insert_with(|| or_insert(node1));
        let idx2 = *graph
            .node_map
            .entry(node2.to_string())
            .or_insert_with(|| or_insert(node2));

        insert_sorted(&mut graph.adjacency[idx1], idx2);
        insert_sorted(&mut graph.adjacency[idx2], idx1);
    });

    let mut current_clique: Vec<usize> = Vec::new();
    let mut max_clique: Vec<usize> = Vec::new();
    bron_kerbosch(
        &graph,
        &mut current_clique,
        &(0..graph.node_names.len()).collect::<Vec<_>>(),
        &Vec::new(),
        &mut max_clique,
    );
    let mut max_clique_names: Vec<&String> = max_clique
        .iter()
        .map(|&idx| &graph.node_names[idx])
        .collect();
    max_clique_names.sort();
    println!(
        "{}",
        max_clique_names
            .iter()
            .map(|&s| s.clone())
            .collect::<Vec<String>>()
            .join(",")
    );
}
