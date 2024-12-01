fn main() {
    let input = "../../input.txt";
    let input_file = std::fs::read_to_string(input).expect("Error reading input file");
    let (nav, net) = input_file.split_once("\n\n").expect("skill issue");
    let net = net.trim().split('\n').map(|node| {
        let (name, children) = node.split_once(" = ").expect("skill issue");
        let (l_child, r_child) = children.trim_start_matches('(').trim_end_matches(')')
            .split_once(", ").expect("skill issue");
        Node {
            name: name.to_string(),
            l_child: l_child.to_string(),
            r_child: r_child.to_string()
        }
    }).collect::<Vec<_>>();
    let mut output = 0;
    let mut node = net.iter().find(|i_node| i_node.name == "AAA".to_string()).expect("AAA not found");
    while node.name != "ZZZ".to_string() {
        let step = nav.chars().nth(output % nav.len()).expect("skill issue");
        match step {
            'L' => node = &net.iter().find(|i_node| i_node.name == node.l_child).expect("L not found"),
            'R' => node = &net.iter().find(|i_node| i_node.name == node.r_child).expect("R not found"),
            _ => panic!("skill issue")
        }
        output += 1;
    }
    println!("{}", output);
}


#[derive(Debug)]
struct Node {
    name: String,
    l_child: String,
    r_child: String
}