fn main() {
    let input = "../../input.txt";
    let input_file = std::fs::read_to_string(input).expect("Error reading input file");
    let (nav, net) = input_file.split_once("\n\n").expect("skill issue");
    let net = net
        .trim()
        .split('\n')
        .map(|node| {
            let (name, children) = node.split_once(" = ").expect("skill issue");
            let (l_child, r_child) = children
                .trim_start_matches('(')
                .trim_end_matches(')')
                .split_once(", ")
                .expect("skill issue");
            Node {
                name: name.to_string(),
                l_child: l_child.to_string(),
                r_child: r_child.to_string(),
            }
        })
        .collect::<Vec<_>>();
    let output = net
        .iter()
        .filter(|node| node.name.chars().nth(2).unwrap() == 'A')
        .map(|node| get_steps(nav, &net, &node))
        .collect::<Vec<_>>();
    let output = lcm_of_array(&output);
    println!("{}", output);
}

#[derive(Debug)]
struct Node {
    name: String,
    l_child: String,
    r_child: String,
}

fn gcd(a: usize, b: usize) -> usize {
    if b == 0 {
        a
    } else {
        gcd(b, a % b)
    }
}

fn lcm(a: usize, b: usize) -> usize {
    (a * b) / gcd(a, b)
}

fn lcm_of_array(numbers: &[usize]) -> usize {
    numbers
        .first()
        .map(|&first| numbers.iter().skip(1).fold(first, |acc, &x| lcm(acc, x)))
        .unwrap()
}

fn get_steps(nav: &str, net: &[Node], starting_node: &Node) -> usize {
    let mut output = 0;
    let mut node = starting_node;
    while node.name.chars().nth(2).unwrap() != 'Z' {
        let step = nav.chars().nth(output % nav.len()).expect("skill issue");
        match step {
            'L' => {
                node = &net
                    .iter()
                    .find(|i_node| i_node.name == node.l_child)
                    .expect("L not found")
            }
            'R' => {
                node = &net
                    .iter()
                    .find(|i_node| i_node.name == node.r_child)
                    .expect("R not found")
            }
            _ => panic!("skill issue"),
        }
        output += 1;
    }
    output
}
