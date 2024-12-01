fn main() {
    let input = "../../input.txt";
    let mut forecat = std::fs::read_to_string(input)
        .expect("Error reading input file")
        .lines()
        .map(|line| {
            line.split(' ')
                .map(|f| f.parse::<isize>().unwrap())
                .collect::<Vec<_>>()
        })
        .collect::<Vec<_>>();
    let output: isize = forecat
        .iter_mut()
        .map(|f| {
            f.reverse();
            get_next(f)
        })
        .sum();
    println!("{}", output);
}

fn get_next(forecast: &[isize]) -> isize {
    let mut layers: Vec<Vec<_>> = vec![forecast.into()];
    while layers.last().unwrap().iter().any(|f| *f != 0) {
        let mut next_layer = Vec::new();
        let index = 0;
        for (i, f) in layers.last().unwrap().iter().enumerate() {
            if i == 0 {
                continue;
            }
            next_layer.push(f - layers.last().unwrap()[i - 1]);
        }
        layers.push(next_layer);
    }
    let mut next = 0;
    for (i, f) in layers.iter().rev().enumerate() {
        if i == 0 {
            continue;
        }
        next += f.last().unwrap();
    }
    next
}
