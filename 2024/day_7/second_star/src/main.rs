use std::usize;

struct Test {
    test: usize,
    values: Vec<usize>,
}

#[derive(Debug, Clone, Copy)]
enum Op {
    Add,
    Mul,
    Concat,
}

fn main() {
    let input = "../input.txt";
    let input_file = std::fs::read_to_string(input).expect("Error reading input file");
    let values: Vec<Test> = input_file
        .lines()
        .map(|l| l.split_once(": ").unwrap())
        .map(|(t, v)| {
            return Test {
                test: t.parse().unwrap(),
                values: v.split_whitespace().map(|v| v.parse().unwrap()).collect(),
            };
        })
        .collect();

    let mut acc = 0;
    for test in values.iter() {
        let result = try_test(test);
        if result {
            acc = acc + test.test;
        }
    }
    println!("{}", acc);
}

fn try_test(test: &Test) -> bool {
    let operations = vec![Op::Add, Op::Mul, Op::Concat];
    let operations = product(&operations, test.values.len() - 1);
    operations
        .iter()
        .map(|ops| eval(&test.values, ops, test.test))
        .any(|v| v == test.test)
}

fn eval(values: &[usize], operations: &[Op], expected: usize) -> usize {
    let mut acc = values[0];
    for (i, value) in values.iter().enumerate().skip(1) {
        match operations[i - 1] {
            Op::Add => acc = acc + value,
            Op::Mul => acc = acc * value,
            Op::Concat => acc = usize::from_str_radix(&format!("{}{}", acc, value), 10).unwrap(),
        }
        if acc > expected {
            return 0;
        }
    }
    acc
}

fn product<T>(iterable: &[T], repeat: usize) -> Vec<Vec<T>>
where
    T: Copy,
{
    let mut pools: Vec<Vec<T>> = Vec::new();
    for _ in 0..repeat {
        pools.push(iterable.to_vec());
    }

    let mut result: Vec<Vec<T>> = Vec::new();
    result.push(Vec::new());
    for pool in pools.iter() {
        let mut new_result: Vec<Vec<T>> = Vec::new();
        for x in result.iter() {
            for y in pool.iter() {
                let mut new_x = x.clone();
                new_x.push(*y);
                new_result.push(new_x);
            }
        }
        result = new_result;
    }
    result
}
