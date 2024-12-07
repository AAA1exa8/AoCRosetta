use lru::LruCache;
use rayon::prelude::*;
use std::num::NonZeroUsize;
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
    // for test in values.iter() {
    //     let (_, found) = brute_force(&test.values, test.test, &mut cache);
    //     if found {
    //         acc = acc + test.test;
    //     }
    // }
    acc = values
        .par_iter()
        .map(|test| {
            let size = NonZeroUsize::new(10000).unwrap();
            let mut cache = LruCache::new(size);
            let (_, found) = brute_force(&test.values, test.test, &mut cache);
            if found {
                return test.test;
            }
            return 0;
        })
        .sum();
    println!("{}", acc);
}

// fn brute_force(numbers: &Vec<usize>, cache ) -> Vec<usize> {
//     if numbers.len() == 1 {
//         return vec![numbers[0]];
//     }
//     let mut a = Vec::new();
//     for op in [Op::Add, Op::Mul, Op::Concat].iter() {
//         for i in brute_force(&numbers[..numbers.len() - 1].to_vec()) {
//             a.push(apply_op(numbers[numbers.len() - 1], i, *op));
//         }
//     }
//     return a;
// }

fn brute_force(
    numbers: &Vec<usize>,
    expected: usize,
    cache: &mut LruCache<Vec<usize>, Vec<usize>>,
) -> (Vec<usize>, bool) {
    if numbers.len() == 1 {
        return (vec![numbers[0]], numbers[0] == expected);
    }
    if let Some(v) = cache.get(numbers) {
        return (v.clone(), v.contains(&expected));
    }
    let mut a = Vec::new();
    for op in [Op::Add, Op::Mul, Op::Concat].iter() {
        let (ress, found) = brute_force(&numbers[..numbers.len() - 1].to_vec(), expected, cache);
        if found {
            return (ress, true);
        }
        for i in ress {
            a.push(apply_op(numbers[numbers.len() - 1], i, *op));
        }
    }
    cache.put(numbers.clone(), a.clone());
    let found = a.contains(&expected);
    return (a, found);
}

fn apply_op(a: usize, b: usize, op: Op) -> usize {
    match op {
        Op::Add => a + b,
        Op::Mul => a * b,
        Op::Concat => b * 10usize.pow(a.ilog10() + 1) + a,
    }
}

// ops = [
//     lambda a, b: a + b,
//     lambda a, b: a * b,
//     lambda a, b: int(str(a) + str(b))
// ]
//
// @functools.lru_cache(maxsize=None)
// def bruteforce(numbers):
//     if len(numbers) == 1:
//         return [numbers[0]]
//     a = []
//     for op in ops:
//         for i in bruteforce(numbers[:-1]):
//             a.append(op(numbers[-1], i))
//     return tuple(a)
//
