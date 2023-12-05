use std::cmp::{max, min};

fn main() {
    let input = "../../input.txt";
    let input_file = std::fs::read_to_string(input)
        .expect("Error reading input file")
        .trim()
        .split("\n\n")
        .map(str::to_string)
        .collect::<Vec<String>>();
    let mut iter = input_file.iter();
    let seeds = iter
        .next()
        .unwrap()
        .split(": ")
        .skip(1)
        .next()
        .unwrap()
        .split(" ")
        .map(|f| f.parse::<i64>().unwrap())
        .collect::<Vec<i64>>();
    let seeds = seeds.chunks(2)
        .map(|f| (f[0], f[1]))
        .collect::<Vec<(i64, i64)>>();
    let mut maps: Vec<Vec<(i64, i64, i64)>> = iter
        .map(|f| {
            f.split("\n")
                .skip(1)
                .map(|f| {
                    let mut b = f.split(" ");
                    (
                        b.next().unwrap().parse::<i64>().unwrap(),
                        b.next().unwrap().parse::<i64>().unwrap(),
                        b.next().unwrap().parse::<i64>().unwrap(),
                    )
                })
                .collect::<Vec<(i64, i64, i64)>>()
        })
        .map(|mut f| {
            f.sort_by_key(|x| x.1);
            f
        })
        .collect::<Vec<Vec<(i64, i64, i64)>>>();


    
    let mut output = i64::MAX;
    for (low, len) in seeds.iter() {
        let mut cur = vec![(*low, *len + low - 1)];
        let mut new = Vec::new();
        for map in maps.iter() {
            for (low, high) in cur.iter() {
                for n in transform_low_high((*low, *high), map) {
                    new.push(n);
                }
            }
            cur = new;
            new = Vec::new();
        }
        for (low, high) in cur.iter() {
            output = min(output, *low);
        }
    }
    println!("{}", output);

}

fn transform_low_high((low, high): (i64, i64), map: &Vec<(i64, i64, i64)>) -> Vec<(i64, i64)> {
    let mut shranges = Vec::new();
    let mut result = Vec::new();
    map.iter().for_each(|(dest, source, len)|{
        let end = source + len -1;
        let diff = dest - source;
        if !(source > &high || end < low){
            shranges.push((max(*source, low), min(end, high), diff));
        }
    });
    for (i, (low, high, diff)) in shranges.iter().enumerate(){
        result.push((low + diff, high + diff));
        if i < shranges.len() - 1 && shranges[i+1].0 > *high + 1{
            result.push((high + 1, shranges[i+1].0 - 1));
        }
    }
    if shranges.len() == 0{
        result.push((low, high));
        return result;
    }
    if shranges[0].0 != low{
        result.push((low, shranges[0].0 - 1));
    }
    if shranges[shranges.len() - 1].1 != high{
        result.push((shranges[shranges.len() - 1].1 + 1, high));
    }
    result
}