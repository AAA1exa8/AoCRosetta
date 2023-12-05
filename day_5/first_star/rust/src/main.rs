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
    let maps: Vec<Vec<(i64, i64, i64)>> = iter
        .map(|f| 
            f.split("\n").skip(1).map(|f| {
                let mut b = f.split(" ");
                (
                    b.next().unwrap().parse::<i64>().unwrap(),
                    b.next().unwrap().parse::<i64>().unwrap(),
                    b.next().unwrap().parse::<i64>().unwrap(),
                )
            }).collect::<Vec<(i64, i64, i64)>>())
        .collect::<Vec<Vec<(i64, i64, i64)>>>();
    let output = seeds.iter().map(|f|{
        let mut fin = f.clone();
        for map in maps.iter() {
            for (dest, source, len) in map.iter() {
                if fin >= *source && fin < (source + len) {
                    fin = fin + (dest - source);
                    break;
                }
            }
        }
        fin
    }).min().unwrap();
    println!("{}", output);
}
