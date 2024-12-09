fn main() {
    let input = "../input.txt";
    let input_file = std::fs::read_to_string(input).expect("Error reading input file");
    let input_file = input_file.trim();
    // let (file_lengths, free_lengths) = parse_disk_map(&input_file);
    // use .reduce
    let mut file_ids = Vec::new();
    let (file_lengths, free_lengths): (Vec<usize>, Vec<usize>) =
        input_file.chars().enumerate().fold(
            (Vec::new(), Vec::new()),
            |(mut file_lengths, mut free_lengths), (i, ch)| {
                if ch.is_digit(10) {
                    let val = ch.to_digit(10).unwrap() as usize;
                    if i % 2 == 0 {
                        file_lengths.push(val);
                        file_ids.push(file_lengths.len() - 1);
                    } else {
                        free_lengths.push(val);
                    }
                }
                (file_lengths, free_lengths)
            },
        );

    let mut disk = expand_disk_map(&file_lengths, &free_lengths, &file_ids);

    while let Some(first_free) = disk.iter().position(|x| x.is_none()) {
        if let Some(rightmost) = (first_free..disk.len()).rev().find(|&i| disk[i].is_some()) {
            let file_id = disk[rightmost].unwrap();
            disk[rightmost] = None;
            disk[first_free] = Some(file_id);
        } else {
            break;
        }
    }

    let checksum: usize = disk
        .iter()
        .enumerate()
        .filter_map(|(i, x)| x.map(|id| i * id))
        .sum();

    println!("{}", checksum);
}

fn expand_disk_map(
    file_lengths: &Vec<usize>,
    free_lengths: &Vec<usize>,
    file_ids: &Vec<usize>,
) -> Vec<Option<usize>> {
    let mut disk = Vec::new();
    let num_files = file_lengths.len();
    for i in 0..num_files {
        for _ in 0..file_lengths[i] {
            disk.push(Some(file_ids[i]));
        }

        if i < free_lengths.len() {
            for _ in 0..free_lengths[i] {
                disk.push(None);
            }
        }
    }
    disk
}
