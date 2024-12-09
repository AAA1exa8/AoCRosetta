#[derive(Debug, Clone)]
struct File {
    id: usize,
    size: usize,
    start: usize,
    end: usize,
}

fn main() {
    let input = "../input.txt";
    let input_file = std::fs::read_to_string(input).expect("Error reading input file");
    let input_file = input_file.trim();

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

    let mut files = identify_files(&disk);

    files.sort_by(|a, b| b.id.cmp(&a.id));

    for file in files.clone() {
        let free_spans = find_free_spans(&disk);

        let candidate_span = free_spans
            .iter()
            .filter(|&&(start, end)| end <= file.start && (end - start) >= file.size)
            .min_by_key(|&&(start, _)| start);

        if let Some(&(span_start, _)) = candidate_span {
            move_file(&mut disk, &file, span_start);

            update_file_position(&mut files, file.id, span_start);
        }
    }

    let checksum = compute_checksum(&disk);

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

fn identify_files(disk: &Vec<Option<usize>>) -> Vec<File> {
    let mut files: Vec<File> = Vec::new();
    let mut current_id: Option<usize> = None;

    for (i, block) in disk.iter().enumerate() {
        match block {
            Some(id) => {
                if Some(*id) != current_id {
                    if let Some(_) = current_id {
                        if let Some(last_file) = files.last_mut() {
                            last_file.end = i;
                            last_file.size = last_file.end - last_file.start;
                        }
                    }

                    current_id = Some(*id);
                    files.push(File {
                        id: *id,
                        size: 1,
                        start: i,
                        end: i + 1,
                    });
                } else {
                    if let Some(last_file) = files.last_mut() {
                        last_file.size += 1;
                        last_file.end = i + 1;
                    }
                }
            }
            None => {
                if let Some(_) = current_id {
                    if let Some(last_file) = files.last_mut() {
                        last_file.end = i;
                        last_file.size = last_file.end - last_file.start;
                    }
                    current_id = None;
                }
            }
        }
    }

    if let Some(_) = current_id {
        if let Some(last_file) = files.last_mut() {
            last_file.end = disk.len();
            last_file.size = last_file.end - last_file.start;
        }
    }

    files
}

fn find_free_spans(disk: &Vec<Option<usize>>) -> Vec<(usize, usize)> {
    let mut spans = Vec::new();
    let mut in_span = false;
    let mut span_start = 0;

    for (i, block) in disk.iter().enumerate() {
        if block.is_none() {
            if !in_span {
                in_span = true;
                span_start = i;
            }
        } else {
            if in_span {
                spans.push((span_start, i));
                in_span = false;
            }
        }
    }

    if in_span {
        spans.push((span_start, disk.len()));
    }

    spans
}

fn move_file(disk: &mut Vec<Option<usize>>, file: &File, target_start: usize) {
    for i in file.start..file.end {
        disk[i] = None;
    }

    for i in 0..file.size {
        disk[target_start + i] = Some(file.id);
    }
}

fn update_file_position(files: &mut Vec<File>, file_id: usize, new_start: usize) {
    if let Some(file) = files.iter_mut().find(|f| f.id == file_id) {
        file.start = new_start;
        file.end = new_start + file.size;
    }
}

fn compute_checksum(disk: &Vec<Option<usize>>) -> usize {
    disk.iter()
        .enumerate()
        .filter_map(|(i, block)| block.map(|id| i * id))
        .sum()
}
