fn main() {
    let desired_output = vec![2, 4, 1, 2, 7, 5, 4, 5, 1, 3, 5, 5, 0, 3, 3, 0];
    let num_steps = desired_output.len();

    let mut steps: Vec<Vec<u64>> = vec![Vec::new(); num_steps + 1];

    steps[num_steps].push(0);

    for k in (0..num_steps).rev() {
        let desired = desired_output[k];
        let mut current_step = Vec::new();

        for &a_next in &steps[k + 1] {
            for d in 0..8 {
                let a_k = 8 * a_next + d;
                let b_k = d ^ 2;

                let c_k = a_k >> b_k;

                let o_k = (b_k ^ c_k ^ 3) & 7;

                if o_k == desired {
                    current_step.push(a_k);
                }
            }
        }

        steps[k] = current_step;

        if steps[k].is_empty() {
            panic!("No valid initial value for register A found.");
        }
    }

    let a0_candidates = &steps[0];

    let minimal_a0 = a0_candidates.iter().min();

    println!("{}", minimal_a0.unwrap());
}
