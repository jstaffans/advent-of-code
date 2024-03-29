use std::fs;
use itertools::Itertools;

fn main() {
    let input = fs::read_to_string("./input.txt").expect("Could not read file");
    let mut depths: Vec<i32> = vec![];
    
    // Looping
    for l in input.lines() {
        let depth = l.parse().expect("Could not parse depth");
        depths.push(depth)
    }

    let mut increases = 0;
    for i in 1..depths.len() {
        if depths[i] > depths[i-1] {
            increases = increases + 1;
        }
    }

    println!("Increases, Part 1: {}", increases);

    // Functional
    let increases = input
        .lines()
        .filter_map(|s| s.parse::<i32>().ok())
        .tuple_windows()
        .filter(|(prev, curr)| prev < curr)
        .count();
    println!("Increases, Part 1 (second method): {}", increases);

    let mut increases = 0;

    for i in 3..depths.len() {
        let prev_window_sum:i32 = depths[i-3..=i-1].iter().sum();
        let curr_window_sum:i32 = depths[i-2..=i].iter().sum();

        if curr_window_sum > prev_window_sum {
            increases = increases + 1;
        }
    }

    println!("Increases, Part 2: {}", increases);
}
