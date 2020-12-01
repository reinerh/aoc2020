#![allow(dead_code)]

use std::fs;
use std::collections::HashSet;

fn read_file(file: &str) -> String {
    fs::read_to_string(file).unwrap()
}

fn read_numbers(file: &str) -> Vec<i32> {
    read_file(file).split('\n')
                   .filter(|x| !x.is_empty())
                   .map(|x| x.parse::<i32>().unwrap())
                   .collect()
}

fn find_pair_with_sum(numbers: &HashSet<i32>, goal: i32) -> Option<(i32, i32)> {
    for i in numbers {
        if numbers.contains(&(goal - i)) {
            return Some((*i, goal - i));
        }
    }
    None
}

fn day1() {
    let input = read_numbers("input01");
    let mut numbers = HashSet::new();
    for i in &input {
        numbers.insert(*i);
    }
    let (x, y) = match find_pair_with_sum(&numbers, 2020) {
        Some(pair) => pair,
        None => panic!("nothing found"),
    };
    println!("1a: {}", x * y);

    for i in &numbers {
        let (x, y) = match find_pair_with_sum(&numbers, 2020 - i) {
            Some(pair) => pair,
            None => continue,
        };
        println!("1b: {}", i * x * y);
        break;
    }
}

fn main() {
    day1();
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_day1() {
        let input = [1721, 979, 366, 299, 675, 1456];
        let mut numbers = HashSet::new();
        for i in &input {
            numbers.insert(*i);
        }
        let (x, y) = find_pair_with_sum(&numbers, 2020).unwrap();
        assert_eq!(x * y, 514579);

        for i in &numbers {
            let (x, y) = match find_pair_with_sum(&numbers, 2020 - i) {
                Some(pair) => pair,
                None => continue,
            };
            assert_eq!(i + x + y, 2020);
            assert_eq!(i * x * y, 241861950);
            break;
        }
    }
}
