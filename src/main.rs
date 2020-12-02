#![allow(dead_code)]

use std::fs;
use std::collections::HashSet;
use regex::Regex;

fn read_file(file: &str) -> String {
    fs::read_to_string(file).unwrap()
}

fn read_lines(file: &str) -> Vec<String> {
    read_file(file).split('\n')
                   .filter(|x| !x.is_empty())
                   .map(String::from)
                   .collect()
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

struct PasswordEntry {
    min : usize,
    max : usize,
    character : char,
    password : String,
}

impl PasswordEntry {
    fn new(input: &str) -> PasswordEntry {
        let re = Regex::new(r"^([0-9]+)-([0-9]+) ([a-z]): ([a-z]+)$").unwrap();
        let caps = re.captures(&input).unwrap();
        PasswordEntry {
            min : caps[1].parse::<usize>().unwrap(),
            max : caps[2].parse::<usize>().unwrap(),
            character : caps[3].parse::<char>().unwrap(),
            password : caps[4].to_string(),
        }
    }

    fn is_valid(&self) -> bool {
        let count = self.password.chars().filter(|x| *x == self.character).count();
        count >= self.min && count <= self.max
    }

    fn is_valid_new(&self) -> bool {
        let c1 = self.password.chars().nth(self.min-1).unwrap();
        let c2 = self.password.chars().nth(self.max-1).unwrap();
        c1 != c2 && (c1 == self.character || c2 == self.character)
    }
}

fn day2() {
    let input : Vec<PasswordEntry> = read_lines("input02")
                                     .iter()
                                     .map(|x| PasswordEntry::new(&x))
                                     .collect();
    let count = input.iter()
                     .filter(|x| x.is_valid())
                     .count();
    println!("2a: {}", count);

    let count = input.iter()
                     .filter(|x| x.is_valid_new())
                     .count();
    println!("2b: {}", count);
}

fn main() {
    day2();
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

    #[test]
    fn test_day2() {
        let lines = ["1-3 a: abcde",
                     "1-3 b: cdefg",
                     "2-9 c: ccccccccc"];
        let entries : Vec<PasswordEntry> = lines.iter().map(|x| PasswordEntry::new(&x)).collect();
        assert_eq!(entries[0].is_valid(), true);
        assert_eq!(entries[1].is_valid(), false);
        assert_eq!(entries[2].is_valid(), true);

        assert_eq!(entries[0].is_valid_new(), true);
        assert_eq!(entries[1].is_valid_new(), false);
        assert_eq!(entries[2].is_valid_new(), false);

    }
}
