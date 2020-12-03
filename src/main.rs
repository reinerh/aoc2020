#![allow(dead_code)]

use std::fs;
use std::cmp;
use std::collections::{HashSet, HashMap};
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

fn read_lines_str(input: &str) -> Vec<String> {
    input.split('\n')
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

#[derive(Eq, PartialEq, Hash, Clone, Copy)]
struct Point {
    x: usize,
    y: usize,
}

#[derive(Clone, Copy, Eq, PartialEq)]
enum MapObject {
    OPEN,
    TREE,
}

struct TobogganMap {
    map: HashMap<Point, MapObject>,
    height: usize,
    width: usize,
}

impl TobogganMap {
    fn new(input: &[String]) -> TobogganMap {
        let mut map = HashMap::new();
        let mut height = 0;
        let mut width = 0;

        for (y, line) in input.iter().enumerate() {
            for (x, symbol) in line.chars().enumerate() {
                let obj = match symbol {
                    '.' => MapObject::OPEN,
                    '#' => MapObject::TREE,
                    _ => panic!("unsupported object"),
                };
                map.insert(Point{x, y}, obj);
                width = cmp::max(x + 1, width);
            }
            height = cmp::max(y + 1, height);
        }
        TobogganMap { map, height, width }
    }

    fn obj_at(&self, p: Point) -> MapObject {
        self.map[&p]
    }

    fn count_trees(&self, from: &Point, dx: usize, dy: usize) -> usize {
        let mut pos = *from;
        let mut count = 0;
        while pos.y < self.height {
            pos.x += dx;
            pos.y += dy;
            let pos_in_map = Point {
                x: pos.x % self.width,
                y: pos.y % self.height,
            };
            if pos.y < self.height && self.map[&pos_in_map] == MapObject::TREE {
                count += 1;
            }
        }
        count
    }
}

fn day3() {
    let input = read_lines("input03");
    let map = TobogganMap::new(&input);

    let start = Point { x: 0, y: 0 };
    let slope1 = map.count_trees(&start, 3, 1);
    println!("3a: {}", slope1);

    let slope2 = map.count_trees(&start, 1, 1);
    let slope3 = map.count_trees(&start, 5, 1);
    let slope4 = map.count_trees(&start, 7, 1);
    let slope5 = map.count_trees(&start, 1, 2);
    println!("3b: {}", slope1 * slope2 * slope3 * slope4 * slope5);
}

fn main() {
    day3();
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

    #[test]
    fn test_day3() {
        let input = "..##.......\n\
                     #...#...#..\n\
                     .#....#..#.\n\
                     ..#.#...#.#\n\
                     .#...##..#.\n\
                     ..#.##.....\n\
                     .#.#.#....#\n\
                     .#........#\n\
                     #.##...#...\n\
                     #...##....#\n\
                     .#..#...#.#\n";
        let map = TobogganMap::new(&read_lines_str(input));
        let start = Point { x: 0, y: 0 };
        let slope1 = map.count_trees(&start, 3, 1);
        assert_eq!(slope1, 7);

        let slope2 = map.count_trees(&start, 1, 1);
        let slope3 = map.count_trees(&start, 5, 1);
        let slope4 = map.count_trees(&start, 7, 1);
        let slope5 = map.count_trees(&start, 1, 2);

        assert_eq!(slope1 * slope2 * slope3 * slope4 * slope5, 336);
    }
}
