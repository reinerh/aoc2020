#![allow(dead_code)]

use std::fs;
use std::hash::{Hash, Hasher};
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

#[derive(Debug)]
struct Passport {
    byr: Option<u32>,
    iyr: Option<u32>,
    eyr: Option<u32>,
    hgt: Option<String>,
    hcl: Option<String>,
    ecl: Option<String>,
    pid: Option<String>,
    cid: Option<u32>,
}

impl Passport {
    fn parse_existence(data: &str) -> Passport {
        let data = data.replace("\n", " ");
        let re_byr = r"byr:(.)";
        let re_iyr = r"iyr:(.)";
        let re_eyr = r"eyr:(.)";
        let re_hgt = r"hgt:(.)";
        let re_hcl = r"hcl:(.)";
        let re_ecl = r"ecl:(.)";
        let re_pid = r"pid:(.)";
        let re_cid = r"cid:(.)";

        Passport {
            byr : Passport::parse_u32(&data, re_byr),
            iyr : Passport::parse_u32(&data, re_iyr),
            eyr : Passport::parse_u32(&data, re_eyr),
            hgt : Passport::parse_str(&data, re_hgt),
            hcl : Passport::parse_str(&data, re_hcl),
            ecl : Passport::parse_str(&data, re_ecl),
            pid : Passport::parse_str(&data, re_pid),
            cid : Passport::parse_u32(&data, re_cid),
        }
    }

    fn parse(data: &str) -> Passport {
        let data = data.replace("\n", " ") + " ";
        let re_byr = r"byr:([0-9]{4}) ";
        let re_iyr = r"iyr:([0-9]{4}) ";
        let re_eyr = r"eyr:([0-9]{4}) ";
        let re_hgt = r"hgt:((1[5-8][0-9]cm)|(19[0-3]cm)|(59in)|(6[0-9]in)|7[0-6]in) ";
        let re_hcl = r"hcl:(#[0-9a-f]{6}) ";
        let re_ecl = r"ecl:(amb|blu|brn|gry|grn|hzl|oth) ";
        let re_pid = r"pid:([0-9]{9}) ";
        let re_cid = r"cid:([0-9]+) ";

        Passport {
            byr : Passport::parse_u32(&data, re_byr),
            iyr : Passport::parse_u32(&data, re_iyr),
            eyr : Passport::parse_u32(&data, re_eyr),
            hgt : Passport::parse_str(&data, re_hgt),
            hcl : Passport::parse_str(&data, re_hcl),
            ecl : Passport::parse_str(&data, re_ecl),
            pid : Passport::parse_str(&data, re_pid),
            cid : Passport::parse_u32(&data, re_cid),
        }
    }

    fn parse_u32(data: &str, pattern: &str) -> Option<u32> {
        let re = Regex::new(pattern).unwrap();
        match re.captures(data) {
            Some(caps) => Some(caps.get(1).unwrap().as_str().parse::<u32>().unwrap()),
            None => None,
        }
    }

    fn parse_str(data: &str, pattern: &str) -> Option<String> {
        let re = Regex::new(pattern).unwrap();
        match re.captures(data) {
            Some(caps) => Some(caps.get(1).unwrap().as_str().to_string()),
            None => None,
        }
    }

    fn fields_exist(&self) -> bool {
        self.byr.is_some() && self.iyr.is_some() && self.eyr.is_some()
        && self.hgt.is_some() && self.hcl.is_some() && self.ecl.is_some()
        && self.pid.is_some()
    }

    fn is_valid(&self) -> bool {
        let byr = match self.byr {
            Some(x) => x,
            None => return false,
        };
        let iyr = match self.iyr {
            Some(x) => x,
            None => return false,
        };
        let eyr = match self.eyr {
            Some(x) => x,
            None => return false,
        };

        self.fields_exist() && byr >= 1920 && byr <= 2002
            && iyr >= 2010 && iyr <= 2020 && eyr >= 2020 && eyr <= 2030
    }
}

fn day4() {
    let input = read_file("input04");
    let count = input.split("\n\n")
                     .map(Passport::parse_existence)
                     .filter(|x| x.fields_exist())
                     .count();
    println!("4a: {}", count);

    let count = input.split("\n\n")
                     .map(Passport::parse)
                     .filter(|x| x.is_valid())
                     .count();
    println!("4b: {}", count);
}

#[derive(Eq, PartialEq, Hash)]
struct BoardingPass {
    row: u32,
    column: u32,
}

impl BoardingPass {
    fn new(input: &str) -> BoardingPass {
        let input = input.replace("B", "1")
                         .replace("F", "0")
                         .replace("R", "1")
                         .replace("L", "0");
        let value = u32::from_str_radix(&input, 2).unwrap();
        BoardingPass {
            row: value >> 3,
            column: value & 0x7,
        }
    }

    fn seat_id(&self) -> u32 {
        self.row * 8 + self.column
    }
}

fn day5() {
    let input = read_lines("input05");
    let mut seats = HashSet::new();
    for i in input {
        seats.insert(BoardingPass::new(&i));
    }
    let seat_id = seats.iter()
                       .map(|x| x.seat_id())
                       .max()
                       .unwrap();
    println!("5a: {}", seat_id);

    let mut missing = HashSet::new();
    for row in 0..=127 {
        for column in 0..=7 {
            let bp = BoardingPass { row, column };
            if !seats.contains(&bp) {
                missing.insert(bp.seat_id());
            }
        }
    }
    let your_seat = missing.iter()
                           .find(|x| !missing.contains(&(*x + 1)) && !missing.contains(&(*x - 1)))
                           .unwrap();
    println!("5b: {}", your_seat);
}

fn count_unique_answers(input: &str) -> usize {
    let mut chars = HashSet::new();
    for c in input.chars().filter(|x| x.is_ascii_lowercase()) {
        chars.insert(c);
    }
    chars.len()
}

fn count_common_answers(input: &str) -> usize {
    let mut answers = HashMap::new();
    let mut person_count = 0;
    for person in input.trim_end().split('\n') {
        person_count += 1;
        for answer in person.chars() {
            let count = answers.entry(answer).or_insert(0);
            *count += 1;
        }
    }
    answers.values().filter(|&x| *x == person_count).count()
}

fn day6() {
    let input = read_file("input06");
    let sum : usize = input.split("\n\n")
                           .map(|x| count_unique_answers(x))
                           .sum();
    println!("6a: {}", sum);

    let sum : usize = input.split("\n\n")
                           .map(|x| count_common_answers(x))
                           .sum();
    println!("6b: {}", sum);
}

#[derive(Clone, Debug)]
struct Bag {
    color: String,
    content: HashMap<String, u32>,
}

impl Bag {
    fn new(description: &str) -> Bag {
        let re = Regex::new(r"([0-9]+) ([a-z ]+) bags?").unwrap();
        let recipe : Vec<String> = description.split(" bags contain ")
                                              .map(String::from)
                                              .collect();
        assert_eq!(recipe.len(), 2);
        let mut content = HashMap::new();
        for cap in re.captures_iter(&recipe[1]) {
            let amount = cap[1].parse::<u32>().unwrap();
            let color = &cap[2];
            content.insert(String::from(color), amount);
        }
        Bag {
            color: recipe[0].clone(),
            content,
        }
    }

    fn can_hold(&self, color: &str) -> bool {
        self.content.contains_key(color)
    }
}

impl Hash for Bag {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.color.hash(state);
    }
}

impl PartialEq for Bag {
    fn eq(&self, other: &Self) -> bool {
        self.color == other.color
    }
}

impl Eq for Bag {}

fn bags_can_hold(bags: &HashSet<Bag>, search: &str) -> HashSet<String> {
    let mut holders = HashSet::new();

    for bag in bags {
        if bag.can_hold(search) {
            holders.insert(bag.color.clone());
            let outers = bags_can_hold(bags, &bag.color);
            for outer_bag in outers {
                holders.insert(outer_bag);
            }
        }
    }

    holders
}

fn count_contained_bags(bags: &HashSet<Bag>, search: &str) -> u32 {
    let mut count = 0;

    let bag = bags.iter().find(|x| x.color == search).unwrap();
    for (color, amount) in &bag.content {
        count += amount * count_contained_bags(bags, &color);
    }

    /* including itself */
    count + 1
}

fn day7() {
    let input = read_lines("input07");
    let mut bags = HashSet::new();
    for line in input {
        bags.insert(Bag::new(&line));
    }
    let holders = bags_can_hold(&bags, "shiny gold");
    println!("7a: {}", holders.len());

    println!("7b: {}", count_contained_bags(&bags, "shiny gold") - 1);

}

#[derive(Copy, Clone, Debug)]
enum Instruction {
    NOP(i32),
    ACC(i32),
    JMP(i32),
}

impl Instruction {
    fn new(input: &str) -> Instruction {
        let operands : Vec<&str> = input.split(' ').collect();
        let (instruction, operand) = (operands[0], operands[1].parse::<i32>().unwrap());
        match instruction {
            "nop" => Instruction::NOP(operand),
            "acc" => Instruction::ACC(operand),
            "jmp" => Instruction::JMP(operand),
            _ => panic!("unsupported instruction: {}", instruction),
        }
    }
}

struct GameConsole {
    instructions: Vec<Instruction>,
    acc: i32,
    pc: i32,
}

impl GameConsole {
    fn new(input: &[String]) -> GameConsole {
        let mut instructions = Vec::new();
        for line in input {
            instructions.push(Instruction::new(&line));
        }
        GameConsole {
            instructions,
            acc: 0,
            pc: 0,
        }
    }

    fn step(&mut self) {
        let instruction = self.instructions[self.pc as usize];
        match instruction {
            Instruction::NOP(_) => {
                self.pc += 1;
            },
            Instruction::ACC(val) => {
                self.acc += val;
                self.pc += 1;
            }
            Instruction::JMP(val) => {
                self.pc += val;
            }
        }
    }

    fn find_loop(&mut self) -> Option<i32> {
        let mut visited = HashSet::new();
        loop {
            let old_acc = self.acc;
            self.step();
            if self.pc >= self.instructions.len() as i32 {
                return None;
            }
            if visited.contains(&self.pc) {
                return Some(old_acc);
            }
            visited.insert(self.pc);
        }
    }

    fn terminates(&mut self) -> bool {
        self.find_loop().is_none()
    }

    fn reset(&mut self) {
        self.acc = 0;
        self.pc = 0;
    }

    fn repair(&mut self) -> i32 {
        for (idx, instruction) in self.instructions.iter().enumerate() {
            let replacement = match instruction {
                Instruction::NOP(val) => Instruction::JMP(*val),
                Instruction::ACC(_) => continue,
                Instruction::JMP(val) => Instruction::NOP(*val),
            };
            let mut console = GameConsole {
                instructions: self.instructions.clone(),
                acc: 0,
                pc: 0,
            };
            console.instructions[idx] = replacement;
            if console.terminates() {
                return console.acc;
            }
        }
        panic!("nothing found");
    }
}

fn day8() {
    let input = read_lines("input08");
    let mut console = GameConsole::new(&input);

    println!("8a: {}", console.find_loop().unwrap());

    console.reset();
    println!("8b: {}", console.repair());
}

fn main() {
    day8();
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

    #[test]
    fn test_day4() {
        let input = "ecl:gry pid:860033327 eyr:2020 hcl:#fffffd\n\
                     byr:1937 iyr:2017 cid:147 hgt:183cm\n\
                     \n\
                     iyr:2013 ecl:amb cid:350 eyr:2023 pid:028048884\n\
                     hcl:#cfa07d byr:1929\n\
                     \n\
                     hcl:#ae17e1 iyr:2013\n\
                     eyr:2024\n\
                     ecl:brn pid:760753108 byr:1931\n\
                     hgt:179cm\n\
                     \n\
                     hcl:#cfa07d eyr:2025 pid:166559648\n\
                     iyr:2011 ecl:brn hgt:59in";
        let count = input.split("\n\n")
                         .map(Passport::parse_existence)
                         .filter(|x| x.fields_exist())
                         .count();
        assert_eq!(count, 2);

        assert_eq!(Passport::parse("eyr:1972 cid:100 hcl:#18171d ecl:amb hgt:170 pid:186cm iyr:2018 byr:1926").is_valid(), false);
        assert_eq!(Passport::parse("iyr:2019 hcl:#602927 eyr:1967 hgt:170cm ecl:grn pid:012533040 byr:1946").is_valid(), false);
        assert_eq!(Passport::parse("hcl:dab227 iyr:2012 ecl:brn hgt:182cm pid:021572410 eyr:2020 byr:1992 cid:277").is_valid(), false);
        assert_eq!(Passport::parse("hgt:59cm ecl:zzz eyr:2038 hcl:74454a iyr:2023 pid:3556412378 byr:2007").is_valid(), false);

        assert_eq!(Passport::parse("pid:087499704 hgt:74in ecl:grn iyr:2012 eyr:2030 byr:1980 hcl:#623a2f").is_valid(), true);
        assert_eq!(Passport::parse("eyr:2029 ecl:blu cid:129 byr:1989 iyr:2014 pid:896056539 hcl:#a97842 hgt:165cm").is_valid(), true);
        assert_eq!(Passport::parse("hcl:#888785 hgt:164cm byr:2001 iyr:2015 cid:88 pid:545766238 ecl:hzl eyr:2022").is_valid(), true);
        assert_eq!(Passport::parse("iyr:2010 hgt:158cm hcl:#b6652a ecl:blu byr:1944 eyr:2021 pid:093154719").is_valid(), true);
    }

    #[test]
    fn test_day5() {
        assert_eq!(BoardingPass::new("FBFBBFFRLR").seat_id(), 357);
        assert_eq!(BoardingPass::new("BFFFBBFRRR").seat_id(), 567);
        assert_eq!(BoardingPass::new("FFFBBBFRRR").seat_id(), 119);
        assert_eq!(BoardingPass::new("BBFFBBFRLL").seat_id(), 820);
    }

    #[test]
    fn test_day6() {
        let input = "abc\n\na\nb\nc\n\nab\nac\n\na\na\na\na\n\nb\n";
        let sum : usize = input.split("\n\n")
                               .map(|x| count_unique_answers(x))
                               .sum();
        assert_eq!(sum, 11);

        let sum : usize = input.split("\n\n")
                               .map(|x| count_common_answers(x))
                               .sum();
        assert_eq!(sum, 6);
    }

    #[test]
    fn test_day7() {
        let input = "light red bags contain 1 bright white bag, 2 muted yellow bags.\n\
                     dark orange bags contain 3 bright white bags, 4 muted yellow bags.\n\
                     bright white bags contain 1 shiny gold bag.\n\
                     muted yellow bags contain 2 shiny gold bags, 9 faded blue bags.\n\
                     shiny gold bags contain 1 dark olive bag, 2 vibrant plum bags.\n\
                     dark olive bags contain 3 faded blue bags, 4 dotted black bags.\n\
                     vibrant plum bags contain 5 faded blue bags, 6 dotted black bags.\n\
                     faded blue bags contain no other bags.\n\
                     dotted black bags contain no other bags.\n";
        let input = read_lines_str(input);
        let mut bags = HashSet::new();
        for line in input {
            bags.insert(Bag::new(&line));
        }
        let holders = bags_can_hold(&bags, "shiny gold");
        assert_eq!(holders.len(), 4);

        let input = "shiny gold bags contain 2 dark red bags.\n\
                     dark red bags contain 2 dark orange bags.\n\
                     dark orange bags contain 2 dark yellow bags.\n\
                     dark yellow bags contain 2 dark green bags.\n\
                     dark green bags contain 2 dark blue bags.\n\
                     dark blue bags contain 2 dark violet bags.\n\
                     dark violet bags contain no other bags.\n";
        let input = read_lines_str(input);
        let mut bags = HashSet::new();
        for line in input {
            bags.insert(Bag::new(&line));
        }
        println!("bags: {:?}", bags);
        assert_eq!(count_contained_bags(&bags, "shiny gold") - 1, 126);
    }

    #[test]
    fn test_day8() {
        let input = "nop +0\n\
                     acc +1\n\
                     jmp +4\n\
                     acc +3\n\
                     jmp -3\n\
                     acc -99\n\
                     acc +1\n\
                     jmp -4\n\
                     acc +6\n";
        let input = read_lines_str(input);
        let mut console = GameConsole::new(&input);
        assert_eq!(console.find_loop().unwrap(), 5);

        console.reset();
        assert_eq!(console.repair(), 8);
    }
}
