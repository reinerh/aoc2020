#![allow(dead_code)]

use std::fs;
use std::hash::{Hash, Hasher};
use std::cmp;
use std::collections::{HashSet, HashMap, VecDeque};
use std::iter::FromIterator;
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

fn read_numbers_64(file: &str) -> Vec<i64> {
    read_file(file).split('\n')
                   .filter(|x| !x.is_empty())
                   .map(|x| x.parse::<i64>().unwrap())
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

#[derive(Eq, PartialEq, Hash, Clone, Copy, Debug)]
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

fn find_xmas_number(input: &[i64], prev_count: usize) -> i64 {
    let mut queue = VecDeque::from_iter(input.iter().copied().take(prev_count));

    for number in input.iter().skip(prev_count) {
        match queue.iter().find(|x| queue.contains(&(number - *x))) {
            None => return *number,
            Some(_) => {
                queue.pop_front();
                queue.push_back(*number);
            },
        }
    }
    panic!("nothing found");
}

fn find_xmas_weakness(input: &[i64], goal: i64) -> i64 {
    for (i, _) in input.iter().enumerate() {
        let mut min = goal;
        let mut max = 0;
        let mut sum = 0;
        for val_j in input.iter().skip(i) {
            min = cmp::min(min, *val_j);
            max = cmp::max(max, *val_j);
            sum += val_j;
            if sum == goal {
                return min + max;
            }
        }
    }
    panic!("nothing found");
}

fn day9() {
    let input = read_numbers_64("input09");

    let number = find_xmas_number(&input, 25);
    println!("9a: {}", number);

    println!("9b: {}", find_xmas_weakness(&input, number));
}

fn count_jolt_differences(input: &[i32]) -> (i32, i32) {
    let mut adapters = HashSet::new();
    for i in input {
        adapters.insert(*i);
    }
    let mut diff1 = 0;
    let mut diff3 = 0;
    let max_jolts = *adapters.iter().max().unwrap();

    let mut adapter = 0;
    loop {
        if adapters.contains(&(adapter + 1)) {
            diff1 += 1;
            adapter += 1;
            continue;
        }
        if adapters.contains(&(adapter + 2)) {
            adapter += 2;
            continue;
        }
        if adapters.contains(&(adapter + 3)) {
            diff3 += 1;
            adapter += 3;
            continue;
        }
        if adapter >= max_jolts {
            break;
        }
        adapter += 1;
    }
    diff3 += 1; /* difference to device adapter */
    (diff1, diff3)
}

fn count_jolt_paths(map: &HashMap::<i32, Vec<i32>>, path_cache: &mut HashMap::<i32, i64>, from: i32, goal: i32) -> i64 {
    if from == goal {
        return 1;
    }
    if path_cache.contains_key(&from) {
        return path_cache[&from];
    }

    let mut count = 0;
    for next in &map[&from] {
        count += count_jolt_paths(map, path_cache, *next, goal);
    }

    path_cache.insert(from, count);

    count
}

fn build_jolt_map(input: &[i32]) -> HashMap::<i32, Vec<i32>> {
    let mut joltmap = HashMap::new();
    let mut input = Vec::from_iter(input.iter().copied());
    input.push(0);
    for i in &input {
        if input.contains(&(i+1)) {
            joltmap.entry(*i).or_insert(Vec::new()).push(*i+1);
        }
        if input.contains(&(i+2)) {
            joltmap.entry(*i).or_insert(Vec::new()).push(*i+2);
        }
        if input.contains(&(i+3)) {
            joltmap.entry(*i).or_insert(Vec::new()).push(*i+3);
        }
    }
    joltmap
}

fn day10() {
    let input = read_numbers("input10");

    let (diff1, diff3) = count_jolt_differences(&input);
    println!("10a: {}", diff1 * diff3);

    let max_jolts = *input.iter().max().unwrap();
    let map = build_jolt_map(&input);
    let mut path_cache = HashMap::new();
    let paths = count_jolt_paths(&map, &mut path_cache, 0, max_jolts);
    println!("10b: {}", paths);
}

#[derive(PartialEq, Eq, Copy, Clone, Debug)]
enum SeatingObject {
    FLOOR,
    EMPTY,
    OCCUPIED,
}

struct SeatingMap {
    map: HashMap<Point, SeatingObject>,
    dimensions: Point,
}

impl SeatingMap {
    fn new(input: &[String]) -> SeatingMap {
        let mut map = HashMap::new();
        let mut dimensions = Point { x: 0, y: 0 };
        for (y, line) in input.iter().enumerate() {
            for (x, symbol) in line.chars().enumerate() {
                let obj = match symbol {
                    '.' => SeatingObject::FLOOR,
                    'L' => SeatingObject::EMPTY,
                    '#' => SeatingObject::OCCUPIED,
                    _ => panic!("unsupported object"),
                };
                map.insert(Point{x, y}, obj);
                dimensions.x = cmp::max(dimensions.x, x + 1);
            }
            dimensions.y = cmp::max(dimensions.y, y + 1);
        }
        SeatingMap { map, dimensions }
    }

    fn count_adjacent_occupied(&self, pos: &Point, part2: bool) -> u32 {
        match part2 {
            false => self.count_adjacent_occupied1(pos),
            true  => self.count_adjacent_occupied2(pos),
        }
    }

    fn count_adjacent_occupied1(&self, pos: &Point) -> u32 {
        let mut count = 0;
        for x in 0..=2 {
            if pos.x + x == 0 {
                continue;
            }
            for y in 0..=2 {
                if (x, y) == (1, 1) {
                    /* skip center point */
                    continue
                }
                if pos.y + y == 0 {
                    continue;
                }
                let pos_check = Point {
                    x: pos.x + x - 1,
                    y: pos.y + y - 1
                };
                if let Some(SeatingObject::OCCUPIED) = self.map.get(&pos_check) {
                    count += 1;
                }
            }
        }
        count
    }

    fn seat_at_direction(&self, pos: &Point, dx: i32, dy: i32) -> SeatingObject {
        let mut pos = *pos;
        loop {
            if pos.x == 0 && dx < 0 { break; }
            if pos.y == 0 && dy < 0 { break; }
            if pos.x == self.dimensions.x - 1 && dx > 0 { break; }
            if pos.y == self.dimensions.y - 1 && dy > 0 { break; }
            pos = Point {
                x: ((pos.x as i32) + dx) as usize,
                y: ((pos.y as i32) + dy) as usize,
            };
            match &self.map[&pos] {
                SeatingObject::FLOOR => {},
                state => return *state,
            }
        }
        SeatingObject::FLOOR
    }

    fn count_adjacent_occupied2(&self, pos: &Point) -> u32 {
        let mut count = 0;

        if self.seat_at_direction(pos, -1, -1) == SeatingObject::OCCUPIED { count += 1 };
        if self.seat_at_direction(pos,  0, -1) == SeatingObject::OCCUPIED { count += 1 };
        if self.seat_at_direction(pos,  1, -1) == SeatingObject::OCCUPIED { count += 1 };
        if self.seat_at_direction(pos, -1,  0) == SeatingObject::OCCUPIED { count += 1 };
        if self.seat_at_direction(pos,  1,  0) == SeatingObject::OCCUPIED { count += 1 };
        if self.seat_at_direction(pos, -1,  1) == SeatingObject::OCCUPIED { count += 1 };
        if self.seat_at_direction(pos,  0,  1) == SeatingObject::OCCUPIED { count += 1 };
        if self.seat_at_direction(pos,  1,  1) == SeatingObject::OCCUPIED { count += 1 };

        count
    }

    fn dump_map(&self) {
        for y in 0..self.dimensions.y {
            for x in 0..self.dimensions.x {
                let pos = Point {x, y};
                let char = match self.map[&pos] {
                    SeatingObject::FLOOR => '.',
                    SeatingObject::EMPTY => 'L',
                    SeatingObject::OCCUPIED => '#',
                };
                print!("{}", char);
            }
            println!();
        }
        println!();
    }

    fn step(&mut self, part2: bool) -> HashMap<Point, SeatingObject> {
        let mut new_map = HashMap::new();
        let occupied_required = if part2 { 5 } else { 4 };
        for (pos, obj) in &self.map {
            let new_state = match *obj {
                SeatingObject::FLOOR => SeatingObject::FLOOR,
                SeatingObject::EMPTY => {
                    if self.count_adjacent_occupied(pos, part2) == 0 {
                        SeatingObject::OCCUPIED
                    } else {
                        SeatingObject::EMPTY
                    }
                },
                SeatingObject::OCCUPIED => {
                    if self.count_adjacent_occupied(pos, part2) >= occupied_required {
                        SeatingObject::EMPTY
                    } else {
                        SeatingObject::OCCUPIED
                    }
                }
            };
            new_map.insert(*pos, new_state);
        }
        new_map
    }

    fn stabilize(&mut self, part2: bool) {
        loop {
            let new_map = self.step(part2);
            if new_map == self.map {
                break;
            }
            self.map = new_map;
        }
    }

    fn count_occupied(&self) -> usize {
        self.map.iter()
                .filter(|(_, state)| *state == &SeatingObject::OCCUPIED)
                .count()
    }
}

fn day11() {
    let input = read_lines("input11");

    let mut map = SeatingMap::new(&input);
    map.stabilize(false);
    println!("11a: {}", map.count_occupied());

    let mut map = SeatingMap::new(&input);
    map.stabilize(true);
    println!("11b: {}", map.count_occupied());
}

#[derive(Clone, Copy)]
enum ShipAction {
    NORTH(i32),
    SOUTH(i32),
    EAST(i32),
    WEST(i32),
    LEFT(i32),
    RIGHT(i32),
    FORWARD(i32),
}

struct Waypoint {
    x: i32,
    y: i32,
}

struct Ship {
    x: i32,
    y: i32,
    direction: i32,
    actions: Vec<ShipAction>,
    waypoint: Waypoint,
}

impl Ship {
    fn new(input: &[String]) -> Ship {
        let mut actions = Vec::new();
        for string in input {
            let (ch, val) = string.split_at(1);
            let val = val.parse::<i32>().unwrap();
            let action = match ch {
                "N" => ShipAction::NORTH(val),
                "S" => ShipAction::SOUTH(val),
                "E" => ShipAction::EAST(val),
                "W" => ShipAction::WEST(val),
                "L" => ShipAction::LEFT(val),
                "R" => ShipAction::RIGHT(val),
                "F" => ShipAction::FORWARD(val),
                _ => panic!("invalid action"),
            };
            actions.push(action);
        }
        let waypoint = Waypoint { x: 10, y: 1 };
        Ship {
            x: 0,
            y: 0,
            direction: 90,
            actions,
            waypoint,
        }
    }

    fn move_ship(&mut self) {
        for action in self.actions.iter_mut() {
            match action {
                ShipAction::NORTH(val) => { self.y += *val; },
                ShipAction::SOUTH(val) => { self.y -= *val; },
                ShipAction::EAST(val) => { self.x += *val; },
                ShipAction::WEST(val) => { self.x -= *val; },
                ShipAction::LEFT(val) => {
                    self.direction += 360 - *val;
                    self.direction %= 360;
                },
                ShipAction::RIGHT(val) => {
                    self.direction += *val;
                    self.direction %= 360;
                },
                ShipAction::FORWARD(val) => {
                    match self.direction {
                        0 => { self.y += *val; },
                        90 => { self.x += *val; },
                        180 => { self.y -= *val; },
                        270 => { self.x -= *val; },
                        _ => panic!("invalid direction"),
                    }
                },
            }
        }
    }

    fn move_ship_by_waypoint(&mut self) {
        for action in self.actions.iter_mut() {
            match action {
                ShipAction::NORTH(val) => { self.waypoint.y += *val; },
                ShipAction::SOUTH(val) => { self.waypoint.y -= *val; },
                ShipAction::EAST(val) => { self.waypoint.x += *val; },
                ShipAction::WEST(val) => { self.waypoint.x -= *val; },
                ShipAction::LEFT(val) => {
                    self.waypoint = match *val {
                        90 => Waypoint {
                                x: -self.waypoint.y,
                                y: self.waypoint.x,
                              },
                        180 => Waypoint {
                                x: -self.waypoint.x,
                                y: -self.waypoint.y,
                              },
                        270 => Waypoint {
                                x: self.waypoint.y,
                                y: -self.waypoint.x,
                              },
                        _ => panic!("invalid value"),
                    }
                },
                ShipAction::RIGHT(val) => {
                    self.waypoint = match *val {
                        90 => Waypoint {
                                x: self.waypoint.y,
                                y: -self.waypoint.x,
                              },
                        180 => Waypoint {
                                x: -self.waypoint.x,
                                y: -self.waypoint.y,
                              },
                        270 => Waypoint {
                                x: -self.waypoint.y,
                                y: self.waypoint.x,
                              },
                        _ => panic!("invalid value"),
                    }
                },
                ShipAction::FORWARD(val) => {
                    self.x += *val * self.waypoint.x;
                    self.y += *val * self.waypoint.y;
                },
            }
        }
    }

    fn distance(&self) -> i32 {
        self.x.abs() + self.y.abs()
    }
}

fn day12() {
    let input = read_lines("input12");

    let mut ship = Ship::new(&input);
    ship.move_ship();
    println!("12a: {}", ship.distance());

    let mut ship = Ship::new(&input);
    ship.move_ship_by_waypoint();
    println!("12b: {}", ship.distance());
}

fn find_next_bus(time: i64, busses: &[i64]) -> i64 {
    let mut min_time = i64::MAX;
    let mut min_bus = 0;
    for bus in busses {
        let next = (time + bus) - (time % bus);
        if next < min_time {
            min_time = next;
            min_bus = *bus;
        }
    }
    min_bus * (min_time - time)
}

/* taken from https://rosettacode.org/wiki/Chinese_remainder_theorem#Rust */
#[allow(clippy::many_single_char_names)]
fn chinese_remainder(residues: &[i64], modulii: &[i64]) -> Option<i64> {
    fn egcd(a: i64, b: i64) -> (i64, i64, i64) {
        if a == 0 {
            (b, 0, 1)
        } else {
            let (g, x, y) = egcd(b % a, a);
            (g, y - (b / a) * x, x)
        }
    }

    fn mod_inv(x: i64, n: i64) -> Option<i64> {
        let (g, x, _) = egcd(x, n);
        if g == 1 {
            Some((x % n + n) % n)
        } else {
            None
        }
    }

    let prod = modulii.iter().product::<i64>();

    let mut sum = 0;

    for (&residue, &modulus) in residues.iter().zip(modulii) {
        let p = prod / modulus;
        sum += residue * mod_inv(p, modulus)? * p
    }

    Some(sum % prod)
}

fn find_earliest_time(input: &str) -> i64 {
    let mut residues = Vec::new();
    let mut modulii = Vec::new();
    for (residue, modulus) in input.split(',').enumerate() {
        if modulus == "x" {
            continue;
        }
        let modulus = modulus.parse::<i64>().unwrap();
        let residue = -(residue as i64); /* time needs to be moved back */
        let residue = (residue + modulus) % modulus; /* make sure it's positive */
        modulii.push(modulus);
        residues.push(residue);
    }
    chinese_remainder(&residues, &modulii).unwrap()
}

fn day13() {
    let input = read_lines("input13");
    let time = input[0].parse::<i64>().unwrap();
    let busses : Vec<i64> = input[1].split(',')
                                    .filter(|b| *b != "x")
                                    .map(|b| b.parse::<i64>().unwrap())
                                    .collect();
    let next = find_next_bus(time, &busses);
    println!("13a: {}", next);

    println!("13b: {}", find_earliest_time(&input[1]));
}

enum InitializationInstruction {
    MEM(u64, u64),
    MASK(String),
}

struct DockingProgram {
    instructions: Vec<InitializationInstruction>,
}

impl DockingProgram {
    fn new(input: &[String]) -> DockingProgram {
        let mut instructions = Vec::new();

        for line in input {
            let instruction : Vec<&str> = line.split(" = ").collect();
            let cmd = instruction[0];
            let value = instruction[1];

            let init_instr = if cmd == "mask" {
                InitializationInstruction::MASK(String::from(value))
            } else {
                let end = cmd.find(']').unwrap();
                let addr = &cmd[4..end].parse::<u64>().unwrap();
                InitializationInstruction::MEM(*addr, value.parse::<u64>().unwrap())
            };
            instructions.push(init_instr);
        }

        DockingProgram { instructions }
    }

    fn mask_value(&self, mask: &str, value: u64) -> u64 {
        let mask0 = u64::from_str_radix(&mask.replace("X", "1"), 2).unwrap();
        let mask1 = u64::from_str_radix(&mask.replace("X", "0"), 2).unwrap();

        (value & mask0) | mask1
    }

    fn run(&self) -> u64 {
        let mut mask = String::new();
        let mut memory = HashMap::new();

        for instruction in &self.instructions {
            match instruction {
                InitializationInstruction::MASK(new_mask) => {
                    mask = new_mask.clone();
                },
                InitializationInstruction::MEM(addr, val) => {
                    memory.insert(addr, self.mask_value(&mask, *val));
                },
            }
        }
        memory.values().sum()
    }

    fn collect_addresses(&self, mask: &str, addr: u64) -> Vec<u64> {
        let mut addresses = Vec::new();
        addresses.push(addr);

        let positions : Vec<usize> = mask.chars()
                                         .enumerate()
                                         .filter(|(_,x)| *x == 'X')
                                         .map(|(idx,_)| mask.len() - 1 - idx)
                                         .collect();
        for pos in positions {
            let mut new_addresses = Vec::new();
            for addr in addresses {
                let set_bit1 = 1 << pos;
                new_addresses.push(addr | set_bit1);

                let set_bit0 = !(1 << pos);
                new_addresses.push(addr & set_bit0);
            }
            addresses = new_addresses;
        }

        addresses
    }

    fn run_v2(&self) -> u64 {
        let mut mask = String::new();
        let mut memory = HashMap::new();

        for instruction in &self.instructions {
            match instruction {
                InitializationInstruction::MASK(new_mask) => {
                    mask = new_mask.clone();
                },
                InitializationInstruction::MEM(addr, val) => {
                    /* overwrite addr with 1s from mask */
                    let mask_step1 = u64::from_str_radix(&mask.replace("X", "0"), 2).unwrap();
                    let addr = addr | mask_step1;

                    for new_addr in self.collect_addresses(&mask, addr) {
                        memory.insert(new_addr, *val);
                    }
                },
            }
        }
        memory.values().sum()
    }
}

fn day14() {
    let input = read_lines("input14");
    let program = DockingProgram::new(&input);

    println!("14a: {}", program.run());
    println!("14b: {}", program.run_v2());
}

fn memory_number(numbers: &[usize], limit: usize) -> usize {
    let mut memory = HashMap::new();
    let mut last_number = 0;

    for (i, number) in numbers.iter().enumerate() {
        memory.insert(*number, (i, i, 0));
        last_number = *number;
    }

    for i in memory.len()..limit {
        let (pos0, pos1, count) = memory[&last_number];
        last_number = match count {
            0 => 0,
            _ => pos1 - pos0,
        };
        let update = match memory.get(&last_number) {
            None => (i, i, 0),
            Some((_, p1, c)) => (*p1, i, c + 1),
        };
        memory.insert(last_number, update);
    }
    last_number
}

fn day15() {
    let input = [1, 2, 16, 19, 18, 0];

    println!("15a: {}", memory_number(&input, 2020));
    println!("15b: {}", memory_number(&input, 30000000));
}

#[derive(PartialEq,Eq,Hash)]
struct TicketRule {
    name: String,
    ranges: Vec<(u32, u32)>,
}

impl TicketRule {
    fn new(input: &str) -> TicketRule {
        let tmp : Vec<&str> = input.split(": ").collect();
        let name = String::from(tmp[0]);

        let mut ranges = Vec::new();
        for range in tmp[1].split(" or ") {
            let range : Vec<&str> = range.split('-').collect();
            let (low, high) = (range[0], range[1]);
            ranges.push((low.parse::<u32>().unwrap(), high.parse::<u32>().unwrap()));
        }

        TicketRule {
            name,
            ranges,
        }
    }

    fn ticket_value_in_range(&self, value: u32) -> bool {
        for range in &self.ranges {
            if value >= range.0 && value <= range.1 {
                return true;
            }
        }
        false
    }
}

#[derive(Clone)]
struct Ticket {
    values: Vec<u32>,
}

impl Ticket {
    fn new(input: &str) -> Ticket {
        let values = input.split(',')
                          .map(|x| x.parse::<u32>().unwrap())
                          .collect();
        Ticket { values }
    }

    fn invalid_values(&self, rules: &[TicketRule]) -> Vec<u32> {
        let mut invalids = Vec::new();
        for val in &self.values {
            let mut valid = false;
            for rule in rules {
                valid = rule.ticket_value_in_range(*val);
                if valid { break; }
            }
            if !valid {
                invalids.push(*val);
            }
        }
        invalids
    }
}

fn possible_rule_positions(rule: &TicketRule, tickets: &[Ticket]) -> Vec<usize> {
    let num_rules = tickets[0].values.len();

    let mut rule_counts = vec![0; num_rules];
    for ticket in tickets {
        for (idx, val) in ticket.values.iter().enumerate() {
            if rule.ticket_value_in_range(*val) {
                rule_counts[idx] += 1;
            }
        }
    }

    /* return positions where the rule was counted for every ticket */
    rule_counts.iter()
               .enumerate()
               .filter(|&(_,count)| *count == tickets.len())
               .map(|(idx,_)| idx)
               .collect()
}

fn find_unique_position(possibilities: &HashMap<&TicketRule, Vec<usize>>) -> Option<usize> {
    for positions in possibilities.values() {
        if positions.len() == 1 {
            return Some(positions[0])
        }
    }
    None
}

fn find_rules_order(rules: &[TicketRule], tickets: &[Ticket]) -> HashMap<String,usize> {
    let mut ordered_rules : HashMap<String,usize> = HashMap::new();

    let mut rule_possibilities = HashMap::new();
    for rule in rules {
        rule_possibilities.insert(rule, possible_rule_positions(rule, tickets));
    }

    while let Some(unique_pos) = find_unique_position(&rule_possibilities) {
        for (rule, positions) in &mut rule_possibilities {
            let tmp = positions.iter().enumerate().find(|&(_,x)| *x == unique_pos);
            if let Some((idx,_)) = tmp {
                if positions.len() == 1 {
                    ordered_rules.insert(rule.name.clone(), unique_pos);
                }
                positions.remove(idx);
            }
        }
    }

    ordered_rules
}

fn day16() {
    let input = read_file("input16");
    let input : Vec<&str> = input.split("\n\n").collect();

    let rules : Vec<TicketRule> = input[0].split('\n')
                                          .filter(|x| !x.is_empty())
                                          .map(TicketRule::new)
                                          .collect();
    let your_ticket = Ticket::new(input[1].split('\n').nth(1).unwrap());

    let nearby_tickets : Vec<Ticket> = input[2].split('\n')
                                               .skip(1)
                                               .filter(|x| !x.is_empty())
                                               .map(Ticket::new)
                                               .collect();

    let mut error_rate = 0;
    for ticket in &nearby_tickets {
        error_rate += ticket.invalid_values(&rules).iter().sum::<u32>();
    }
    println!("16a: {}", error_rate);

    let nearby_tickets : Vec<Ticket> = nearby_tickets.iter()
                                                     .filter(|x| x.invalid_values(&rules).is_empty())
                                                     .cloned()
                                                     .collect();
    let ordered_rules = find_rules_order(&rules, &nearby_tickets);
    let result : usize = ordered_rules.iter()
                                      .filter(|(name,_)| name.starts_with("departure"))
                                      .map(|(_,pos)| your_ticket.values[*pos] as usize)
                                      .product();
    println!("16b: {}", result);
}

#[derive(Eq, PartialEq, Hash, Clone, Copy, Debug)]
struct Point4D {
    x: isize,
    y: isize,
    z: isize,
    w: isize,
}

struct ConwayMap {
    map: HashSet<Point4D>,
    dimensions: (Point4D, Point4D),
}

impl ConwayMap {
    fn new(input: &[String]) -> ConwayMap {
        let mut map = HashSet::new();
        for (y, line) in input.iter().enumerate() {
            for (x, c) in line.chars().enumerate() {
                if c == '#' {
                    let point = Point4D {
                        x: x as isize,
                        y: y as isize,
                        z: 0,
                        w: 0,
                    };
                    map.insert(point);
                }
            }
        }
        let dimensions = (Point4D { x:0, y:0, z:0, w:0 }, Point4D { x:0, y:0, z:0, w:0 });
        ConwayMap { map, dimensions }
    }

    fn update_dimensions(&mut self, four_d: bool) {
        let mut lower = Point4D { x:0, y:0, z:0, w:0 };
        let mut upper = lower;

        for pos in &self.map {
            lower.x = cmp::min(lower.x, pos.x);
            lower.y = cmp::min(lower.y, pos.y);
            lower.z = cmp::min(lower.z, pos.z);
            lower.w = cmp::min(lower.w, pos.w);
            upper.x = cmp::max(upper.x, pos.x);
            upper.y = cmp::max(upper.y, pos.y);
            upper.z = cmp::max(upper.z, pos.z);
            upper.w = cmp::max(upper.w, pos.w);
        }
        if !four_d {
            lower.w = 0;
            upper.w = 0;
        }

        self.dimensions = (lower, upper);
    }

    fn count_neighbors(&self, pos: &Point4D, four_d: bool) -> u32 {
        let mut count = 0;
        for dx in -1..=1 {
            for dy in -1..=1 {
                for dz in -1..=1 {
                    for dw in -1..=1 {
                        if !four_d && dw != 0 {
                            continue;
                        }
                        let neighbor = Point4D {
                            x: pos.x + dx,
                            y: pos.y + dy,
                            z: pos.z + dz,
                            w: pos.w + dw,
                        };
                        if neighbor == *pos {
                            continue;
                        }
                        if self.map.contains(&neighbor) {
                            count += 1;
                        }
                    }
                }
            }
        }
        count
    }

    fn is_active(&self, pos: &Point4D) -> bool {
        self.map.contains(&pos)
    }

    fn step(&mut self, four_d: bool) {
        self.update_dimensions(four_d);
        let mut new_map = HashSet::new();

        for x in self.dimensions.0.x - 1 ..= self.dimensions.1.x + 1 {
            for y in self.dimensions.0.y - 1 ..= self.dimensions.1.y + 1 {
                for z in self.dimensions.0.z - 1 ..= self.dimensions.1.z + 1 {
                    for w in self.dimensions.0.w - 1 ..= self.dimensions.1.w + 1 {
                        let point = Point4D { x, y, z, w };
                        let neighbors = self.count_neighbors(&point, four_d);
                        if self.is_active(&point) {
                            if neighbors == 2 || neighbors == 3 {
                                new_map.insert(point);
                            }
                        } else if neighbors == 3 {
                            new_map.insert(point);
                        }
                    }
                }
            }
        }

        self.map = new_map;
    }
}

fn day17() {
    let input = read_lines("input17");

    let mut map = ConwayMap::new(&input);
    for _ in 0..6 { map.step(false); }
    println!("17a: {}", map.map.len());

    let mut map = ConwayMap::new(&input);
    for _ in 0..6 { map.step(true); }
    println!("17b: {}", map.map.len());
}

fn eval_expression_no_brackets(input: &str) -> u64 {
    let tokens : Vec<&str> = input.split(' ').collect();
    let mut value = tokens[0].parse::<u64>().unwrap();

    if tokens.len() == 1 {
        return value;
    }

    for pair in tokens[1..].chunks(2) {
        let operation = pair[0];
        let operand = pair[1].parse::<u64>().unwrap();
        match operation {
            "*" => value *= operand,
            "+" => value += operand,
            _ => panic!("unsupported operation"),
        }
    }
    value
}

fn eval_expression_no_brackets_advanced(input: &str) -> u64 {
    let multiplications : Vec<&str> = input.split(" * ").collect();
    multiplications.iter()
                   .map(|x| eval_expression_no_brackets(x))
                   .product()
}

fn eval_expression(input: &str, advanced: bool) -> u64 {
    let re_bracket = Regex::new(r"\(([0-9 +*]+)\)").unwrap();
    let mut input = String::from(input);

    while let Some(cap) = re_bracket.captures(&input) {
        let evaluated = if advanced {
            eval_expression_no_brackets_advanced(cap.get(1).unwrap().as_str())
        } else {
            eval_expression_no_brackets(cap.get(1).unwrap().as_str())
        };
        input = input.replace(cap.get(0).unwrap().as_str(), &evaluated.to_string());
    }

    if advanced {
        eval_expression_no_brackets_advanced(&input)
    } else {
        eval_expression_no_brackets(&input)
    }
}

fn day18() {
    let input = read_lines("input18");

    let result : u64 = input.iter()
                            .map(|x| eval_expression(x, false))
                            .sum();
    println!("18a: {}", result);

    let result : u64 = input.iter()
                            .map(|x| eval_expression(x, true))
                            .sum();
    println!("18b: {}", result);
}

enum MsgSubRule {
    CHAR(char),
    OPTIONS(Vec<Vec<u32>>),
}

impl MsgSubRule {
    fn new(input: &str) -> MsgSubRule {
        /* check for terminating rule with single character */
        if let Some(pos) = input.find('"') {
            return MsgSubRule::CHAR(input.chars().nth(pos+1).unwrap());
        }

        /* we have a list of subrules */
        let mut options = Vec::new();
        let options_str : Vec<&str> = input.split(" | ").collect();
        for opt in options_str {
            let mut subrules = Vec::new();
            let values : Vec<&str> = opt.split(' ').collect();
            for val in values {
                let val = val.parse::<u32>().unwrap();
                subrules.push(val);
            }
            options.push(subrules);
        }
        MsgSubRule::OPTIONS(options)
    }
}

struct MsgRules {
    rules: HashMap<u32, MsgSubRule>,
}

impl MsgRules {
    fn new(input: &[String]) -> MsgRules {
        let mut rules = HashMap::new();
        for line in input {
            let parts : Vec<&str> = line.split(": ").collect();
            let idx = parts[0].parse::<u32>().unwrap();
            let subrule = MsgSubRule::new(parts[1]);
            rules.insert(idx, subrule);
        }
        MsgRules { rules }
    }

    fn matches(&self, msg: &str, idx: u32, remaining: &[u32]) -> bool {
        let rule = &self.rules[&idx];

        if msg.is_empty() {
            return false;
        }

        match rule {
            MsgSubRule::CHAR(c) => {
                if remaining.is_empty() {
                    return c.to_string() == msg;
                } else {
                    return *c == msg.chars().next().unwrap()
                        && self.matches(&msg[1..], remaining[0], &remaining[1..]);
                }
            },
            MsgSubRule::OPTIONS(options) => {
                for option in options {
                    let next_idx = option[0];
                    let remaining = [&option[1..], remaining].concat();
                    if self.matches(msg, next_idx, &remaining) {
                        return true;
                    }
                }
            },
        }

        false
    }

    fn matches_word(&self, msg: &str) -> bool {
        self.matches(msg, 0, &[])
    }
}

fn day19() {
    let input = read_file("input19");
    let input : Vec<&str> = input.split("\n\n").collect();
    let (rules, messages) = (read_lines_str(input[0]), read_lines_str(input[1]));

    let mut msgrules = MsgRules::new(&rules);
    let match_count = messages.iter()
                              .filter(|msg| msgrules.matches_word(msg))
                              .count();
    println!("19a: {}", match_count);

    msgrules.rules.insert(8,  MsgSubRule::new("42 | 42 8"));
    msgrules.rules.insert(11, MsgSubRule::new("42 31 | 42 11 31"));
    let match_count = messages.iter()
                              .filter(|msg| msgrules.matches_word(msg))
                              .count();
    println!("19b: {}", match_count);
}

#[derive(Clone)]
struct PuzzleTile {
    pixels: Vec<Vec<char>>,
    id: u64,
    borders: Vec<String>,
}

impl PuzzleTile {
    fn new(input: &str) -> PuzzleTile {
        let mut lines = input.split('\n');

        let tileid = lines.next().unwrap()
                          .split(' ')
                          .nth(1).unwrap()
                          .strip_suffix(":").unwrap()
                          .parse::<u64>().unwrap();

        let mut pixels = Vec::new();
        for line in lines {
            let mut linevec = Vec::new();
            for pixel in line.chars() {
                linevec.push(pixel);
            }
            pixels.push(linevec);
        }

        let mut borders = Vec::new();
        let mut border1 = String::new();
        let mut border2 = String::new();
        let mut border3 = String::new();
        let mut border4 = String::new();
        for i in 0..pixels.len() {
            border1.push(pixels[0][i]);
            border2.push(pixels[pixels.len()-1][i]);
            border3.push(pixels[i][0]);
            border4.push(pixels[i][pixels.len()-1]);
        }
        borders.push(border1); /* top */
        borders.push(border2); /* bottom */
        borders.push(border3); /* left */
        borders.push(border4); /* right */

        PuzzleTile {
            pixels,
            id: tileid,
            borders,
        }
    }

    fn has_border(&self, border: &str) -> bool {
        let rev_border : String = border.chars().rev().collect();
        self.borders.contains(&String::from(border)) || self.borders.contains(&rev_border)
    }

    fn has_common_border(&self, tile: &PuzzleTile) -> bool {
        for border in &tile.borders {
            if self.has_border(border) {
                return true;
            }
        }
        false
    }

    fn border_top(&self) -> String {
        self.borders[0].clone()
    }

    fn border_bottom(&self) -> String {
        self.borders[1].clone()
    }

    fn border_left(&self) -> String {
        self.borders[2].clone()
    }

    fn border_right(&self) -> String {
        self.borders[3].clone()
    }

    fn rotate_right(&self) -> PuzzleTile {
        let mut pixels = Vec::new();
        for y in 0..self.pixels.len() {
            let mut linevec = Vec::new();
            for x in 0..self.pixels.len() {
                linevec.push(self.pixels[self.pixels.len() - 1 - x][y]);
            }
            pixels.push(linevec);
        }
        let mut borders = Vec::new();
        let mut border1 = String::new();
        let mut border2 = String::new();
        let mut border3 = String::new();
        let mut border4 = String::new();
        for i in 0..pixels.len() {
            border1.push(pixels[0][i]);
            border2.push(pixels[pixels.len()-1][i]);
            border3.push(pixels[i][0]);
            border4.push(pixels[i][pixels.len()-1]);
        }
        borders.push(border1); /* top */
        borders.push(border2); /* bottom */
        borders.push(border3); /* left */
        borders.push(border4); /* right */

        PuzzleTile {
            pixels,
            id: self.id,
            borders,
        }
    }

    fn flip_horizontally(&self) -> PuzzleTile {
        let mut pixels = Vec::new();
        for line in self.pixels.iter().rev() {
            pixels.push(line.clone());
        }
        let mut borders = self.borders.clone();
        borders[2] = borders[2].chars().rev().collect();
        borders[3] = borders[3].chars().rev().collect();
        PuzzleTile {
            pixels,
            id: self.id,
            borders,
        }
    }

    fn orientate_left_border(&self, border: &str) -> PuzzleTile {
        let mut rotated = self.clone();
        for i in 0..3 {
            if i == 1 {
                rotated = rotated.flip_horizontally();
            } else if i == 2 {
                rotated = rotated.flip_horizontally();
                rotated = rotated.rotate_right();
                rotated = rotated.flip_horizontally();
            }
            for _ in 0..4 {
                if rotated.border_left() == border {
                    return rotated;
                }
                rotated = rotated.rotate_right();
            }
        }
        panic!("border not found");
    }

    fn orientate_top_border(&self, border: &str) -> PuzzleTile {
        let mut rotated = self.clone();
        for i in 0..3 {
            if i == 1 {
                rotated = rotated.flip_horizontally();
            } else if i == 2 {
                rotated = rotated.flip_horizontally();
                rotated = rotated.rotate_right();
                rotated = rotated.flip_horizontally();
            }
            for _ in 0..4 {
                if rotated.border_top() == border {
                    return rotated;
                }
                rotated = rotated.rotate_right();
            }
        }
        panic!("border not found");
    }

    fn remove_borders(&self) -> PuzzleTile {
        let mut pixels = Vec::new();
        for line in self.pixels.iter().skip(1).take(self.pixels.len() - 2) {
            let mut line = line.clone();
            line.pop();
            line.remove(0);
            pixels.push(line);
        }
        PuzzleTile {
            pixels,
            id: self.id,
            borders: Vec::new(),
        }
    }

    fn count_pattern(&self, pattern: &[&str]) -> usize {
        let mut count = 0;
        for y in 0..self.pixels.len() - pattern.len() {
            'next_x: for x in 0..self.pixels[y].len() - pattern[0].len() + 1 {
                for (pattern_y,_) in pattern.iter().enumerate() {
                    for pattern_x in 0..pattern[pattern_y].len() {
                        if let Some('#') = pattern[pattern_y].chars().nth(pattern_x) {
                            if self.pixels[y + pattern_y][x + pattern_x] != '#' {
                                continue 'next_x;
                            }
                        }
                    }
                }
                count += 1;
            }
        }
        count
    }

    fn dump(&self) {
        println!("{}", self.id);
        for y in 0..self.pixels.len() {
            for x in 0..self.pixels.len() {
                print!("{}", self.pixels[y][x]);
            }
            println!();
        }
        println!();
    }
}

fn is_same_border(border1: &str, border2: &str) -> bool{
    let str1 = String::from(border1);
    let str2 = String::from(border2);
    let str2_rev : String = str2.chars().rev().collect();
    str1 == str2 || str1 == str2_rev
}

fn find_corner_tiles(tiles: &[PuzzleTile]) -> Vec<u64> {
    let mut counts = HashMap::new();
    for tile1 in tiles {
        for tile2 in tiles {
            if tile1.id == tile2.id {
                continue;
            }
            if tile1.has_common_border(tile2) {
                let count = counts.entry(tile1.id).or_insert(0);
                *count += 1;
            }
        }
    }
    counts.iter()
          .filter(|&(_,count)| *count == 2)
          .map(|(id,_)| *id)
          .collect()
}

fn find_corner_product(tiles: &[PuzzleTile]) -> u64 {
    find_corner_tiles(tiles).iter().product()
}

fn find_tile_with_border(tiles: &[PuzzleTile], border: &str) -> Option<PuzzleTile> {
    for tile in tiles {
        if tile.borders.contains(&String::from(border)) {
            return Some(tile.clone());
        }
    }
    None
}

fn find_neighbor_tiles(tiles: &[PuzzleTile], search: &PuzzleTile) -> Vec<PuzzleTile> {
    let mut neighbors = Vec::new();

    for tile in tiles {
        if tile.id == search.id {
            continue;
        }
        if search.has_common_border(tile) {
            neighbors.push(tile.clone())
        }
    }

    neighbors
}

fn find_neighbor_border(tiles: &[PuzzleTile], id: u64, border: &str) -> Option<PuzzleTile> {
    for tile in tiles {
        if tile.id == id {
            continue;
        }
        if tile.has_border(border) {
            return Some(tile.clone())
        }
    }
    None
}

fn solve_puzzle(tiles: &[PuzzleTile]) -> usize {
    let mut tile_map = HashMap::new();
    for tile in tiles {
        tile_map.insert(tile.id, tile.clone());
    }
    let corners = find_corner_tiles(tiles);

    /* roate a corner piece so that it fits in left/upper corner */
    let mut tile = tile_map[&corners[0]].clone();
    let neighbors = find_neighbor_tiles(tiles, &tile);
    assert_eq!(neighbors.len(), 2);
    loop {
        /* tile needs to have neighbor at right and at bottom */
        if (neighbors[0].has_border(&tile.border_right()) && neighbors[1].has_border(&tile.border_bottom()))
        || (neighbors[1].has_border(&tile.border_right()) && neighbors[0].has_border(&tile.border_bottom())) {
            break;
        }
        tile = tile.rotate_right();
    }

    /* place new tiles left-to-right, top-to-bottom */
    let mut puzzle_field = Vec::new();
    let mut current = tile;
    loop {
        let mut puzzle_row = Vec::new();
        puzzle_row.push(current.clone());
        while let Some(right) = find_neighbor_border(tiles, current.id, &current.border_right()) {
            let right = right.orientate_left_border(&current.border_right());
            puzzle_row.push(right.clone());
            current = right;
        }
        let left = puzzle_row[0].clone();
        puzzle_field.push(puzzle_row);

        current = match find_neighbor_border(tiles, left.id, &left.border_bottom()) {
            None => break,
            Some(t) => {
                t.orientate_top_border(&left.border_bottom())
            },
        };
    }

    /* all pieces are placed, remove borders */
    let mut borderless_field = Vec::new();
    for line in puzzle_field {
        let mut new_line = Vec::new();
        for tile in line {
            new_line.push(tile.remove_borders());
        }
        borderless_field.push(new_line);
    }

    /* merge all tiles into big field */
    let tile_height = borderless_field[0][0].pixels.len();
    let mut merged_field = Vec::new();
    for _ in 0..(tile_height * borderless_field.len()) {
        let chars : Vec<char> = Vec::new();
        merged_field.push(chars);
    }
    for i in 0..borderless_field.len() {
        let tile_line = &borderless_field[i];
        for tile in tile_line {
            for y in 0..tile.pixels.len() {
                for x in 0..tile.pixels[y].len() {
                    merged_field[i * tile_height + y].push(tile.pixels[y][x]);
                }
            }
        }
    }
    let mut puzzle_field = PuzzleTile {
        pixels: merged_field,
        id: 0,
        borders: Vec::new(),
    };

    /* search for pattern */
    let pattern = [
        "                  # ",
        "#    ##    ##    ###",
        " #  #  #  #  #  #   ",
    ];
    let mut pattern_count = 0;
    'outer: for i in 0..3 {
        if i == 1 {
            puzzle_field = puzzle_field.flip_horizontally();
        } else if i == 2 {
            puzzle_field = puzzle_field.flip_horizontally();
            puzzle_field = puzzle_field.rotate_right();
            puzzle_field = puzzle_field.flip_horizontally();
        }
        for _ in 0..4 {
            let solutions = puzzle_field.count_pattern(&pattern);
            if solutions > 0 {
                pattern_count = solutions;
                break 'outer;
            }
            puzzle_field = puzzle_field.rotate_right();
        }
    }
    let total_places = puzzle_field.pixels.iter().flatten().filter(|&x| *x == '#').count();
    let pattern_places : usize = pattern.iter().map(|x| x.chars().filter(|&x| x == '#').count()).sum();
    total_places - (pattern_count * pattern_places)
}

fn day20() {
    let input = read_file("input20");
    let tiles : Vec<PuzzleTile> = input.split("\n\n")
                                       .filter(|x| !x.is_empty())
                                       .map(PuzzleTile::new)
                                       .collect();

    println!("20a: {}", find_corner_product(&tiles));
    println!("20b: {}", solve_puzzle(&tiles));
}

fn find_allergens(input: &[String]) -> (usize, String) {
    let re = Regex::new(r"^([a-z ]+) \(contains ([a-z ,]+)\)$").unwrap();
    let mut allergen_map = HashMap::new();
    let mut all_ingredients = HashSet::new();
    for line in input {
        let caps = re.captures(line).unwrap();
        let ingredients : HashSet<&str> = caps.get(1).unwrap().as_str().split(' ').collect();
        let allergens : Vec<&str> = caps.get(2).unwrap().as_str().split(", ").collect();
        for allergen in allergens {
            let list = allergen_map.entry(allergen).or_insert(Vec::new());
            list.push(ingredients.clone());
        }
        all_ingredients = all_ingredients.union(&ingredients).cloned().collect();
    }

    let mut solutions = HashMap::new();
    let mut known_ingredients = HashSet::new();
    loop {
        for (allergen, ingredient_sets) in &allergen_map {
            let mut unique_set = all_ingredients.clone();
            for ingr in ingredient_sets {
                unique_set = unique_set.intersection(ingr).cloned().collect();
            }
            unique_set = unique_set.difference(&known_ingredients).cloned().collect();
            if unique_set.len() == 1 {
                let ingr = unique_set.iter().cloned().next().unwrap();
                known_ingredients.insert(ingr);
                solutions.insert(*allergen, ingr);
            }
        }
        if allergen_map.len() == solutions.len() {
            break;
        }
    }

    /* count non-allergens for part 1 */
    let no_allergens : HashSet<&str> = all_ingredients.difference(&known_ingredients).cloned().collect();
    let mut no_allergen_count = 0;
    for line in input {
        let caps = re.captures(line).unwrap();
        no_allergen_count += caps.get(1).unwrap().as_str().split(' ').filter(|x| no_allergens.contains(x)).count();
    }

    /* build solution string for part 2 */
    let mut ingredients = String::new();
    let mut allergens : Vec<&str> = solutions.keys().copied().collect();
    allergens.sort_unstable();
    ingredients += solutions[allergens[0]];
    for allergen in allergens.iter().skip(1) {
        ingredients += ",";
        ingredients += solutions[allergen];
    }

    (no_allergen_count, ingredients)
}

fn day21() {
    let input = read_lines("input21");

    let (no_allergen_count, ingredients) = find_allergens(&input);
    println!("21a: {}", no_allergen_count);
    println!("21b: {}", ingredients);
}

fn calculate_score(deck: &VecDeque<usize>) -> usize {
    let mut score = 0;
    for (i, val) in deck.iter().rev().enumerate() {
        score += val * (1 + i);
    }
    score
}

fn find_space_card_winner(player1: &[usize], player2: &[usize]) -> usize {
    let mut deck1 = VecDeque::from_iter(player1.iter().cloned());
    let mut deck2 = VecDeque::from_iter(player2.iter().cloned());

    while !deck1.is_empty() && !deck2.is_empty() {
        let played = (deck1.pop_front().unwrap(), deck2.pop_front().unwrap());

        if played.0 > played.1 {
            deck1.push_back(played.0);
            deck1.push_back(played.1);
        } else {
            deck2.push_back(played.1);
            deck2.push_back(played.0);
        }
    }

    if deck1.is_empty() {
        calculate_score(&deck2)
    } else {
        calculate_score(&deck1)
    }
}

fn find_space_card_winner_recursive(player1: &[usize], player2: &[usize]) -> (usize, usize) {
    let mut deck1 = VecDeque::from_iter(player1.iter().cloned());
    let mut deck2 = VecDeque::from_iter(player2.iter().cloned());

    let mut played_decks = HashSet::new();

    while !deck1.is_empty() && !deck2.is_empty() {
        if played_decks.contains(&(deck1.clone(), deck2.clone())) {
            return (1, 0);
        }
        played_decks.insert((deck1.clone(), deck2.clone()));
        let played = (deck1.pop_front().unwrap(), deck2.pop_front().unwrap());

        let round_winner;
        if deck1.len() >= played.0 && deck2.len() >= played.1 {
            /* start a sub-game */
            let subdeck1 = Vec::from_iter(deck1.iter().take(played.0).cloned());
            let subdeck2 = Vec::from_iter(deck2.iter().take(played.1).cloned());
            round_winner = find_space_card_winner_recursive(&subdeck1, &subdeck2).0;
        } else {
            round_winner = if played.0 > played.1 { 1 } else { 2 };
        }
        if round_winner == 1 {
            deck1.push_back(played.0);
            deck1.push_back(played.1);
        } else {
            deck2.push_back(played.1);
            deck2.push_back(played.0);
        }
    }

    if deck1.is_empty() {
        (2, calculate_score(&deck2))
    } else {
        (1, calculate_score(&deck1))
    }
}

fn day22() {
    let input = read_file("input22");
    let input : Vec<&str> = input.split("\n\n").collect();
    let mut player1 = Vec::new();
    let mut player2 = Vec::new();
    for card in input[0].split('\n').filter(|x| !x.is_empty()).skip(1) {
        player1.push(card.parse::<usize>().unwrap());
    }
    for card in input[1].split('\n').filter(|x| !x.is_empty()).skip(1) {
        player2.push(card.parse::<usize>().unwrap());
    }

    println!("22a: {}", find_space_card_winner(&player1, &player2));
    println!("22b: {}", find_space_card_winner_recursive(&player1, &player2).1);
}

fn play_cups_round(cups: &[usize]) -> Vec<usize> {
    let current = cups[0];
    let pickup = &cups[1..=3];

    let mut destination = (current + cups.len() - 2) % cups.len() + 1;
    while pickup.contains(&destination) {
        destination = (destination + cups.len() - 2) % cups.len() + 1;
    }
    let dest_pos = cups.iter().position(|x| *x == destination).unwrap();

    let mut new_cups = Vec::new();
    for cup in cups.iter().skip(4).take(dest_pos - 3) {
        new_cups.push(*cup);
    }
    for cup in pickup {
        new_cups.push(*cup);
    }
    for cup in cups.iter().skip(dest_pos + 1) {
        new_cups.push(*cup);
    }
    new_cups.push(current);

    new_cups
}

fn play_cups_game(cups: &[usize], rounds: usize) -> usize {
    let mut cups = Vec::from(cups);
    for _ in 0..rounds {
        cups = play_cups_round(&cups);
    }

    let pos_1 = cups.iter().position(|x| *x == 1).unwrap();
    let mut result = 0;
    for cup in cups.iter().skip(pos_1 + 1) {
        result *= 10;
        result += cup;
    }
    for cup in cups.iter().take(pos_1) {
        result *= 10;
        result += cup;
    }
    result
}

fn play_cups_game2(cups: &[usize], rounds: usize) -> usize {
    let mut circle = HashMap::new();

    for (i, cup) in cups.iter().enumerate().take(cups.len() - 1) {
        circle.insert(*cup, cups[i+1]);
    }
    circle.insert(*cups.last().unwrap(), cups[0]);

    let mut current = cups[0];

    for _ in 0..rounds {
        let pickup1 = *circle.get(&current).unwrap();
        let pickup2 = *circle.get(&pickup1).unwrap();
        let pickup3 = *circle.get(&pickup2).unwrap();
        let after_pickup = *circle.get(&pickup3).unwrap();

        let mut dest = (current + cups.len() - 2) % cups.len() + 1;
        while dest == pickup1 || dest == pickup2 || dest == pickup3 {
            dest = (dest + cups.len() - 2) % cups.len() + 1;
        }

        let after_dest = *circle.get(&dest).unwrap();

        circle.insert(current, after_pickup);
        circle.insert(pickup3, after_dest);
        circle.insert(dest, pickup1);

        current = after_pickup;
    }

    let after_1 = *circle.get(&1).unwrap();
    after_1 * circle.get(&after_1).unwrap()
}

fn day23() {
    let input = [1, 5, 6, 7, 9, 4, 8, 2, 3];

    println!("23a: {}", play_cups_game(&input, 100));

    let mut input = Vec::from(input);
    for i in input.len()+1..=1000000 {
        input.push(i);
    }
    println!("23b: {}", play_cups_game2(&input, 10000000));
}

fn main() {
    day23();
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

    #[test]
    fn test_day9() {
        let input = [35, 20, 15, 25, 47, 40, 62, 55, 65, 95, 102, 117, 150, 182, 127, 219, 299, 277, 309, 576];
        let input = Vec::from_iter(input.iter().copied());

        let number = find_xmas_number(&input, 5);
        assert_eq!(number, 127);

        assert_eq!(find_xmas_weakness(&input, number), 62);
    }

    #[test]
    fn test_day10() {
        let input = [16, 10, 15, 5, 1, 11, 7, 19, 6, 12, 4];
        assert_eq!(count_jolt_differences(&input), (7, 5));

        let max_jolts = *input.iter().max().unwrap();
        let map = build_jolt_map(&input);
        let mut path_cache = HashMap::new();
        assert_eq!(count_jolt_paths(&map, &mut path_cache, 0, max_jolts), 8);

        let input = [28, 33, 18, 42, 31, 14, 46, 20, 48, 47, 24,
                     23, 49, 45, 19, 38, 39, 11, 1, 32, 25, 35,
                     8, 17, 7, 9, 4, 2, 34, 10, 3];
        assert_eq!(count_jolt_differences(&input), (22, 10));

        let max_jolts = *input.iter().max().unwrap();
        let map = build_jolt_map(&input);
        let mut path_cache = HashMap::new();
        assert_eq!(count_jolt_paths(&map, &mut path_cache, 0, max_jolts), 19208);
    }

    #[test]
    fn test_day11() {
        let input = "L.LL.LL.LL\n\
                     LLLLLLL.LL\n\
                     L.L.L..L..\n\
                     LLLL.LL.LL\n\
                     L.LL.LL.LL\n\
                     L.LLLLL.LL\n\
                     ..L.L.....\n\
                     LLLLLLLLLL\n\
                     L.LLLLLL.L\n\
                     L.LLLLL.LL\n";
        let input = read_lines_str(input);

        let mut map = SeatingMap::new(&input);
        map.stabilize(false);
        assert_eq!(map.count_occupied(), 37);

        let mut map = SeatingMap::new(&input);
        map.stabilize(true);
        assert_eq!(map.count_occupied(), 26);
    }

    #[test]
    fn test_day12() {
        let input = "F10\nN3\nF7\nR90\nF11\n";
        let input = read_lines_str(input);

        let mut ship = Ship::new(&input);
        ship.move_ship();
        assert_eq!(ship.distance(), 25);

        let mut ship = Ship::new(&input);
        ship.move_ship_by_waypoint();
        assert_eq!(ship.distance(), 286);
    }

    #[test]
    fn test_day13() {
        assert_eq!(find_next_bus(939, &[7, 13, 59, 31, 19]), 295);

        assert_eq!(find_earliest_time("7,13,x,x,59,x,31,19"), 1068781);
        assert_eq!(find_earliest_time("17,x,13,19"), 3417);
        assert_eq!(find_earliest_time("67,7,59,61"), 754018);
        assert_eq!(find_earliest_time("67,x,7,59,61"), 779210);
        assert_eq!(find_earliest_time("67,7,x,59,61"), 1261476);
        assert_eq!(find_earliest_time("1789,37,47,1889"), 1202161486);
    }

    #[test]
    fn test_day14() {
        let input = "mask = XXXXXXXXXXXXXXXXXXXXXXXXXXXXX1XXXX0X\n\
                     mem[8] = 11\n\
                     mem[7] = 101\n\
                     mem[8] = 0\n";
        let input = read_lines_str(input);
        let program = DockingProgram::new(&input);
        assert_eq!(program.run(), 165);

        let input = "mask = 000000000000000000000000000000X1001X\n\
                     mem[42] = 100\n\
                     mask = 00000000000000000000000000000000X0XX\n\
                     mem[26] = 1\n";
        let input = read_lines_str(input);
        let program = DockingProgram::new(&input);
        assert_eq!(program.run_v2(), 208);
    }

    #[test]
    fn test_day15() {
        assert_eq!(memory_number(&[1, 3, 2], 2020), 1);
        assert_eq!(memory_number(&[2, 1, 3], 2020), 10);
        assert_eq!(memory_number(&[1, 2, 3], 2020), 27);
        assert_eq!(memory_number(&[2, 3, 1], 2020), 78);
        assert_eq!(memory_number(&[3, 2, 1], 2020), 438);
        assert_eq!(memory_number(&[3, 1, 2], 2020), 1836);

        // disabled due to long runtime
        //assert_eq!(memory_number(&[0, 3, 6], 30000000), 175594);
        //assert_eq!(memory_number(&[1, 3, 2], 30000000), 2578);
        //assert_eq!(memory_number(&[2, 1, 3], 30000000), 3544142);
        //assert_eq!(memory_number(&[1, 2, 3], 30000000), 261214);
        //assert_eq!(memory_number(&[2, 3, 1], 30000000), 6895259);
        //assert_eq!(memory_number(&[3, 2, 1], 30000000), 18);
        //assert_eq!(memory_number(&[3, 1, 2], 30000000), 362);
    }

    #[test]
    fn test_day16() {
        let input = "class: 1-3 or 5-7\n\
                     row: 6-11 or 33-44\n\
                     seat: 13-40 or 45-50\n\
                     \n\
                     your ticket:\n\
                     7,1,14\n\
                     \n\
                     nearby tickets:\n\
                     7,3,47\n\
                     40,4,50\n\
                     55,2,20\n\
                     38,6,12\n";
        let input : Vec<&str> = input.split("\n\n").collect();

        let rules : Vec<TicketRule> = input[0].split('\n')
                                              .map(TicketRule::new)
                                              .collect();

        let nearby_tickets : Vec<Ticket> = input[2].split('\n')
                                                   .skip(1)
                                                   .filter(|x| !x.is_empty())
                                                   .map(Ticket::new)
                                                   .collect();

        let mut error_rate = 0;
        for ticket in &nearby_tickets {
            error_rate += ticket.invalid_values(&rules).iter().sum::<u32>();
        }
        assert_eq!(error_rate, 71);

        let input = "class: 0-1 or 4-19\n\
                     row: 0-5 or 8-19\n\
                     seat: 0-13 or 16-19\n\
                     \n\
                     your ticket:\n\
                     11,12,13\n\
                     \n\
                     nearby tickets:\n\
                     3,9,18\n\
                     15,1,5\n\
                     5,14,9\n";
        let input : Vec<&str> = input.split("\n\n").collect();

        let rules : Vec<TicketRule> = input[0].split('\n')
                                              .map(TicketRule::new)
                                              .collect();

        let nearby_tickets : Vec<Ticket> = input[2].split('\n')
                                                   .skip(1)
                                                   .filter(|x| !x.is_empty())
                                                   .map(Ticket::new)
                                                   .filter(|x| x.invalid_values(&rules).len() == 0)
                                                   .collect();

        let ordered_rules = find_rules_order(&rules, &nearby_tickets);
        assert_eq!(ordered_rules["row"], 0);
        assert_eq!(ordered_rules["class"], 1);
        assert_eq!(ordered_rules["seat"], 2);
    }

    #[test]
    fn test_day17() {
        let input = ".#.\n\
                     ..#\n\
                     ###\n";
        let input = read_lines_str(&input);

        let mut map = ConwayMap::new(&input);
        for _ in 0..6 { map.step(false); }
        assert_eq!(map.map.len(), 112);

        let mut map = ConwayMap::new(&input);
        for _ in 0..6 { map.step(true); }
        assert_eq!(map.map.len(), 848);
    }

    #[test]
    fn test_day18() {
        assert_eq!(eval_expression("1 + 2 * 3 + 4 * 5 + 6", false), 71);
        assert_eq!(eval_expression("1 + (2 * 3) + (4 * (5 + 6))", false), 51);
        assert_eq!(eval_expression("2 * 3 + (4 * 5)", false), 26);
        assert_eq!(eval_expression("5 + (8 * 3 + 9 + 3 * 4 * 3)", false), 437);
        assert_eq!(eval_expression("5 * 9 * (7 * 3 * 3 + 9 * 3 + (8 + 6 * 4))", false), 12240);
        assert_eq!(eval_expression("((2 + 4 * 9) * (6 + 9 * 8 + 6) + 6) + 2 + 4 * 2", false), 13632);

        assert_eq!(eval_expression("1 + 2 * 3 + 4 * 5 + 6", true), 231);
        assert_eq!(eval_expression("1 + (2 * 3) + (4 * (5 + 6))", true), 51);
        assert_eq!(eval_expression("2 * 3 + (4 * 5)", true), 46);
        assert_eq!(eval_expression("5 + (8 * 3 + 9 + 3 * 4 * 3)", true), 1445);
        assert_eq!(eval_expression("5 * 9 * (7 * 3 * 3 + 9 * 3 + (8 + 6 * 4))", true), 669060);
        assert_eq!(eval_expression("((2 + 4 * 9) * (6 + 9 * 8 + 6) + 6) + 2 + 4 * 2", true), 23340);
    }

    #[test]
    fn test_day19() {
        let input = "0: 4 1 5\n\
                     1: 2 3 | 3 2\n\
                     2: 4 4 | 5 5\n\
                     3: 4 5 | 5 4\n\
                     4: \"a\"\n\
                     5: \"b\"\n";
        let rules = read_lines_str(input);

        let msgrules = MsgRules::new(&rules);

        assert_eq!(msgrules.matches_word("ababbb"), true);
        assert_eq!(msgrules.matches_word("abbbab"), true);
        assert_eq!(msgrules.matches_word("bababa"), false);
        assert_eq!(msgrules.matches_word("aaabbb"), false);
        assert_eq!(msgrules.matches_word("aaaabbb"), false);

        let input = "42: 9 14 | 10 1\n\
                     9: 14 27 | 1 26\n\
                     10: 23 14 | 28 1\n\
                     1: \"a\"\n\
                     11: 42 31\n\
                     5: 1 14 | 15 1\n\
                     19: 14 1 | 14 14\n\
                     12: 24 14 | 19 1\n\
                     16: 15 1 | 14 14\n\
                     31: 14 17 | 1 13\n\
                     6: 14 14 | 1 14\n\
                     2: 1 24 | 14 4\n\
                     0: 8 11\n\
                     13: 14 3 | 1 12\n\
                     15: 1 | 14\n\
                     17: 14 2 | 1 7\n\
                     23: 25 1 | 22 14\n\
                     28: 16 1\n\
                     4: 1 1\n\
                     20: 14 14 | 1 15\n\
                     3: 5 14 | 16 1\n\
                     27: 1 6 | 14 18\n\
                     14: \"b\"\n\
                     21: 14 1 | 1 14\n\
                     25: 1 1 | 1 14\n\
                     22: 14 14\n\
                     8: 42\n\
                     26: 14 22 | 1 20\n\
                     18: 15 15\n\
                     7: 14 5 | 1 21\n\
                     24: 14 1\n";
        let rules = read_lines_str(input);

        let mut msgrules = MsgRules::new(&rules);
        let messages = [
            "abbbbbabbbaaaababbaabbbbabababbbabbbbbbabaaaa",
            "bbabbbbaabaabba",
            "babbbbaabbbbbabbbbbbaabaaabaaa",
            "aaabbbbbbaaaabaababaabababbabaaabbababababaaa",
            "bbbbbbbaaaabbbbaaabbabaaa",
            "bbbababbbbaaaaaaaabbababaaababaabab",
            "ababaaaaaabaaab",
            "ababaaaaabbbaba",
            "baabbaaaabbaaaababbaababb",
            "abbbbabbbbaaaababbbbbbaaaababb",
            "aaaaabbaabaaaaababaa",
            "aaaabbaaaabbaaa",
            "aaaabbaabbaaaaaaabbbabbbaaabbaabaaa",
            "babaaabbbaaabaababbaabababaaab",
            "aabbbbbaabbbaaaaaabbbbbababaaaaabbaaabba",
        ];
        let match_count = messages.iter()
                                  .filter(|msg| msgrules.matches_word(msg))
                                  .count();
        assert_eq!(match_count, 3);

        msgrules.rules.insert(8,  MsgSubRule::new("42 | 42 8"));
        msgrules.rules.insert(11, MsgSubRule::new("42 31 | 42 11 31"));
        let match_count = messages.iter()
                                  .filter(|msg| msgrules.matches_word(msg))
                                  .count();
        assert_eq!(match_count, 12);
    }

    #[test]
    fn test_day20() {
        let input = "Tile 2311:\n\
                    ..##.#..#.\n\
                    ##..#.....\n\
                    #...##..#.\n\
                    ####.#...#\n\
                    ##.##.###.\n\
                    ##...#.###\n\
                    .#.#.#..##\n\
                    ..#....#..\n\
                    ###...#.#.\n\
                    ..###..###\n\
                    \n\
                    Tile 1951:\n\
                    #.##...##.\n\
                    #.####...#\n\
                    .....#..##\n\
                    #...######\n\
                    .##.#....#\n\
                    .###.#####\n\
                    ###.##.##.\n\
                    .###....#.\n\
                    ..#.#..#.#\n\
                    #...##.#..\n\
                    \n\
                    Tile 1171:\n\
                    ####...##.\n\
                    #..##.#..#\n\
                    ##.#..#.#.\n\
                    .###.####.\n\
                    ..###.####\n\
                    .##....##.\n\
                    .#...####.\n\
                    #.##.####.\n\
                    ####..#...\n\
                    .....##...\n\
                    \n\
                    Tile 1427:\n\
                    ###.##.#..\n\
                    .#..#.##..\n\
                    .#.##.#..#\n\
                    #.#.#.##.#\n\
                    ....#...##\n\
                    ...##..##.\n\
                    ...#.#####\n\
                    .#.####.#.\n\
                    ..#..###.#\n\
                    ..##.#..#.\n\
                    \n\
                    Tile 1489:\n\
                    ##.#.#....\n\
                    ..##...#..\n\
                    .##..##...\n\
                    ..#...#...\n\
                    #####...#.\n\
                    #..#.#.#.#\n\
                    ...#.#.#..\n\
                    ##.#...##.\n\
                    ..##.##.##\n\
                    ###.##.#..\n\
                    \n\
                    Tile 2473:\n\
                    #....####.\n\
                    #..#.##...\n\
                    #.##..#...\n\
                    ######.#.#\n\
                    .#...#.#.#\n\
                    .#########\n\
                    .###.#..#.\n\
                    ########.#\n\
                    ##...##.#.\n\
                    ..###.#.#.\n\
                    \n\
                    Tile 2971:\n\
                    ..#.#....#\n\
                    #...###...\n\
                    #.#.###...\n\
                    ##.##..#..\n\
                    .#####..##\n\
                    .#..####.#\n\
                    #..#.#..#.\n\
                    ..####.###\n\
                    ..#.#.###.\n\
                    ...#.#.#.#\n\
                    \n\
                    Tile 2729:\n\
                    ...#.#.#.#\n\
                    ####.#....\n\
                    ..#.#.....\n\
                    ....#..#.#\n\
                    .##..##.#.\n\
                    .#.####...\n\
                    ####.#.#..\n\
                    ##.####...\n\
                    ##..#.##..\n\
                    #.##...##.\n\
                    \n\
                    Tile 3079:\n\
                    #.#.#####.\n\
                    .#..######\n\
                    ..#.......\n\
                    ######....\n\
                    ####.#..#.\n\
                    .#...#.##.\n\
                    #.#####.##\n\
                    ..#.###...\n\
                    ..#.......\n\
                    ..#.###...\n\n";
        let tiles : Vec<PuzzleTile> = input.split("\n\n")
                                           .filter(|x| !x.is_empty())
                                           .map(PuzzleTile::new)
                                           .collect();

        assert_eq!(find_corner_product(&tiles), 20899048083289);
        assert_eq!(solve_puzzle(&tiles), 273);
    }

    #[test]
    fn test_day21() {
        let input = "mxmxvkd kfcds sqjhc nhms (contains dairy, fish)\n\
                     trh fvjkl sbzzf mxmxvkd (contains dairy)\n\
                     sqjhc fvjkl (contains soy)\n\
                     sqjhc mxmxvkd sbzzf (contains fish)\n";
        let input = read_lines_str(&input);

        let (no_allergen_count, ingredients) = find_allergens(&input);
        assert_eq!(no_allergen_count, 5);
        assert_eq!(ingredients, "mxmxvkd,sqjhc,fvjkl");
    }

    #[test]
    fn test_day22() {
        let player1 = [9, 2, 6, 3, 1];
        let player2 = [5, 8, 4, 7, 10];

        assert_eq!(find_space_card_winner(&player1, &player2), 306);
        assert_eq!(find_space_card_winner_recursive(&player1, &player2).1, 291);
    }

    #[test]
    fn test_day23() {
        let input = [3, 8, 9, 1, 2, 5, 4, 6, 7];

        assert_eq!(play_cups_game(&input, 10), 92658374);
        assert_eq!(play_cups_game(&input, 100), 67384529);

        let mut input = Vec::from(input);
        for i in input.len()+1..=1000000 {
            input.push(i);
        }
        assert_eq!(play_cups_game2(&input, 10000000), 149245887792);
    }
}
