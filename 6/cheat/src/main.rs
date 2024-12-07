use std::collections::HashMap;
use std::collections::HashSet;
use std::fs;
use std::time::SystemTime;
use std::time::UNIX_EPOCH;

#[derive(Debug, Eq, Hash, PartialEq, Copy, Clone)]
enum Facing {
    UP,
    RIGHT,
    DOWN,
    LEFT,
}

type Board = HashMap<(usize, usize), char>;

fn get_board() -> Board {
    let one_line_board = fs::read_to_string("../input.text").unwrap();
    let vec_str: Vec<_> = one_line_board.split("\n").collect();
    let mut board: HashMap<(usize, usize), char> = HashMap::new();
    for (y, row) in vec_str.iter().enumerate() {
        for (x, char_) in row.chars().enumerate() {
            board.insert((x, y), char_);
        }
    }
    return board;
}

fn run_board(mut board: Board) -> i32 {
    let mut visited_cells = HashSet::new();
    let mut res = update_board(&mut board);
    loop {
        let _ = match res {
            Ok(ref starting_pos) => {
                if visited_cells.contains(starting_pos) {
                    return 1;
                }
                visited_cells.insert(starting_pos.clone());
                res = update_board(&mut board);
            }
            Err(str_) => {
                println!("{:?}", str_);
                return 0;
            }
        };
    }
}

fn main() {
    let read_start = SystemTime::now().duration_since(UNIX_EPOCH).unwrap();
    let board = get_board();
    let read_end = SystemTime::now().duration_since(UNIX_EPOCH).unwrap();
    println!("{:?}", read_end - read_start);
    let mut obstructions = 0;
    for ((x, y), char_) in &board {
        if *char_ == '#' || *char_ == '^' {
            continue;
        }
        let mut new_board = board.clone();
        new_board.insert((*x, *y), '#');
        obstructions += run_board(new_board);
        println!("Modified {:?}", (x, y));
    }
    println!("{:?}", obstructions);
}

fn get_guard_pos_and_facing(board: &Board) -> Result<(usize, usize, Facing), String> {
    for ((x, y), char_) in board {
        match *char_ {
            '<' => return Ok((*x, *y, Facing::LEFT)),
            '>' => return Ok((*x, *y, Facing::RIGHT)),
            '^' => return Ok((*x, *y, Facing::UP)),
            'v' => return Ok((*x, *y, Facing::DOWN)),
            _ => (),
        }
    }

    return Err("Guard not found".to_string());
}

fn get_coords_in_facing_dir(
    guard_x: isize,
    guard_y: isize,
    guard_facing: &Facing,
) -> (isize, isize) {
    match guard_facing {
        Facing::LEFT => (guard_x - 1, guard_y),
        Facing::UP => (guard_x, guard_y - 1),
        Facing::RIGHT => (guard_x + 1, guard_y),
        Facing::DOWN => (guard_x, guard_y + 1),
    }
}

fn spot_out_of_bounds(x: isize, y: isize) -> Result<(usize, usize), String> {
    if x < 0 || y < 0 {
        return Err("Guard left board".to_string());
    } else if x > 129 {
        return Err("Guard left board".to_string());
    } else if y > 129 {
        return Err("Guard left board".to_string());
    } else {
        return Ok((x.try_into().unwrap(), y.try_into().unwrap()));
    }
}

fn char_by_facing(facing: &Facing) -> char {
    match facing {
        Facing::LEFT => '<',
        Facing::UP => '^',
        Facing::RIGHT => '>',
        Facing::DOWN => 'v',
    }
}

fn rotation_result(facing: Facing) -> char {
    match facing {
        Facing::LEFT => '^',
        Facing::UP => '>',
        Facing::RIGHT => 'v',
        Facing::DOWN => '<',
    }
}

fn update_board(board: &mut Board) -> Result<(usize, usize, Facing), String> {
    let (guard_x, guard_y, guard_facing) = get_guard_pos_and_facing(&board)?;
    let (infront_x, infront_y) = get_coords_in_facing_dir(
        guard_x.try_into().unwrap(),
        guard_y.try_into().unwrap(),
        &guard_facing,
    );
    let (infront_x, infront_y) = spot_out_of_bounds(infront_x, infront_y)?;
    let infront_char = board[&(infront_x, infront_y)];
    if infront_char == '.' {
        board.insert((guard_x, guard_y), '.');
        board.insert((infront_x, infront_y), char_by_facing(&guard_facing));
    } else if infront_char == '#' {
        board.insert((guard_x, guard_y), rotation_result(guard_facing));
    }
    return Ok((guard_x, guard_y, guard_facing));
}
