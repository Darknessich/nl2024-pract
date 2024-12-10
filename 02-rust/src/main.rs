use std::collections::{HashSet, VecDeque};
pub struct Solution;

impl Solution {
    const ROWS: usize = 2;
    const COLS: usize = 3;
    const SIZE: usize = Self::ROWS * Self::COLS;
    const SHIFT: usize = 3;

    // 0x 0000 ... 00 001 010 011 100 101 000
    //    | not use | |0| |1| |2| |3| |4| |5|
    fn get_starting_board_id() -> usize {
        let mut id = 0;
        for i in 0..Self::ROWS {
            for j in 0..Self::COLS {
                id = (id << Self::SHIFT) | ((i * Self::COLS + j + 1) % Self::SIZE);
            }
        }
        return id;
    }


    fn get_board_id(board: &Vec<Vec<i32>>) -> usize {
        let mut id = 0;
        for i in 0..Self::ROWS {
            for j in 0..Self::COLS {
                id = (id << Self::SHIFT) | board[i][j] as usize;
            }
        }
        return id;
    }

    fn swap_positions_id(id: usize, pos1: usize, pos2: usize) -> usize {
        let shift1 = (Self::SIZE - pos1 - 1) * Self::SHIFT;
        let mask1 = 0x7 << shift1;
        let shift2 = (Self::SIZE - pos2 - 1) * Self::SHIFT;
        let mask2 = 0x7 << shift2;
        let mut new_id = id & !(mask1 | mask2);
        new_id |= ((id & mask1) >> shift1) << shift2;
        new_id |= ((id & mask2) >> shift2) << shift1;
        return new_id;
    }

    pub fn sliding_puzzle(board: Vec<Vec<i32>>) -> i32 {
        let mut states = VecDeque::new();
        let mut passed = HashSet::new();
        let target_id = Self::get_board_id(&board);

        states.push_back((Self::get_starting_board_id(), Self::SIZE - 1));
        for steps in 0.. {
            if states.len() == 0 {
                break;
            }

            for _ in 0..states.len() {
                if let Some((id, zero)) = states.pop_front() {
                    if passed.contains(&id) {
                        continue;
                    }
                    if id == target_id {
                        return steps;
                    }

                    passed.insert(id);
                    let row = zero / Self::COLS;
                    let col = zero % Self::COLS;

                    if row > 0 {
                        let pos = (row - 1) * Self::COLS + col;
                        states.push_back((Self::swap_positions_id(id, zero, pos), pos));
                    }
                    if row < Self::ROWS - 1 {
                        let pos = (row + 1) * Self::COLS + col;
                        states.push_back((Self::swap_positions_id(id, zero, pos), pos));
                    }
                    if col > 0 {
                        let pos = row * Self::COLS + col - 1;
                        states.push_back((Self::swap_positions_id(id, zero, pos), pos));
                    }
                    if col < Self::COLS - 1 {
                        let pos = row * Self::COLS + col + 1;
                        states.push_back((Self::swap_positions_id(id, zero, pos), pos));
                    }
                }
            }
        }

        return -1;
    }
}

fn test(board: Vec<Vec<i32>>, result: i32) -> bool {
    return Solution::sliding_puzzle(board) == result;
}

fn main() {
    println!("Test #1: {}", if test(vec![vec![1, 2, 3], vec![4, 0, 5]], 1) {"Succes"} else {"Failed"});
    println!("Test #2: {}", if test(vec![vec![1, 2, 3], vec![5, 4, 0]], -1) {"Succes"} else {"Failed"});
    println!("Test #3: {}", if test(vec![vec![4, 1, 2], vec![5, 0, 3]], 5) {"Succes"} else {"Failed"});
}
