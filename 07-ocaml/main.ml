(* Определение структуры доски *)
module QueenBoard = struct
  type t = {
    size: int;
    horizontal: bool array;
    vertical: bool array;
    diagonal1: bool array;
    diagonal2: bool array;
  }

  let create n = {
    size = n;
    horizontal = Array.make n false;
    vertical = Array.make n false;
    diagonal1 = Array.make (2 * n - 1) false;
    diagonal2 = Array.make (2 * n - 1) false;
  }

  let is_attack board i j =
    let d1 = i + j in
    let d2 = i - j + board.size - 1 in
    board.horizontal.(i) || board.vertical.(j) || board.diagonal1.(d1) || board.diagonal2.(d2)

  let set_queen board i j =
    let d1 = i + j in
    let d2 = i - j + board.size - 1 in
    board.horizontal.(i) <- true;
    board.vertical.(j) <- true;
    board.diagonal1.(d1) <- true;
    board.diagonal2.(d2) <- true

  let unset_queen board i j =
    let d1 = i + j in
    let d2 = i - j + board.size - 1 in
    board.horizontal.(i) <- false;
    board.vertical.(j) <- false;
    board.diagonal1.(d1) <- false;
    board.diagonal2.(d2) <- false
end

(* Рекурсивная функция для подсчета решений *)
let rec queen_nxn n board j =
  if j = n then 1
  else
    let count = ref 0 in
    for i = 0 to n - 1 do
      if not (QueenBoard.is_attack board i j) then begin
        QueenBoard.set_queen board i j;
        count := !count + queen_nxn n board (j + 1);
        QueenBoard.unset_queen board i j
      end
    done;
    !count

(* Главная функция *)
let queen_nxn_main n =
  let board = QueenBoard.create n in
  queen_nxn n board 0

(* Точка входа *)
let () =
  let tests = [
    (2, 0,  "Test #1");
    (3, 0,  "Test #2");
    (4, 2,  "Test #3");
    (5, 10, "Test #4");
    (8, 92, "Test #5")
  ] in
  List.iter (fun (n, expected, name) ->
    let result = queen_nxn_main n in
    Printf.printf "%s: %s (Expected: %d, Got: %d)\n"
      name
      (if result = expected then "Success" else "Failed")
      expected
      result
  ) tests