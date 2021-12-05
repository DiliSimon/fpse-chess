open Board
open Core

let print_board (b:board) (p:player) =
  print_string " 01234567\n";
  List.iteri b ~f:(
    fun i row ->
      print_string @@ Int.to_string i;
      List.iter row ~f:(
      fun piece ->
      (match piece with
      | Empty -> " "
      | Occupied(King(White)) -> "♔"
      | Occupied(Queen(White)) -> "♕"
      | Occupied(Rook(White)) -> "♖"
      | Occupied(Bishop(White)) -> "♗"
      | Occupied(Knight(White)) -> "♘"
      | Occupied(Pawn(White)) -> "♙"
      | Occupied(King(Black)) -> "♚"
      | Occupied(Queen(Black)) -> "♛"
      | Occupied(Rook(Black)) -> "♜"
      | Occupied(Bishop(Black)) -> "♝"
      | Occupied(Knight(Black)) -> "♞"
      | Occupied(Pawn(Black)) -> "♟")
      |> print_string
    );
    print_string "\n"
  );
  print_string @@ 
    (match p with 
    | White -> "White"
    | Black -> "Black") ^ "'s turn\n"

let print_result (p:player) =
  (match p with
  | White -> "white"
  | Black -> "black") ^ " wins"
  |> print_string


(* let b_1 = [[Occupied(King(White));Occupied(Queen(Black))];[Empty;Occupied(Pawn(White))]];; *)