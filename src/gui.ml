open Board
open Core

let print_board (b:board) (p:player) =
  print_string " 0 1 2 3 4 5 6 7\n";
  List.iteri b ~f:(
    fun i row ->
      print_string @@ Int.to_string i;
      List.iter row ~f:(
        fun piece ->
          (match piece with
           | Empty -> "  "
           | Occupied(King(White,_)) -> "♔ "
           | Occupied(Queen(White)) -> "♕ "
           | Occupied(Rook(White,_)) -> "♖ "
           | Occupied(Bishop(White)) -> "♗ "
           | Occupied(Knight(White)) -> "♘ "
           | Occupied(Pawn(White)) -> "♙ "
           | Occupied(King(Black,_)) -> "♚ "
           | Occupied(Queen(Black)) -> "♛ "
           | Occupied(Rook(Black,_)) -> "♜ "
           | Occupied(Bishop(Black)) -> "♝ "
           | Occupied(Knight(Black)) -> "♞ "
           | Occupied(Pawn(Black)) -> "♟ ")
          |> print_string
      );
      print_string "\n"
  );
  print_string @@ 
  (match p with 
   | White -> "White"
   | Black -> "Black") 
  ^ 
  "'s turn\n"

let print_result (p:player) =
  (match p with
   | White -> "white"
   | Black -> "black") 
  ^ 
  " wins"
  |> print_string
