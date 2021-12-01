let print_board (b:board) =
  print_string " 01234567\n";
  List.iteri b ~f:(
    fun i row ->
      print_string @@ Int.to_string i;
      List.iter row ~f:(
      fun piece ->
      (match piece with
      | Empty(_) -> " "
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
  )

let print_result (p:player) =
  (match p with
  | White -> "white"
  | Black -> "black") ^ " wins"
  |> print_string


(* let b_1 = [[Occupied(King(White));Occupied(Queen(Black))];[Empty;Occupied(Pawn(White))]];; *)