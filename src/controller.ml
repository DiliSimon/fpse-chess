(* PvP or PvE *)
val mode

let curr_player = ref White

(* main control loop *)
val main : () -> ()

let save_game (b:board) (g:string) =
  let d = 
  List.fold b ~init:"" ~f:(
    fun res row ->
      res ^
      List.fold ~init:"" row ~f:(
      fun acc piece ->
      acc ^ (match piece with
      | Empty(_) -> " "
      | Occupied(King(White)) -> "k"
      | Occupied(Queen(White)) -> "q"
      | Occupied(Rook(White)) -> "r"
      | Occupied(Bishop(White)) -> "b"
      | Occupied(Knight(White)) -> "n"
      | Occupied(Pawn(White)) -> "p"
      | Occupied(King(Black)) -> "K"
      | Occupied(Queen(Black)) -> "Q"
      | Occupied(Rook(Black)) -> "R"
      | Occupied(Bishop(Black)) -> "B"
      | Occupied(Knight(Black)) -> "N"
      | Occupied(Pawn(Black)) -> "P")
    )
    ^ "\n"
  ) 
  ^ (match !curr_player with 
      | White -> "w"
      | Black -> "b")
  in
  Out_channel.write_all g ~data:d
  

  

let read_game (g:string) : board=
  (let turn = 
    (In_channel.read_all g
    |> String.split_on_chars ~on:['\n']
    |> List.last_exn)
    in match turn with 
    | "w" -> curr_player := White
    | "b" -> curr_player := Black);
  In_channel.read_all g
  |> String.split_on_chars ~on:['\n']
  |> List.drop_last_exn
  |> List.fold ~init:[] ~f:(
    fun b row ->
      b @ 
      [String.fold row ~init:[] ~f:(
        fun acc c ->
          acc @ [
            match c with
            | 'k' -> Occupied(King(White))
            | 'q' -> Occupied(Queen(White))
            | 'r' -> Occupied(Rook(White))
            | 'b' -> Occupied(Bishop(White))
            | 'n' -> Occupied(Knight(White))
            | 'p' -> Occupied(Pawn(White))
            | 'K' -> Occupied(King(Black))
            | 'Q' -> Occupied(Queen(Black))
            | 'R' -> Occupied(Rook(Black))
            | 'B' -> Occupied(Bishop(Black))
            | 'N' -> Occupied(Knight(Black))
            | 'P' -> Occupied(Pawn(Black))
            | _ -> Empty
          ]
      )]
    )
