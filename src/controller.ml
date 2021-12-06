(* PvP or PvE *)
(* val mode *)


(* main control loop *)


open Board
open Core
open Gui

let curr_player = ref White

let save_game (b:board) (g:string) =
  let d = 
  List.fold b ~init:"" ~f:(
    fun res row ->
      res ^
      List.fold ~init:"" row ~f:(
      fun acc piece ->
      acc ^ (match piece with
      | Empty -> " "
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
    | "b" -> curr_player := Black
    | _ -> print_string "bad save");
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

let parse_move (s: string) (pair:int) (pos:int) =
  String.split_on_chars s ~on:[' ']
  |> Fn.flip List.nth_exn pair
  |> String.split_on_chars ~on:[',']
  |> Fn.flip List.nth_exn pos
  |> int_of_string

let command =
  Command.basic
    ~summary:"chess"
    Command.Let_syntax.(
      let%map_open
      start = flag "--init" no_arg
        ~doc:"initialize game"
      and instruction = flag "--move" (optional string)
        ~doc:"move piece"
      and save = flag "--save" (optional string)
        ~doc:"saved name"
      and load = flag "--load" (optional string)
        ~doc:"game to load"
      in
      fun () ->
        match (start, instruction, save, load) with
        | (true, _, _, _) -> 
          save_game (init_board ()) "cur_game";
          print_board (init_board ()) (!curr_player)
        | (_, Some instruction, None, None) -> 
          let orig_1 = parse_move instruction 0 0 in
          let orig_2 = parse_move instruction 0 1 in
          let targ_1 = parse_move instruction 1 0 in
          let targ_2 = parse_move instruction 1 1 in
          let (new_b,res) = move (read_game "cur_game") (!curr_player) (orig_1,orig_2) (targ_1,targ_2)
          in
          (match res with
          | Normal -> 
            (match !curr_player with
            | White -> curr_player:= Black
            | Black -> curr_player:= White);
            save_game new_b "cur_game";
            print_board new_b (!curr_player)
          | Check ->
            (match !curr_player with
            | White -> curr_player:= Black
            | Black -> curr_player:= White);
            save_game new_b "cur_game";
            print_board new_b (!curr_player);
            print_string "Check!"
          | Checkmate ->
            print_result (!curr_player)
          | Fail(m) -> print_string m)
        | (_, _, Some file, None) ->
          save_game (read_game "cur_game") file;
          print_string @@ "game saved to " ^ file
        | (_, _, None, Some file) ->
          save_game (read_game file) "cur_game";
          print_board (read_game file) (!curr_player)
        | (_,_,_,_) ->
          print_string "bad instruction"
    )
    

let () = Command.run command


(* dune exec -- ./src/controller.exe --init *)

(* dune exec -- ./src/controller.exe --move "1,0 2,0" *)