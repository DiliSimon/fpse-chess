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
      | Occupied(King(White,true)) -> "0"
      | Occupied(King(White,false)) -> "1"
      | Occupied(Queen(White)) -> "q"
      | Occupied(Rook(White,true)) -> "2"
      | Occupied(Rook(White,false)) -> "3"
      | Occupied(Bishop(White)) -> "b"
      | Occupied(Knight(White)) -> "n"
      | Occupied(Pawn(White)) -> "p"
      | Occupied(King(Black,true)) -> "4"
      | Occupied(King(Black,false)) -> "5"
      | Occupied(Queen(Black)) -> "Q"
      | Occupied(Rook(Black,true)) -> "6"
      | Occupied(Rook(Black,false)) -> "7"
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
            | '0' -> Occupied(King(White,true))
            | '1' -> Occupied(King(White,false))
            | 'q' -> Occupied(Queen(White))
            | '2' -> Occupied(Rook(White,true))
            | '3' -> Occupied(Rook(White,false))
            | 'b' -> Occupied(Bishop(White))
            | 'n' -> Occupied(Knight(White))
            | 'p' -> Occupied(Pawn(White))
            | '4' -> Occupied(King(Black,true))
            | '5' -> Occupied(King(Black,false))
            | 'Q' -> Occupied(Queen(Black))
            | '6' -> Occupied(Rook(Black,true))
            | '7' -> Occupied(Rook(Black,false))
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
      and castling = flag "--castling" (optional string)
        ~doc:"castling"
      and save = flag "--save" (optional string)
        ~doc:"saved name"
      and load = flag "--load" (optional string)
        ~doc:"game to load"
      in
      fun () ->
        match (start, instruction, castling, save, load) with
        | (true, _, _, _, _) -> 
          save_game (init_board ()) "cur_game";
          print_board (init_board ()) (!curr_player)
        | (_, Some instruction, None, None, None) -> 
          let orig_1 = parse_move instruction 0 0 in
          let orig_2 = parse_move instruction 0 1 in
          let targ_1 = parse_move instruction 1 0 in
          let targ_2 = parse_move instruction 1 1 in
          let cur_b = read_game "cur_game" in 
          let (new_b,res) = move cur_b (!curr_player) (orig_1,orig_2) (targ_1,targ_2)
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
        | (_, _, Some side, None, None) ->
          let cur_b = read_game "cur_game" in
          let king_side = (match side with
          | "king" -> true
          | "queen" -> false
          | _ -> failwith "bad instruction")
          in
          let res = Board.castling cur_b (!curr_player) king_side
          in
          (match res with
          | Some b -> 
            (match !curr_player with
            | White -> curr_player:= Black
            | Black -> curr_player:= White);
            save_game b "cur_game";
            print_board b (!curr_player)
          | None -> print_string "invalid move")
        | (_, _, _, Some file, None) ->
          save_game (read_game "cur_game") file;
          print_string @@ "game saved to " ^ file
        | (_, _, _, None, Some file) ->
          save_game (read_game file) "cur_game";
          print_board (read_game file) (!curr_player)
        | (_,_,_,_,_) ->
          print_string "bad instruction"
    )
    

let () = Command.run command


(* dune exec -- ./src/controller.exe --init *)

(* dune exec -- ./src/controller.exe --move "1,0 2,0" *)