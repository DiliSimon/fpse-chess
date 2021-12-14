open Core

type player = White | Black [@@deriving equal]
type chess = King of (player * bool) | Queen of player | Rook of (player * bool) | Bishop of player | Knight of player | Pawn of player [@@deriving equal]
type pos = Occupied of chess | Empty [@@deriving equal]
type board = pos list list [@@deriving equal]
type condition = Check | Checkmate | Fail of string | Normal [@@deriving equal]

exception PosErr of string

let board_fold (board: board) ~(init: 'b) ~(f: (int * int) -> 'b -> pos -> 'b) =
    List.foldi board ~init:init ~f:(fun ridx accum1 col -> List.foldi col ~init:accum1 ~f:(fun cidx accum2 p -> f (ridx, cidx) accum2 p))

let is_empty_pos (pos: pos) : bool =
    match pos with
    | Occupied(_) -> false
    | Empty -> true

let opponent_of (player: player) : player =
    if equal_player player White then Black else White

let get_player (chess: chess) : player =
    match chess with
    | King((p, _)) -> p
    | Queen(p) -> p
    | Rook((p, _)) -> p
    | Bishop(p) -> p
    | Knight(p) -> p
    | Pawn(p) -> p

let get_player_from_pos (pos: pos) : player =
    match pos with
    | Occupied(c) -> get_player c
    | Empty -> raise (PosErr "tried to get player from empty pos")

let init_pos (row: int) (col: int): pos =
    if row > 1 && row < 6 then Empty
    else
    match row, col with
    | 1, _ -> Occupied(Pawn(White))
    | 6, _ -> Occupied(Pawn(Black))
    | 0, 0 -> Occupied(Rook(White, false))
    | 0, 1 -> Occupied(Knight(White))
    | 0, 2 -> Occupied(Bishop(White))
    | 0, 3 -> Occupied(Queen(White))
    | 0, 4 -> Occupied(King(White, false))
    | 0, 5 -> Occupied(Bishop(White))
    | 0, 6 -> Occupied(Knight(White))
    | 0, 7 -> Occupied(Rook(White, false))
    | 7, 0 -> Occupied(Rook(Black, false))
    | 7, 1 -> Occupied(Knight(Black))
    | 7, 2 -> Occupied(Bishop(Black))
    | 7, 3 -> Occupied(Queen(Black))
    | 7, 4 -> Occupied(King(Black, false))
    | 7, 5 -> Occupied(Bishop(Black))
    | 7, 6 -> Occupied(Knight(Black))
    | 7, 7 -> Occupied(Rook(Black, false))
    | _, _ -> raise (PosErr "invalid pos")

let init_board _ : board =
    List.init 8 ~f:(fun rn -> List.init 8 ~f:(fun cn -> init_pos rn cn))

let get_board_pos_exn (board: 'a list list) (idx: int * int): 'a =
    match idx with
    | (row, col) -> List.nth_exn (List.nth_exn board row) col

let get_board_pos (board: 'a list list) (idx: int * int): 'a option =
    match idx with
    | (row, col) -> (match (List.nth board row) with
                    | Some(rl) -> List.nth rl col
                    | None -> None)

let set_board_pos_exn (board: 'a list list) ~(idx: int * int) ~(pos: 'a) : 'a list list =
    match idx with
    | (row, col) -> if row < 0 || row >= 8 || col < 0 || col >= 8 
                    then raise (PosErr ("invalid pos in is_blocked_helper; " ^ "row: " ^ (string_of_int row) ^ " col: " ^ (string_of_int col)))
                    else
                    List.mapi board ~f:(fun ridx  r -> if ridx = row then
                                                        List.mapi r ~f:(fun cidx c -> if cidx = col then pos else c)
                                                        else r)

let set_board_pos (board: 'a list list) (idx: int * int) (pos: 'a) : 'a list list option =
    match idx with
    | (row, col) -> if row < 0 || row >= 8 || col < 0 || col >= 8 then None
                    else
                    Some(List.mapi board ~f:(fun ridx  r -> if ridx = row then
                                                        List.mapi r ~f:(fun cidx c -> if cidx = col then pos else c)
                                                        else r)
                        )

let rec is_blocked_helper (board: board) (ridx: int) (cidx: int) (next_ridx: int) (next_cidx: int) (rd: int) (cd: int) (curr_player: player): bool =
    let curr_pos = (match get_board_pos board (ridx, cidx) with
                    | Some(p) -> p
                    | None -> raise (PosErr ("invalid pos in is_blocked_helper; " ^ "row: " ^ (string_of_int ridx) ^ " col: " ^ (string_of_int cidx))))
    in
    if (ridx = next_ridx) && (cidx = next_cidx) 
    (* If the piece at target position is of the same color, then the path is blocked *)
    then (if (not (is_empty_pos curr_pos)) then (equal_player (get_player_from_pos curr_pos) curr_player) else false)
    else
    match get_board_pos board (ridx, cidx) with
    (* if there's other chess pieces along the path then it is blocked*)
    | Some(Occupied(_)) -> true
    | Some(Empty) -> is_blocked_helper board (ridx+rd) (cidx+cd) next_ridx next_cidx rd cd curr_player
    | None -> raise (PosErr ("invalid pos in is_blocked_helper; " ^ "row: " ^ (string_of_int ridx) ^ " col: " ^ (string_of_int cidx)))

(* Return true if the line from (ridx, cidx) to (next_ridx, next_cidx) contains other pieces*)
let is_blocked (board: board) (ridx: int) (cidx: int) (next_ridx: int) (next_cidx: int) : bool =
    let rd = if (Int.abs((next_ridx - ridx)) = 0) then 0 else (next_ridx - ridx) / Int.abs((next_ridx - ridx)) in
    let cd = if (Int.abs((next_cidx - cidx)) = 0) then 0 else (next_cidx - cidx) / Int.abs((next_cidx - cidx)) in
    match get_board_pos board (ridx, cidx) with
    | Some(Occupied(c)) ->  is_blocked_helper board (ridx+rd) (cidx+cd) next_ridx next_cidx rd cd (get_player c)
    | Some(Empty) -> raise (PosErr "position doesn't contain piece")
    | None -> raise (PosErr "invalid pos")

let is_valid_pawn_move (board: board) (curr_player: player) (ridx: int) (cidx: int) (next_r: int) (next_c: int) : bool =
    if next_r < 0 || next_r > 7 || next_c < 0 || next_c > 7 then false else
    let next_pos = get_board_pos_exn board (next_r, next_c) in
    if cidx = next_c then 
        is_empty_pos next_pos && (((abs (ridx - next_r)) = 1) || (is_empty_pos (get_board_pos_exn board (next_r - (if next_r > ridx then 1 else (-1)), next_c))))
    else if is_empty_pos next_pos then false
    else not (equal_player (get_player_from_pos next_pos) curr_player)

let is_valid_knight_move (board: board) (curr_player: player) (ridx: int) (cidx: int) (next_r: int) (next_c: int) : bool =
    if next_r < 0 || next_r > 7 || next_c < 0 || next_c > 7 then false else
    if not ((abs(next_r - ridx) = 1 && abs(next_c - cidx) = 2) || (abs(next_r - ridx) = 2 && abs(next_c - cidx) = 1)) then false else
    match get_board_pos_exn board (next_r, next_c) with
    | Occupied(chess) -> chess |> get_player |> equal_player curr_player |> not
    | Empty -> true

let is_valid_king_move (board: board) (curr_player: player) (ridx: int) (cidx: int) (next_r: int) (next_c: int) : bool =
    if next_r < 0 || next_r > 7 || next_c < 0 || next_c > 7 then false else
    if not ((abs(next_r - ridx) = 1 && abs(next_c - cidx) = 0) 
            || (abs(next_r - ridx) = 0 && abs(next_c - cidx) = 1)
            || (abs(next_r - ridx) = 1 && abs(next_c - cidx) = 1)) then false else
    match get_board_pos_exn board (next_r, next_c) with
    | Occupied(chess) -> chess |> get_player |> equal_player curr_player |> not
    | Empty -> true

let is_valid_queen_move (board: board) (ridx: int) (cidx: int) (next_r: int) (next_c: int) : bool =
    if next_r < 0 || next_r > 7 || next_c < 0 || next_c > 7 then false else
    if not ((abs(next_r - ridx) > 0 && abs(next_c - cidx) = 0) 
            || (abs(next_r - ridx) = 0 && abs(next_c - cidx) > 0)
            || (abs(next_r - ridx) > 0 && abs(next_c - cidx) > 0 && abs(next_r - ridx) = abs(next_c - cidx))) then false else
    not (is_blocked board ridx cidx next_r next_c)

let is_valid_rook_move (board: board) (ridx: int) (cidx: int) (next_r: int) (next_c: int) : bool =
    if next_r < 0 || next_r > 7 || next_c < 0 || next_c > 7 then false else
    if not ((abs(next_r - ridx) > 0 && abs(next_c - cidx) = 0) 
            || (abs(next_r - ridx) = 0 && abs(next_c - cidx) > 0)) then false else
    not (is_blocked board ridx cidx next_r next_c)

let is_valid_bishop_move (board: board) (ridx: int) (cidx: int) (next_r: int) (next_c: int) : bool =
    if next_r < 0 || next_r > 7 || next_c < 0 || next_c > 7 then false else
    if not (abs(next_r - ridx) > 0 && abs(next_c - cidx) > 0 && abs(next_r - ridx) = abs(next_c - cidx)) then false else
    not (is_blocked board ridx cidx next_r next_c)

(* return lists of possible moves for the chess at position (ridx, cidx)*)
let get_possible_moves (board: board) (chess: chess) (ridx: int) (cidx: int) : (int * int) list =
    match chess with
    | King((player, _)) ->[(ridx+1, cidx); (ridx-1, cidx); (ridx, cidx+1); (ridx, cidx-1); (ridx+1, cidx+1); (ridx+1, cidx-1); (ridx-1, cidx+1); (ridx-1, cidx-1)]
                    |> List.filter ~f:(fun idx -> match idx with | (next_r, next_c) -> is_valid_king_move board player ridx cidx next_r next_c)

    | Queen(_) -> ((List.init 8 ~f:(fun i -> (ridx+i, cidx))) @ (List.init 8 ~f:(fun i -> (ridx, cidx+i))) 
                    @ (List.init 8 ~f:(fun i -> (ridx-i, cidx))) @ (List.init 8 ~f:(fun i -> (ridx, cidx-i)))
                    @ (List.init 8 ~f:(fun i -> (ridx+i, cidx+i))) @ (List.init 8 ~f:(fun i -> (ridx-i, cidx-i)))
                    @ (List.init 8 ~f:(fun i -> (ridx+i, cidx-i))) @ (List.init 8 ~f:(fun i -> (ridx-i, cidx+i)))
                    )
                    |> List.filter ~f:(fun idx -> match idx with | (next_r, next_c) -> is_valid_queen_move board ridx cidx next_r next_c)

    | Rook(_) -> ((List.init 8 ~f:(fun i -> (ridx+i, cidx))) @ (List.init 8 ~f:(fun i -> (ridx, cidx+i))) 
                @ (List.init 8 ~f:(fun i -> (ridx-i, cidx))) @ (List.init 8 ~f:(fun i -> (ridx, cidx-i))))
                |> List.filter ~f:(fun idx -> match idx with | (next_r, next_c) -> is_valid_rook_move board ridx cidx next_r next_c)

    | Bishop(_) -> ((List.init 8 ~f:(fun i -> (ridx+i, cidx+i))) @ (List.init 8 ~f:(fun i -> (ridx-i, cidx-i)))
                    @ (List.init 8 ~f:(fun i -> (ridx+i, cidx-i))) @ (List.init 8 ~f:(fun i -> (ridx-i, cidx+i)))
                    )
                    |> List.filter ~f:(fun idx -> match idx with | (next_r, next_c) -> is_valid_bishop_move board ridx cidx next_r next_c)
                    
    | Pawn(White) -> ([(ridx+1, cidx); (ridx+1, cidx+1); (ridx+1, cidx-1)]) @ (if ridx = 1 then [(ridx + 2, cidx)] else [])
                    |> List.filter ~f:(fun idx -> match idx with | (next_r, next_c) -> is_valid_pawn_move board White ridx cidx next_r next_c)

    | Pawn(Black) -> ([(ridx-1, cidx); (ridx-1, cidx+1); (ridx-1, cidx-1)]) @ (if ridx = 6 then [(ridx - 2, cidx)] else [])
                    |> List.filter ~f:(fun idx -> match idx with | (next_r, next_c) -> is_valid_pawn_move board Black ridx cidx next_r next_c)

    | Knight(player) -> [(ridx+2, cidx+1); (ridx+2,cidx-1); (ridx-2, cidx+1); (ridx-2,cidx-1);(ridx+1, cidx+2); (ridx+1,cidx-2); (ridx-1, cidx+2); (ridx-1,cidx-2)]
                    |> List.filter ~f:(fun idx -> match idx with | (next_r, next_c) -> is_valid_knight_move board player ridx cidx next_r next_c)

let get_all_possible_moves (board: board) (curr_player: player) : ((int * int) * (int * int)) list =
    board_fold board ~init:([]) ~f:(fun src accum pos -> 
                                    match pos with
                                    | Empty -> accum
                                    | Occupied(piece)-> let (ridx, cidx) = src in
                                                        if equal_player curr_player (get_player piece)
                                                        then (List.map (get_possible_moves board piece ridx cidx) ~f:(fun dst -> (src, dst))) @ accum
                                                        else accum
                                    )

let rec append_possible_moves_to_map (curr_piece: chess) (curr_map: pos list list list) (possible_moves: (int * int) list) : pos list list list =
    match possible_moves with
    | move_pos :: ms -> (let next_map = append_possible_moves_to_map curr_piece curr_map ms in
                        match 
                        (set_board_pos next_map move_pos 
                        (Occupied(curr_piece) :: match get_board_pos next_map move_pos with | Some(t) -> t | None -> raise (PosErr "can't find pos to append to")))
                        with
                        | Some(map) -> map
                        | None -> raise (PosErr "Something wrong..."))
    | [] -> curr_map

let add_next_step (board: board) (pos: pos) (ridx: int) (cidx: int) (curr_player: player) (curr_map: pos list list list) : pos list list list =
    match pos with
    | Empty -> curr_map
    | Occupied(chess) -> if not (equal_player (get_player chess) curr_player) then curr_map
                        else get_possible_moves board chess ridx cidx |> append_possible_moves_to_map chess curr_map


let get_next_step_map (board: board) (curr_player: player) : pos list list list =
    let curr_map = ref (List.init 8 ~f:(fun _ -> List.init 8 ~f:(fun _ -> []))) in
    let (_: board) = List.mapi board ~f:(fun ridx r -> List.mapi r ~f:(fun cidx pos -> curr_map := add_next_step board pos ridx cidx curr_player (!curr_map); pos)) in
    !curr_map

let find_king (board: board) (target_player: player) : (int * int) =
    let king_pos = ref (-1, -1) in
    let (_: board) = List.mapi board ~f:(fun ridx r -> List.mapi r ~f:(fun cidx pos -> match pos with 
                                                                                    | Occupied(King((p, _))) -> if equal_player p target_player then king_pos := (ridx, cidx); pos
                                                                                    | _ -> pos ))
    in
    !king_pos

(* verify whether curr_player have checked his opponent *)
let is_check (board: board) (curr_player: player) : bool =
    let next_step_map = get_next_step_map board curr_player in
    let king_pos = find_king board (opponent_of curr_player) in
    List.length (match get_board_pos next_step_map king_pos with 
                | Some(l) -> l 
                | None -> raise (PosErr "fail to extract from step map")) 
                > 0

(* simply set position t to position f, then set f to empty *)
let move_pure (board: board) (f: int * int) (t: (int * int)) : board option = 
    match set_board_pos board t (get_board_pos_exn board f) with
    | Some(nb) -> 
                (match set_board_pos nb f Empty with
                | Some(nb1) ->  Some(nb1)
                | None -> None)
    | None -> None

(* verify whether curr_player has checkmated his opponent*)
let is_checkmate (board: board) (curr_player: player) : bool =
    let all_moves = get_all_possible_moves board (opponent_of curr_player) in
    let next_boards = List.map all_moves ~f:(fun (f, t) -> move_pure board f t) in
    not(List.exists next_boards 
    ~f:(fun wrap -> match wrap with
                    | Some(next_board) -> not (is_check next_board curr_player)
                    | None -> false
                    )
    )

let get_condition (board: board) (curr_player: player) : condition =
    if is_check board curr_player then
        if is_checkmate board curr_player then Checkmate
        else Check
    else Normal

let validate (board: board) (curr_player: player) (f: int * int) (t: (int * int)) : bool =
    match f, t with
    | (rf, cf), (rt, ct) -> if rf = rt && cf = ct then false
                            else
                            (match (get_board_pos board f), (get_board_pos board t) with
                            | _, None -> false
                            | None, _ -> false
                            | Some(Empty), _ -> false
                            | Some(Occupied(chess)), _ -> if not (equal_player (get_player chess) curr_player) then false else
                                                            (match chess with
                                                            | King((player, _)) -> is_valid_king_move board player rf cf rt ct
                                                            | Queen(_) -> is_valid_queen_move board rf cf rt ct
                                                            | Rook(_) -> is_valid_rook_move board rf cf rt ct
                                                            | Bishop(_) -> is_valid_bishop_move board rf cf rt ct
                                                            | Knight(player) -> is_valid_knight_move board player rf cf rt ct
                                                            | Pawn(player) -> is_valid_pawn_move board player rf cf rt ct
                                                            )
                            )

let move (board: board) (curr_player: player) (f: int * int) (t: (int * int)) : (board * condition) =
    if not (validate board curr_player f t) then (board, Fail("invalid move")) else
    match get_board_pos board f with
    | Some(Occupied(curr_chess)) -> 
                    (
                    let (rt, _) = t in
                    (* Pawn Promotion *)
                    let next_pos = 
                        (if not (((equal_chess curr_chess (Pawn(White))) || (equal_chess curr_chess (Pawn(Black)))) && (rt = 0 || rt = 7)) 
                        then (match curr_chess with | King(p, _) -> Occupied(King(p, true)) | Rook(p, _) -> Occupied(Rook(p, true)) | _ -> Occupied(curr_chess))
                        else Occupied(Queen(get_player curr_chess))) 
                    in                    
                    match set_board_pos board t next_pos with
                    | Some(nb) ->   (match set_board_pos nb f Empty with
                                    | Some(nb1) ->  if (is_check nb1 (opponent_of curr_player)) 
                                                    then (board, Fail("still checked after move")) 
                                                    else if is_checkmate nb1 curr_player then (nb1, Checkmate) else (nb1, Normal)
                                    | None -> (board, Fail("fail to set piece")))
                    | None -> (board, Fail("fail to set piece"))
                    )
    | _ -> (board, Fail("fail to get piece"))

let castling (board: board) (curr_player: player) (is_kingside: bool) : board option =
    match curr_player, is_kingside with
    | Black, false  ->  (match (get_board_pos_exn board (7, 4)), (get_board_pos_exn board (7, 0)) with
                        | Occupied(King(Black, false)), Occupied(Rook(Black, false)) -> (if (is_valid_rook_move board 7 0 7 3) 
                                                                                        then Some(set_board_pos_exn board ~idx:(7, 0) ~pos:Empty 
                                                                                                    |> set_board_pos_exn ~idx:(7, 4) ~pos:Empty
                                                                                                    |> set_board_pos_exn ~idx:(7, 3) ~pos:(Occupied(Rook(Black, true)))
                                                                                                    |> set_board_pos_exn ~idx:(7, 2) ~pos:(Occupied(King(Black, true))))
                                                                                        else None
                                                                                        )
                        | _, _ -> None
                        )
    | White, false  ->  (match (get_board_pos_exn board (0, 4)), (get_board_pos_exn board (0, 0)) with
                        | Occupied(King(White, false)), Occupied(Rook(White, false)) -> (if (is_valid_rook_move board 0 0 0 3) 
                                                                                        then Some(set_board_pos_exn board ~idx:(0, 0) ~pos:Empty 
                                                                                                    |> set_board_pos_exn ~idx:(0, 4) ~pos:Empty
                                                                                                    |> set_board_pos_exn ~idx:(0, 3) ~pos:(Occupied(Rook(White, true)))
                                                                                                    |> set_board_pos_exn ~idx:(0, 2) ~pos:(Occupied(King(White, true))))
                                                                                        else None
                                                                                        )
                        | _, _ -> None
                        )
    | Black, true  ->  (match (get_board_pos_exn board (7, 4)), (get_board_pos_exn board (7, 7)) with
                        | Occupied(King(Black, false)), Occupied(Rook(Black, false)) -> (if (is_valid_rook_move board 7 7 7 5) 
                                                                                        then Some(set_board_pos_exn board ~idx:(7, 7) ~pos:Empty 
                                                                                                    |> set_board_pos_exn ~idx:(7, 4) ~pos:Empty
                                                                                                    |> set_board_pos_exn ~idx:(7, 5) ~pos:(Occupied(Rook(Black, true)))
                                                                                                    |> set_board_pos_exn ~idx:(7, 6) ~pos:(Occupied(King(Black, true))))
                                                                                        else None
                                                                                        )
                        | _, _ -> None
                        )
    | White, true  ->  (match (get_board_pos_exn board (0, 4)), (get_board_pos_exn board (0, 7)) with
                        | Occupied(King(White, false)), Occupied(Rook(White, false)) -> (if (is_valid_rook_move board 0 7 0 5) 
                                                                                        then Some(set_board_pos_exn board ~idx:(0, 7) ~pos:Empty 
                                                                                                    |> set_board_pos_exn ~idx:(0, 4) ~pos:Empty
                                                                                                    |> set_board_pos_exn ~idx:(0, 5) ~pos:(Occupied(Rook(White, true)))
                                                                                                    |> set_board_pos_exn ~idx:(0, 6) ~pos:(Occupied(King(White, true))))
                                                                                        else None
                                                                                        )
                        | _, _ -> None
                        )