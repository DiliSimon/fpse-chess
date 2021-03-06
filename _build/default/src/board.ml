open Core

type player = White | Black [@@deriving equal]
type chess = King of player | Queen of player | Rook of player | Bishop of player | Knight of player | Pawn of player [@@deriving equal]
type pos = Occupied of chess | Empty [@@deriving equal]
type board = pos list list [@@deriving equal]
type condition = Check | Checkmate | Fail of string | Normal [@@deriving equal]

exception PosErr of string

let opponent_of (player: player) : player =
    if equal_player player White then Black else White
let get_player (chess: chess) : player =
    match chess with
    | King(p) -> p
    | Queen(p) -> p
    | Rook(p) -> p
    | Bishop(p) -> p
    | Knight(p) -> p
    | Pawn(p) -> p

let init_pos (row: int) (col: int): pos =
    if row > 1 && row < 6 then Empty
    else
    match row, col with
    | 1, _ -> Occupied(Pawn(White))
    | 6, _ -> Occupied(Pawn(Black))
    | 0, 0 -> Occupied(Rook(White))
    | 0, 1 -> Occupied(Knight(White))
    | 0, 2 -> Occupied(Bishop(White))
    | 0, 3 -> Occupied(Queen(White))
    | 0, 4 -> Occupied(King(White))
    | 0, 5 -> Occupied(Bishop(White))
    | 0, 6 -> Occupied(Knight(White))
    | 0, 7 -> Occupied(Rook(White))
    | 7, 0 -> Occupied(Rook(Black))
    | 7, 1 -> Occupied(Knight(Black))
    | 7, 2 -> Occupied(Bishop(Black))
    | 7, 3 -> Occupied(Queen(Black))
    | 7, 4 -> Occupied(King(Black))
    | 7, 5 -> Occupied(Bishop(Black))
    | 7, 6 -> Occupied(Knight(Black))
    | 7, 7 -> Occupied(Rook(Black))
    | _, _ -> raise (PosErr "invalid pos")

let init_board _ : board =
    List.init 8 ~f:(fun rn -> List.init 8 ~f:(fun cn -> init_pos rn cn))

let get_board_pos (board: 'a list list) (idx: int * int): 'a option =
    match idx with
    | (row, col) -> (match (List.nth board row) with
                    | Some(rl) -> List.nth rl col
                    | None -> None)

let set_board_pos (board: 'a list list) (idx: int * int) (pos: 'a) : 'a list list option =
    match idx with
    | (row, col) -> if row < 0 || row >= 8 || col < 0 || col >= 8 then None
                    else
                    Some(List.mapi board ~f:(fun ridx  r -> if ridx = row then
                                                        List.mapi r ~f:(fun cidx c -> if cidx = col then pos else c)
                                                        else r)
                        )

let rec is_blocked_helper (board: board) (ridx: int) (cidx: int) (next_ridx: int) (next_cidx: int) (rd: int) (cd: int) (curr_player: player): bool =
    if (ridx - rd = next_ridx) && (cidx - cd = next_cidx) then false
    else
    match get_board_pos board (ridx, cidx) with
    | Some(Occupied(chess)) -> if not (ridx = next_ridx && cidx = next_cidx) then true else (equal_player (get_player chess) curr_player)
    | Some(Empty) -> is_blocked_helper board (ridx+rd) (cidx+cd) next_ridx next_cidx rd cd curr_player
    | None -> raise (PosErr "invalid pos in is_blocked_helper")


(* Return true if the line from (ridx, cidx) to (next_ridx, next_cidx) contains other pieces*)
let is_blocked (board: board) (ridx: int) (cidx: int) (next_ridx: int) (next_cidx: int) : bool =
    let rd = (ridx - next_ridx) / Int.abs((ridx - next_ridx)) in
    let cd = (cidx - next_cidx) / Int.abs((cidx - next_cidx)) in
    match get_board_pos board (ridx, cidx) with
    | Some(Occupied(c)) ->  is_blocked_helper board ridx cidx next_ridx next_cidx rd cd (get_player c)
    | Some(Empty) -> raise (PosErr "position doesn't contain piece")
    | None -> raise (PosErr "invalid pos")


let get_possible_moves (board: board) (chess: chess) (ridx: int) (cidx: int) : (int * int) list =
    match chess with
    | King(_) -> List.filter ~f:(fun idx -> match idx with | (r, c) -> r >= 0 && r < 8 && c >= 0 && c < 8) 
                    [(ridx+1, cidx); (ridx-1, cidx); (ridx, cidx+1); (ridx, cidx-1); (ridx+1, cidx+1); (ridx+1, cidx-1); (ridx-1, cidx+1); (ridx-1, cidx-1)]
                |> List.filter ~f:(fun idx -> match idx with | (next_r, next_c) -> not (is_blocked board ridx cidx next_r next_c))

    | Queen(_) -> List.filter ~f:(fun idx -> match idx with | (r, c) -> r >= 0 && r < 8 && c >= 0 && c < 8) 
                    (
                    (List.init 8 ~f:(fun i -> (ridx+i, cidx))) @ (List.init 8 ~f:(fun i -> (ridx, cidx+i))) 
                    @ (List.init 8 ~f:(fun i -> (ridx-i, cidx))) @ (List.init 8 ~f:(fun i -> (ridx, cidx-i)))
                    @ (List.init 8 ~f:(fun i -> (ridx+i, cidx+i))) @ (List.init 8 ~f:(fun i -> (ridx-i, cidx-i)))
                    @ (List.init 8 ~f:(fun i -> (ridx+i, cidx-i))) @ (List.init 8 ~f:(fun i -> (ridx-i, cidx+i)))
                    )
                    |> List.filter ~f:(fun idx -> match idx with | (next_r, next_c) -> not (is_blocked board ridx cidx next_r next_c))
    | _ -> []


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
                        else 
                        let possible_moves = get_possible_moves board chess ridx cidx in
                        append_possible_moves_to_map chess curr_map possible_moves


let get_next_step_map (board: board) (curr_player: player) : pos list list list =
    let curr_map = ref (List.init 8 ~f:(fun _ -> List.init 8 ~f:(fun _ -> []))) in
    let (_: board) = List.mapi board ~f:(fun ridx r -> List.mapi r ~f:(fun cidx pos -> curr_map := add_next_step board pos ridx cidx curr_player (!curr_map); pos)) in
    !curr_map

let find_king (board: board) (target_player: player) : (int * int) =
    let king_pos = ref (-1, -1) in
    let (_: board) = List.mapi board ~f:(fun ridx r -> List.mapi r ~f:(fun cidx pos -> match pos with 
                                                                                    | Occupied(King(p)) -> if equal_player p target_player then king_pos := (ridx, cidx); pos
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

(* TODO: perform all possible moves and see if still checked *)
let is_checkmate (_: board) (_: player) : bool =
    false

let get_condition (board: board) (curr_player: player) : condition =
    if is_check board curr_player then
        if is_checkmate board curr_player then Checkmate
        else Check
    else Normal

(* TODO: 
1. validate movement based on chess piece type
2. must move king when checked*)
let validate (board: board) (f: int * int) (t: (int * int)) : bool =
    match f, t with
    | (rf, cf), (rt, ct) -> if rf = rt && cf = ct then false
    else
    match (get_board_pos board f), (get_board_pos board t) with
    | _, None -> false
    | None, _ -> false
    | Some(Empty), _ -> false
    | Some(Occupied(cf)), Some(Occupied(ct)) -> not (equal_player (get_player cf) (get_player ct))
    | Some(Occupied(_)), Some(Empty) -> true

let move (board: board) (f: int * int) (t: (int * int)) : (board * condition) =
    if not (validate board f t) then (board, Fail("invalid move")) else
    match get_board_pos board f with
    | Some(Occupied(curr_chess)) -> 
                    (
                    let (rt, _) = t in
                    (* Pawn Promotion. TODO: allow choosing piece type promoted to*)
                    let next_pos = 
                    (if not (((equal_chess curr_chess (Pawn(White))) || (equal_chess curr_chess (Pawn(Black)))) && (rt = 0 || rt = 7)) then Occupied(curr_chess)
                    else Occupied(Queen(get_player curr_chess))) 
                    in                    
                    match set_board_pos board t next_pos with
                    | Some(nb) -> (match set_board_pos nb f Empty with
                                    | Some(nb1) -> (nb1, Normal)
                                    | None -> (board, Fail("fail to set piece")))
                    | None -> (board, Fail("fail to set piece"))
                    )
    | _ -> (board, Fail("fail to get piece"))