open Core

type player = White | Black [@@deriving equal]
type chess = King of player | Queen of player | Rook of player | Bishop of player | Knight of player | Pawn of player [@@deriving equal]
type pos = Occupied of chess | Empty [@@deriving equal]
type board = pos list list [@@deriving equal]
type condition = Check | Checkmate | Fail | Normal [@@deriving equal]

exception PosErr of string

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
    | 7, _ -> Occupied(Pawn(Black))
    | 0, 0 -> Occupied(Rook(White))
    | 0, 1 -> Occupied(Knight(White))
    | 0, 2 -> Occupied(Bishop(White))
    | 0, 3 -> Occupied(Queen(White))
    | 0, 4 -> Occupied(King(White))
    | 0, 5 -> Occupied(Bishop(White))
    | 0, 6 -> Occupied(Knight(White))
    | 0, 7 -> Occupied(Rook(White))
    | 6, 0 -> Occupied(Rook(Black))
    | 6, 1 -> Occupied(Knight(Black))
    | 6, 2 -> Occupied(Bishop(Black))
    | 6, 3 -> Occupied(Queen(Black))
    | 6, 4 -> Occupied(King(Black))
    | 6, 5 -> Occupied(Bishop(Black))
    | 6, 6 -> Occupied(Knight(Black))
    | 6, 7 -> Occupied(Rook(Black))
    | _, _ -> raise (PosErr "invalid pos")

let init_board _ : board =
    List.init 8 ~f:(fun rn -> List.init 8 ~f:(fun cn -> init_pos rn cn))

let get_board_pos (board: board) (idx: int * int): pos option =
    match idx with
    | (row, col) -> (match (List.nth board row) with
                    | Some(rl) -> List.nth rl col
                    | None -> None)

let set_board_pos (board: board) (idx: int * int) (pos: pos) : board option =
    match idx with
    | (row, col) -> if row < 0 || row >= 8 || col < 0 || col >= 8 then None
                    else
                    Some(List.mapi board ~f:(fun ridx  r -> if ridx = row then
                                                        List.mapi r ~f:(fun cidx c -> if cidx = col then pos else c)
                                                        else r)
                        )

let get_condition (_: board) : condition =
    Normal

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
    if not (validate board f t) then (board, Fail) else
    match get_board_pos board t with
    | Some(p) -> (match set_board_pos board t p with
                                | Some(nb) -> (nb, Normal)
                                | None -> (board, Fail))
    | None -> (board, Fail)