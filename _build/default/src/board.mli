type player = White | Black [@@deriving equal]
type chess = King of player | Queen of player | Rook of player | Bishop of player | Knight of player | Pawn of player [@@deriving equal]
type pos = Occupied of chess | Empty [@@deriving equal]
type board = pos list list [@@deriving equal]
type condition = Check | Checkmate | Fail of string | Normal [@@deriving equal]

val init_board : unit -> board

(* move the chess from pos A to pos B *)
val move: board -> (int * int) -> (int * int) -> (board * condition)

(* validate move, return false if the move is not valid *)
val validate: board -> (int * int) -> (int * int) -> bool

(* check for check and checkmate *)
val get_condition: board -> player -> condition

val get_board_pos: 'a list list -> (int * int) -> 'a option

val set_board_pos: 'a list list -> (int * int) -> 'a -> 'a list list option

val is_blocked: board -> (int) -> int -> int -> int -> bool

val get_possible_moves: board -> chess -> int -> int -> (int * int) list

val append_possible_moves_to_map: chess -> pos list list list -> (int * int) list -> pos list list list

val get_next_step_map: board -> player -> pos list list list

val is_check: board -> player -> bool

val find_king: board -> player -> (int * int)

val is_checkmate : board -> player -> bool