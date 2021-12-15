type player = White | Black [@@deriving equal]
type chess = King of (player * bool) | Queen of player | Rook of (player * bool) | Bishop of player | Knight of player | Pawn of player [@@deriving equal]
type pos = Occupied of chess | Empty [@@deriving equal]
type board = pos list list [@@deriving equal]
type condition = Check | Checkmate | Fail of string | Normal

exception PosErr of string

val init_board : unit -> board

(* Move the chess from pos A to pos B *)
val move: board -> player -> (int * int) -> (int * int) -> (board * condition)

(* Validate move, return false if the move is not valid *)
val validate: board -> player -> (int * int) -> (int * int) -> bool

(* Returns none if castling is invalid *)
val castling : board -> player -> bool -> board option

(* Returns opponent of given player *)
val opponent_of: player -> player

(* Returns pos at given index; throws exception directly *)
val get_board_pos_exn: 'a list list -> (int * int) -> 'a

(* Returns pos at given index; returns None if error *)
val get_board_pos: 'a list list -> (int * int) -> 'a option

(* Set given index of board; throws exception directly *)
val set_board_pos_exn: 'a list list -> idx: (int * int) -> pos: 'a -> 'a list list

(* Set given index of board; returns None if error *)
val set_board_pos: 'a list list -> (int * int) -> 'a -> 'a list list option

(* 
 Returns true if the line from (x, y) to (x', y') contains any pieces,
 i.e. a piece like Rook can't go from (x, y) to (x', y')
*)
val is_blocked: board -> (int) -> int -> int -> int -> bool

(* Returns all possible next steps for a piece at (x, y) *)
val get_possible_moves: board -> chess -> int -> int -> (int * int) list

(* Return all possible moves of the form (src, dst) of given player *)
val get_all_possible_moves: board -> player -> ((int * int) * (int * int)) list

val get_next_step_map: board -> player -> pos list list list

(* Returns true if the given player has checked his opponent *)
val is_check: board -> player -> bool

(* Returns true if the given player has checkmated his opponent *)
val is_checkmate : board -> player -> bool

(* Returns the index of the player's king *)
val find_king: board -> player -> (int * int)
