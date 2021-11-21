type player
type chess
type pos
type board
type condition

val init_board : unit -> board

(* move the chess from pos A to pos B *)
val move: board -> (int * int) -> (int * int) -> (board * condition)

(* validate move, return false if the move is not valid *)
val validate: board -> (int * int) -> (int * int) -> bool

(* check for check and checkmate *)
val get_condition: board -> condition



