type player = White | Black
type chess = King of player | Queen of player | Rook of player | Bishop of player | Knight of player | Pawn of player
type pos = chess | Empty
type board = pos list list
type condition = Check | Checkmate | Fail | Normal

val init_board : unit -> board

(* move the chess from pos A to pos B *)
val move: board -> (int * int) -> (int * int) -> condition

(* validate move *)
val validate: board -> (int * int) -> (int * int) -> bool

(* check for check and checkmate *)
val get_condition: board -> condition



