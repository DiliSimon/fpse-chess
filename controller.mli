(* main control loop *)
val main : () -> ()

val save_game : board -> string -> string Option

val read_game : string -> board Option
