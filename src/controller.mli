(* PvP or PvE *)
(* val mode *)

(* main control loop *)
(* val main : () -> () *)
open Board

val save_game : board -> string -> unit

val read_game : string -> board