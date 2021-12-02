(*
  Put the tests for lib.ml functions here
*)

(* open Core;; *)
open OUnit2;;
let section1_tests =
  "Section 1" >: test_list [
  ]

let series =
  "Assignment2 Tests" >::: [
  ]

let () = 
  run_test_tt_main series

