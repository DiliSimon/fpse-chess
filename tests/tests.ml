(*
  Put the tests for lib.ml functions here
*)

(* open Core;; *)
open OUnit2;;
open Board;;

let board_initial = init_board ();;
let board_check = (List.init 8 (fun _ -> (List.init 8 (fun _ -> Empty)))) 
          |> set_board_pos_exn ~idx:(2, 5) ~pos:(Occupied(King(White)))
          |> set_board_pos_exn ~idx:(2, 6) ~pos:(Occupied(Bishop(White))) 
          |> set_board_pos_exn ~idx:(5, 3) ~pos:(Occupied(King(Black)))
          |> set_board_pos_exn ~idx:(5, 5) ~pos:(Occupied(Bishop(Black)))
          |> set_board_pos_exn ~idx:(4, 7) ~pos:(Occupied(Knight(Black)))
;;

let test_is_blocked _ =
  assert_equal (is_blocked board_initial 0 0 4 0) @@ true;
  assert_equal (is_blocked board_initial 1 0 4 0) @@ false;
  (* diagnally capture opponent's piece *)
  assert_equal (is_blocked board_initial 1 0 6 5) @@ false;
  (* path blocked by piece of oneself *)
  assert_equal (is_blocked board_initial 0 0 1 0) @@ true;
  (* path blocked by piece of other opponent *)
  assert_equal (is_blocked board_initial 1 0 7 0) @@ true

let test_find_king _ =
  assert_equal (find_king board_initial White) @@ (0, 4);
  assert_equal (find_king board_initial Black) @@ (7, 4)

let test_get_possible_moves _ =
  assert_equal (get_possible_moves board_initial (King(White)) 0 4) @@ [];
  assert_equal (get_possible_moves board_initial (Pawn(White)) 1 3) @@ [(2, 3); (3, 3)];
  assert_equal (get_possible_moves board_initial (Pawn(Black)) 6 3) @@ [(5, 3); (4, 3)];
  assert_equal (get_possible_moves board_check (Knight(Black)) 4 7) @@ [(6, 6); (2, 6); (3, 5)]
  
let test_get_next_step_map _ =
  assert_equal (get_next_step_map board_check Black) @@ 
  ([[[Occupied (Bishop Black)]; []; []; []; []; []; []; []];
 [[]; [Occupied (Bishop Black)]; []; []; []; []; []; []];
 [[]; []; [Occupied (Bishop Black)]; []; []; []; [Occupied (Knight Black)]; []];
 [[]; []; []; [Occupied (Bishop Black)]; []; [Occupied (Knight Black)]; 
  []; [Occupied (Bishop Black)]];
 [[]; []; [Occupied (King Black)]; [Occupied (King Black)];
  [Occupied (Bishop Black); Occupied (King Black)]; [];
  [Occupied (Bishop Black)]; []];
 [[]; []; [Occupied (King Black)]; []; [Occupied (King Black)]; []; []; []];
 [[]; []; [Occupied (King Black)]; [Occupied (King Black)];
  [Occupied (Bishop Black); Occupied (King Black)]; [];
  [Occupied (Bishop Black); Occupied (Knight Black)]; []];
 [[]; []; []; [Occupied (Bishop Black)]; []; []; []; [Occupied (Bishop Black)]]])

let test_is_check _ =
  assert_bool "White is checked" (is_check board_check White);
  assert_bool "Black is not checked" (not (is_check board_check Black))

let section1_tests =
  "Section 1" >: test_list [
    "test_is_unblocked" >:: test_is_blocked;
    "test_find_king" >:: test_find_king;
    "test_get_possible_moves" >:: test_get_possible_moves;
    "test_get_next_step_map" >:: test_get_next_step_map;
    "test_is_check" >:: test_is_check
  ]

let series =
  "All Tests" >::: [
    section1_tests
  ]

let () = 
  run_test_tt_main series

