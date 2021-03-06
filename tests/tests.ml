
(* open Core;; *)
open OUnit2;;
open Board;;
open Bot;;
open Gui;;

module BaseMinimaxBot = MinimaxBot(Bot.BaseEval);;

let board_initial = init_board ();;
let board_check = 
  (List.init 8 (fun _ -> (List.init 8 (fun _ -> Empty)))) 
  |> set_board_pos_exn ~idx:(2, 5) ~pos:(Occupied(King((White, true))))
  |> set_board_pos_exn ~idx:(2, 6) ~pos:(Occupied(Bishop(White))) 
  |> set_board_pos_exn ~idx:(5, 3) ~pos:(Occupied(King((Black, true))))
  |> set_board_pos_exn ~idx:(5, 5) ~pos:(Occupied(Bishop(Black)))
  |> set_board_pos_exn ~idx:(4, 7) ~pos:(Occupied(Knight(Black)))
;;

let board_check_2 = 
  (List.init 8 (fun _ -> (List.init 8 (fun _ -> Empty))))
  |> set_board_pos_exn ~idx:(7, 5) ~pos:(Occupied(King(White, true)))
  |> set_board_pos_exn ~idx:(5, 3) ~pos:(Occupied(King(Black, true)))
  |> set_board_pos_exn ~idx:(1, 1) ~pos:(Occupied(Rook(Black, true)))
  |> set_board_pos_exn ~idx:(1, 2) ~pos:(Occupied(Queen(Black)))
;;

let board_checkmate = 
  (List.init 8 (fun _ -> (List.init 8 (fun _ -> Empty)))) 
  |> set_board_pos_exn ~idx:(7, 4) ~pos:(Occupied(King((Black, false))))
  |> set_board_pos_exn ~idx:(4, 7) ~pos:(Occupied(Rook(Black, true))) 
  |> set_board_pos_exn ~idx:(0, 7) ~pos:(Occupied(Queen((Black))))
  |> set_board_pos_exn ~idx:(1, 5) ~pos:(Occupied(Pawn(White)))
  |> set_board_pos_exn ~idx:(1, 6) ~pos:(Occupied(Pawn(White)))
  |> set_board_pos_exn ~idx:(0, 5) ~pos:(Occupied(Rook(White, true)))
  |> set_board_pos_exn ~idx:(0, 6) ~pos:(Occupied(King(White, true)))
;;

let board_checkmate_one_step = 
  (List.init 8 (fun _ -> (List.init 8 (fun _ -> Empty)))) 
  |> set_board_pos_exn ~idx:(7, 4) ~pos:(Occupied(King((Black, false))))
  |> set_board_pos_exn ~idx:(4, 7) ~pos:(Occupied(Rook(Black, true))) 
  (* Black move queen to (0, 7) to checkmate *)
  |> set_board_pos_exn ~idx:(2, 7) ~pos:(Occupied(Queen((Black))))
  |> set_board_pos_exn ~idx:(1, 5) ~pos:(Occupied(Pawn(White)))
  |> set_board_pos_exn ~idx:(1, 6) ~pos:(Occupied(Pawn(White)))
  |> set_board_pos_exn ~idx:(0, 5) ~pos:(Occupied(Rook(White, true)))
  |> set_board_pos_exn ~idx:(0, 6) ~pos:(Occupied(King(White, true)))
;;

let board_castling = 
  (List.init 8 (fun _ -> (List.init 8 (fun _ -> Empty)))) 
  |> set_board_pos_exn ~idx:(7, 4) ~pos:(Occupied(King((Black, false))))
  |> set_board_pos_exn ~idx:(7, 0) ~pos:(Occupied(Rook(Black, false))) 
  |> set_board_pos_exn ~idx:(0, 4) ~pos:(Occupied(King((White, false))))
  |> set_board_pos_exn ~idx:(0, 7) ~pos:(Occupied(Rook(White, false)))
;;

let board_castling_black_under_attack = 
  (List.init 8 (fun _ -> (List.init 8 (fun _ -> Empty)))) 
  |> set_board_pos_exn ~idx:(7, 4) ~pos:(Occupied(King((Black, false))))
  |> set_board_pos_exn ~idx:(7, 0) ~pos:(Occupied(Rook(Black, false))) 
  |> set_board_pos_exn ~idx:(0, 4) ~pos:(Occupied(King((White, false))))
  |> set_board_pos_exn ~idx:(0, 7) ~pos:(Occupied(Rook(White, false)))
  |> set_board_pos_exn ~idx:(6, 4) ~pos:(Occupied(Bishop(White)))
;;

let board_castling_black_under_attack_2 = 
  (List.init 8 (fun _ -> (List.init 8 (fun _ -> Empty)))) 
  |> set_board_pos_exn ~idx:(7, 4) ~pos:(Occupied(King((Black, false))))
  |> set_board_pos_exn ~idx:(7, 0) ~pos:(Occupied(Rook(Black, false))) 
  |> set_board_pos_exn ~idx:(0, 4) ~pos:(Occupied(King((White, false))))
  |> set_board_pos_exn ~idx:(0, 7) ~pos:(Occupied(Rook(White, false)))
  |> set_board_pos_exn ~idx:(6, 4) ~pos:(Occupied(Rook(White, true)))
;;

let board_castling_empty_last = init_board ()
  |> set_board_pos_exn ~idx:(7, 1) ~pos:(Empty)
  |> set_board_pos_exn ~idx:(7, 2) ~pos:(Empty)
  |> set_board_pos_exn ~idx:(7, 3) ~pos:(Empty)
  |> set_board_pos_exn ~idx:(7, 5) ~pos:(Empty)
  |> set_board_pos_exn ~idx:(7, 6) ~pos:(Empty)
  |> set_board_pos_exn ~idx:(0, 1) ~pos:(Empty)
  |> set_board_pos_exn ~idx:(0, 2) ~pos:(Empty)
  |> set_board_pos_exn ~idx:(0, 3) ~pos:(Empty)
  |> set_board_pos_exn ~idx:(0, 5) ~pos:(Empty)
  |> set_board_pos_exn ~idx:(0, 6) ~pos:(Empty)
;;

let board_castling_black_moved = 
  (List.init 8 (fun _ -> (List.init 8 (fun _ -> Empty)))) 
  |> set_board_pos_exn ~idx:(7, 4) ~pos:(Occupied(King((Black, true))))
  |> set_board_pos_exn ~idx:(7, 0) ~pos:(Occupied(Rook(Black, false))) 
  |> set_board_pos_exn ~idx:(0, 4) ~pos:(Occupied(King((White, false))))
  |> set_board_pos_exn ~idx:(0, 7) ~pos:(Occupied(Rook(White, false)))
;;

let board_check_3 =
  (List.init 8 (fun _ -> (List.init 8 (fun _ -> Empty)))) 
  |> set_board_pos_exn ~idx:(7, 0) ~pos:(Occupied(Rook(Black, false))) 
  |> set_board_pos_exn ~idx:(7, 1) ~pos:(Occupied(Knight((Black))))
  |> set_board_pos_exn ~idx:(7, 2) ~pos:(Occupied(Bishop((Black))))
  |> set_board_pos_exn ~idx:(7, 3) ~pos:(Occupied(King((Black, false))))
  |> set_board_pos_exn ~idx:(7, 4) ~pos:(Occupied(Queen((Black))))
  |> set_board_pos_exn ~idx:(0, 0) ~pos:(Occupied(Rook(White, false))) 
  |> set_board_pos_exn ~idx:(0, 1) ~pos:(Occupied(Knight((White))))
  |> set_board_pos_exn ~idx:(0, 2) ~pos:(Occupied(Bishop((White))))
  |> set_board_pos_exn ~idx:(0, 4) ~pos:(Occupied(King((White, true))))
  |> set_board_pos_exn ~idx:(0, 6) ~pos:(Occupied(Knight((White))))
  |> set_board_pos_exn ~idx:(0, 7) ~pos:(Occupied(Rook((White, false))))
  |> set_board_pos_exn ~idx:(5, 0) ~pos:(Occupied(Pawn((Black))))
  |> set_board_pos_exn ~idx:(3, 7) ~pos:(Occupied(Pawn((White))))

let board_wo_white_king = 
  init_board ()
  |> set_board_pos_exn ~idx:(0, 4) ~pos:(Empty)

let board_pawn_promotion =
  (List.init 8 (fun _ -> (List.init 8 (fun _ -> Empty)))) 
  |> set_board_pos_exn ~idx:(7, 3) ~pos:(Occupied(King((Black, false))))
  |> set_board_pos_exn ~idx:(0, 4) ~pos:(Occupied(King((White, true))))
  |> set_board_pos_exn ~idx:(6, 6) ~pos:(Occupied(Pawn((White))))

let test_is_blocked _ =
  assert_equal (is_blocked board_initial 0 0 4 0) @@ true;
  assert_equal (is_blocked board_initial 1 0 4 0) @@ false;
  (* diagnally capture opponent's piece *)
  assert_equal (is_blocked board_initial 1 0 6 5) @@ false;
  (* path blocked by piece of oneself *)
  assert_equal (is_blocked board_initial 0 0 1 0) @@ true;
  (* path blocked by piece of other opponent *)
  assert_equal (is_blocked board_initial 1 0 7 0) @@ true;
  assert_raises (PosErr "position doesn't contain piece") (fun () -> is_blocked board_check_3 3 0 7 0);
  assert_raises (PosErr "invalid pos") (fun () -> is_blocked board_check_3 9 0 7 0)

let test_find_king _ =
  assert_equal (find_king board_initial White) @@ (0, 4);
  assert_equal (find_king board_initial Black) @@ (7, 4)

let test_get_possible_moves _ =
  assert_equal (get_possible_moves board_initial (King((White, true))) 0 4) @@ [];
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
 [[]; []; [Occupied (King(Black, true))]; [Occupied (King(Black, true))];
  [Occupied (Bishop Black); Occupied (King(Black, true))]; [];
  [Occupied (Bishop Black)]; []];
 [[]; []; [Occupied (King(Black, true))]; []; [Occupied (King(Black, true))]; []; []; []];
 [[]; []; [Occupied (King(Black, true))]; [Occupied (King(Black, true))];
  [Occupied (Bishop Black); Occupied (King(Black, true))]; [];
  [Occupied (Bishop Black); Occupied (Knight Black)]; []];
 [[]; []; []; [Occupied (Bishop Black)]; []; []; []; [Occupied (Bishop Black)]]])

let test_is_check _ =
  assert_bool "White is checked" (is_check board_check White);
  assert_bool "Black is not checked" (not (is_check board_check Black));
  assert_bool "White is checked" (is_check board_check_3 Black);
  assert_raises (PosErr "fail to extract from step map") (fun () -> is_check board_wo_white_king Black)

let test_move _ =
  (*  normal move *)
  assert_equal (move board_check White (2, 6) (4, 4)) 
  @@ ((set_board_pos_exn board_check ~idx:(4, 4) ~pos:(Occupied(Bishop(White))) |> set_board_pos_exn ~idx:(2, 6) ~pos:Empty), Check);
  (*  normal move *)
  assert_equal (move board_check_2 Black (1, 1) (0, 1))
  @@ ((set_board_pos_exn board_check_2 ~idx:(0, 1) ~pos:(Occupied(Rook(Black, true))) |> set_board_pos_exn ~idx:(1, 1) ~pos:Empty), Normal);
  (*  normal move *)
  assert_equal (move board_check_2 Black (1, 2) (0, 2)) 
  @@ ((set_board_pos_exn board_check_2 ~idx:(0, 2) ~pos:(Occupied(Queen(Black))) |> set_board_pos_exn ~idx:(1, 2) ~pos:Empty), Normal); 
  (* invalid move blocked by other pieces *)
  assert_equal (move board_check White (2, 6) (6, 2)) 
  @@ (board_check, Fail("invalid move"));
  (* move doesn't match piece type *)
  assert_equal (move board_check White (2, 6) (1, 6)) 
  @@ (board_check, Fail("invalid move"));
  (* still checked after move *)
  assert_equal (move board_check Black (5, 3) (4, 4)) 
  @@ (board_check, Fail("still checked after move"));
  (* Black knight captures white bishop *)
  assert_equal (move board_check Black (4, 7) (2, 6)) 
  @@ ((set_board_pos_exn board_check ~idx:(2, 6) ~pos:(Occupied(Knight(Black))) |> set_board_pos_exn ~idx:(4, 7) ~pos:Empty), Normal);
  (* Black checkmates white *)
  assert_equal (move board_checkmate_one_step Black (2, 7) (0, 7))
  @@ (board_checkmate, Checkmate);
  (* Pawn Promotion *)
  assert_equal (move board_pawn_promotion White (6, 6) (7, 6))
  @@ ((board_pawn_promotion |> set_board_pos_exn ~idx:(6, 6) ~pos:Empty |> set_board_pos_exn ~idx:(7, 6) ~pos:(Occupied(Queen(White)))), Check)

let test_validate _ =
  assert_equal (validate board_check White (9, 2) (1, 3)) @@ false;
  assert_equal (validate board_check White (3, 2) (-1, 3)) @@ false;
  assert_equal (validate board_check White (3, 2) (1, 3)) @@ false;
  assert_equal (validate board_check White (2, 6) (3, 5)) @@ true

let test_castling _ =
  assert_equal (castling board_castling Black false) @@
  Some((List.init 8 (fun _ -> (List.init 8 (fun _ -> Empty)))) 
          |> set_board_pos_exn ~idx:(7, 2) ~pos:(Occupied(King((Black, true))))
          |> set_board_pos_exn ~idx:(7, 3) ~pos:(Occupied(Rook(Black, true))) 
          |> set_board_pos_exn ~idx:(0, 4) ~pos:(Occupied(King((White, false))))
          |> set_board_pos_exn ~idx:(0, 7) ~pos:(Occupied(Rook(White, false))));
  assert_equal (castling board_castling White true) @@
  Some((List.init 8 (fun _ -> (List.init 8 (fun _ -> Empty)))) 
          |> set_board_pos_exn ~idx:(7, 4) ~pos:(Occupied(King((Black, false))))
          |> set_board_pos_exn ~idx:(7, 0) ~pos:(Occupied(Rook(Black, false))) 
          |> set_board_pos_exn ~idx:(0, 6) ~pos:(Occupied(King((White, true))))
          |> set_board_pos_exn ~idx:(0, 5) ~pos:(Occupied(Rook(White, true))));
  assert_equal (castling board_castling_empty_last Black true) @@
  Some (
    board_castling_empty_last
    |> set_board_pos_exn ~idx:(7, 4) ~pos:(Empty)
    |> set_board_pos_exn ~idx:(7, 7) ~pos:(Empty)
    |> set_board_pos_exn ~idx:(7, 6) ~pos:(Occupied(King((Black, true))))
    |> set_board_pos_exn ~idx:(7, 5) ~pos:(Occupied(Rook(Black, true))) 
    );
  assert_equal (castling board_castling_empty_last White false) @@
  Some (
    board_castling_empty_last
    |> set_board_pos_exn ~idx:(0, 0) ~pos:(Empty)
    |> set_board_pos_exn ~idx:(0, 4) ~pos:(Empty)
    |> set_board_pos_exn ~idx:(0, 2) ~pos:(Occupied(King((White, true))))
    |> set_board_pos_exn ~idx:(0, 3) ~pos:(Occupied(Rook(White, true))) 
    );
  assert_equal (castling board_castling White false) @@ None;
  assert_equal (castling board_check White true) @@ None;
  assert_equal (castling board_initial White true) @@ None;
  (* black king still in inital position but have been moved *)
  assert_equal (castling board_castling_black_moved Black false) @@ None;
  (* black king will pass a position attackable by white Bishop *)
  assert_equal (castling board_castling_black_under_attack Black false) @@ None;
  assert_equal (castling board_castling_black_under_attack Black true) @@ None;
  (* Black king checked *)
  assert_equal (castling board_castling_black_under_attack_2 Black true) @@ None

let test_eval_board _ =
  assert_equal (BaseEval.compare (BaseEval.eval board_initial Black) (BaseEval.eval board_initial White)) @@ 0;
  assert_bool "Black has more pieces" ((BaseEval.compare (BaseEval.eval board_check Black) (BaseEval.eval board_check White)) > 0);
  assert_bool "Black has queen & check" ((BaseEval.compare (BaseEval.eval board_check_3 Black) (BaseEval.eval board_check_3 White)) > 0)

let test_checkmate _ =
  assert_bool "White should be checkmated" (is_checkmate board_checkmate Black);
  assert_bool "White is not checkmated" (not(is_checkmate board_checkmate_one_step Black));
  assert_bool "Black not checkmated" (not (is_checkmate board_check_3 White))

let test_get_best_move _ =
  assert_equal (BaseMinimaxBot.get_best_move board_check_3 White) @@ Some((0, 4),(0, 5));
  assert_equal (BaseMinimaxBot.get_best_move board_check_2 Black) @@ Some((5, 3), (6, 3));
  assert_equal (BaseMinimaxBot.get_best_move board_check_2 White) @@ Some((7, 5), (6, 5))

let test_misc _ =
  assert_bool "" (equal_player Black Black);
  assert_bool "" (not(equal_player White Black));
  assert_bool "" (equal_board board_initial board_initial);
  assert_bool "" (equal_chess (Knight(Black)) (Knight(Black)))

let test_print_board _ = 
  let b_1 = [[Occupied(Bishop(White));Occupied(Knight(Black))];[Empty;Occupied(Pawn(White))]]
  in 
  assert_equal (print_board b_1 White) ()

let test_print_result _ =
  assert_equal (print_result White) ()

let board_tests =
  "Board test" >: test_list [
    "test_is_unblocked" >:: test_is_blocked;
    "test_find_king" >:: test_find_king;
    "test_get_possible_moves" >:: test_get_possible_moves;
    "test_get_next_step_map" >:: test_get_next_step_map;
    "test_is_check" >:: test_is_check;
    "test_move" >:: test_move;
    "test_castling" >:: test_castling;
    "test_checkmate" >:: test_checkmate;
  ]

let bot_tests = 
  "Bot test" >: test_list [
    "test_get_best_move" >:: test_get_best_move;
    "test_eval_board" >:: test_eval_board;
    "test_validate" >:: test_validate;
    "test_misc" >:: test_misc
  ]

let gui_tests =
  "Gui test" >: test_list [
    "test_print_board" >:: test_print_board;
    "test_print_result" >:: test_print_result
  ]

let series =
  "All Tests" >::: [
    board_tests;
    bot_tests;
    gui_tests;
  ]

let () = 
  run_test_tt_main series

