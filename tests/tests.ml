
(* open Core;; *)
open OUnit2;;
open Board;;

let board_initial = init_board ();;
let board_check = (List.init 8 (fun _ -> (List.init 8 (fun _ -> Empty)))) 
          |> set_board_pos_exn ~idx:(2, 5) ~pos:(Occupied(King((White, true))))
          |> set_board_pos_exn ~idx:(2, 6) ~pos:(Occupied(Bishop(White))) 
          |> set_board_pos_exn ~idx:(5, 3) ~pos:(Occupied(King((Black, true))))
          |> set_board_pos_exn ~idx:(5, 5) ~pos:(Occupied(Bishop(Black)))
          |> set_board_pos_exn ~idx:(4, 7) ~pos:(Occupied(Knight(Black)))
;;

let board_check_2 = (List.init 8 (fun _ -> (List.init 8 (fun _ -> Empty))))
  |> set_board_pos_exn ~idx:(7, 5) ~pos:(Occupied(King(White, true)))
  |> set_board_pos_exn ~idx:(5, 3) ~pos:(Occupied(King(Black, true)))
  |> set_board_pos_exn ~idx:(1, 1) ~pos:(Occupied(Rook(Black, true)))
  |> set_board_pos_exn ~idx:(1, 2) ~pos:(Occupied(Queen(Black)))
;;

let board_castling = (List.init 8 (fun _ -> (List.init 8 (fun _ -> Empty)))) 
          |> set_board_pos_exn ~idx:(7, 4) ~pos:(Occupied(King((Black, false))))
          |> set_board_pos_exn ~idx:(7, 0) ~pos:(Occupied(Rook(Black, false))) 
          |> set_board_pos_exn ~idx:(0, 4) ~pos:(Occupied(King((White, false))))
          |> set_board_pos_exn ~idx:(0, 7) ~pos:(Occupied(Rook(White, false)))
;;

let board_castling_black_moved = (List.init 8 (fun _ -> (List.init 8 (fun _ -> Empty)))) 
          |> set_board_pos_exn ~idx:(7, 4) ~pos:(Occupied(King((Black, true))))
          |> set_board_pos_exn ~idx:(7, 0) ~pos:(Occupied(Rook(Black, false))) 
          |> set_board_pos_exn ~idx:(0, 4) ~pos:(Occupied(King((White, false))))
          |> set_board_pos_exn ~idx:(0, 7) ~pos:(Occupied(Rook(White, false)))
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
  assert_bool "Black is not checked" (not (is_check board_check Black))

let test_move _ =
  (*  normal move *)
  assert_equal (move board_check White (2, 6) (4, 4)) 
  @@ ((set_board_pos_exn board_check ~idx:(4, 4) ~pos:(Occupied(Bishop(White))) |> set_board_pos_exn ~idx:(2, 6) ~pos:Empty), Normal);
  (*  normal move *)
  assert_equal (move board_check_2 Black (1, 1) (0, 1))
  @@ ((set_board_pos_exn board_check_2 ~idx:(0, 1) ~pos:(Occupied(Rook(Black, true))) |> set_board_pos_exn ~idx:(1, 1) ~pos:Empty), Normal);
  (*  normal move *)
  assert_equal (move board_check_2 Black (1, 2) (0, 2)) 
  @@ ((set_board_pos_exn board_check_2 ~idx:(0, 2) ~pos:(Occupied(Queen(Black))) |> set_board_pos_exn ~idx:(1, 2) ~pos:Empty), Normal); 
  (* normal move *)
  assert_equal (move board_check White (2, 6) (5, 3)) 
  @@ ((set_board_pos_exn board_check ~idx:(5, 3) ~pos:(Occupied(Bishop(White))) |> set_board_pos_exn ~idx:(2, 6) ~pos:Empty), Normal);
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
  @@ ((set_board_pos_exn board_check ~idx:(2, 6) ~pos:(Occupied(Knight(Black))) |> set_board_pos_exn ~idx:(4, 7) ~pos:Empty), Normal)

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
  assert_equal (castling board_castling White false) @@ None;
  assert_equal (castling board_check White true) @@ None;
  assert_equal (castling board_initial White true) @@ None;
  (* black king still in inital position but have been moved *)
  assert_equal (castling board_castling_black_moved Black false) @@ None


let section1_tests =
  "Section 1" >: test_list [
    "test_is_unblocked" >:: test_is_blocked;
    "test_find_king" >:: test_find_king;
    "test_get_possible_moves" >:: test_get_possible_moves;
    "test_get_next_step_map" >:: test_get_next_step_map;
    "test_is_check" >:: test_is_check;
    "test_move" >:: test_move;
    "test_castling" >:: test_castling
  ]

let series =
  "All Tests" >::: [
    section1_tests
  ]

let () = 
  run_test_tt_main series

