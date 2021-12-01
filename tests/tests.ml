(*
  Put the tests for lib.ml functions here
*)

open Core;;
open OUnit2;;
open Lib;;


(* module H = Histo *)
let test_chunks _ =
  assert_equal (Lib.chunks 3 [1; 2; 3; 4; 5]) @@ [[1;2;3]; [2;3;4]; [3;4;5]];
  assert_equal (Lib.chunks 2 ["a"; "b"; "c"; "d"; "e"]) @@ [ ["a";"b"]; ["b";"c"]; ["c";"d"]; ["d";"e"] ];
  assert_equal (Lib.chunks 3 [1; 2;]) @@ []

let test_split_last _ = 
  assert_equal (split_last [1;2;3]) @@ (3, [1;2]);
  assert_equal (split_last [1]) @@ (1, [])

module Int_List_Key = Lib.List_key(Int);;

let test_compare _ =
  assert_equal (Int_List_Key.compare [1; 2; 3] [1; 2; 3; 4]) @@ (-1);
  assert_equal (Int_List_Key.compare [1; 2; 3] []) @@ (1);
  assert_equal (Int_List_Key.compare [1; 2; 4; 3] [1; 3; 2] > 0) @@ (false)

let communicative (a: Int_List_Key.t) =
  assert_equal (Int_List_Key.t_of_sexp (Int_List_Key.sexp_of_t a)) @@ a

let test_sexp_invariants _ =
  communicative [1; 2; 3; 4];
  communicative []

module Fake_Random : Randomness = struct
  let int (_:int) : int =
    0
end

let test_sample _ =
  let b = Bag.create () in
  assert_equal (sample (module Fake_Random) b) @@ None;
  let _ = Bag.add b 3 in
  assert_equal (sample (module Fake_Random) b) @@ Some(3);
  let _ = Bag.add b 4 in
  assert_equal (sample (module Fake_Random) b) @@ Some(4)

module Int_NGram = N_grams(Fake_Random)(Int);;

let find_int (l: int list) (n: int) =
  match List.find l ~f:(fun i -> i = n) with
  | Some(_) -> true
  | None -> false

let test_ngrams _ =
  let dist = Int_NGram.ngrams 3 [1; 4; 2; 6; 34] in
  let bl = Bag.to_list (Int_NGram.Token_list_map.find_exn dist [1; 4]) in
  assert_bool "wrong bag content" (find_int bl 2 && List.length bl = 1);
  let bl = Bag.to_list (Int_NGram.Token_list_map.find_exn dist [4; 2]) in
  assert_bool "wrong bag content" (find_int bl 6 && List.length bl = 1);
  let bl = Bag.to_list (Int_NGram.Token_list_map.find_exn dist [2; 6]) in
  assert_bool "wrong bag content" (find_int bl 34 && List.length bl = 1)
  
let test_sample_sequence _ =
  let dist = Int_NGram.ngrams 3 [1; 2; 3; 4; 4; 4; 2; 2; 3; 1] in
  assert_equal (Int_NGram.sample_sequence dist ~max_length:5 ~initial_ngram:[2; 3]) @@ [2; 3; 1];
  assert_equal (Int_NGram.sample_sequence dist ~max_length:5 ~initial_ngram:[2]) @@ [];
  assert_equal (Int_NGram.sample_sequence dist ~max_length:5 ~initial_ngram:[1; 1; 3]) @@ [1; 1 ;3]

let test_sanitize _ =
  assert_equal (sanitize "ABZ123() ") @@ Some("abz123");
  assert_equal (sanitize "() ") @@ None;
  assert_equal (sanitize "\tA  \nBZ123() ") @@ Some("abz123")

let check_str_sanitize sanitizer =
    Quickcheck.test String.quickcheck_generator
        ~f:(fun l -> OUnit2.assert_bool "uncleaned" (match sanitizer l with
                            | None -> true
                            | Some(s) -> not ((String.contains s '%')||(String.contains s '!')||(String.contains s '?'))
                              && String.for_all s ~f:(fun c -> (((Char.to_int c) - (Char.to_int '0') >= 0) && ((Char.to_int c) - (Char.to_int '0') <= 9)) || (((Char.to_int c) - (Char.to_int 'a') >= 0) && ((Char.to_int c) - (Char.to_int 'a') <= 25)))))

let test_sanitize_random _ =
  check_str_sanitize sanitize;
  check_str_sanitize sanitize;
  check_str_sanitize sanitize;
  check_str_sanitize sanitize

let test_build_map _ =
  let tl = [["hello"; "world"]; ["hello"; "world"]; ["janes"; "street"]] in
  let m = Lib.build_map tl in
  assert_equal (Freq_Map.find m ["hello"; "world"]) @@ Some(2);
  assert_equal (Freq_Map.find m ["janes"; "street"]) @@ Some(1)

let test_map_to_list _ =
  let tl = [["hello"; "world"]; ["hello"; "world"]; ["janes"; "street"]] in
  let m = Lib.build_map tl in
  Lib.map_to_list m;
  assert_bool "doesn't contain hello world or freq doesn't match" (List.exists !Lib.fl 
    ~f:(fun r -> Lib.compare_string_list r.ngram ["hello"; "world"] 0 = 0 && r.frequency=2));
  assert_bool "doesn't contain janes street or freq doesn't match" (List.exists !Lib.fl 
    ~f:(fun r -> Lib.compare_string_list r.ngram ["janes"; "street"] 0 = 0 && r.frequency=1))

let test_list_first_nth _ =
  assert_equal (Lib.list_first_nth [1;2;3] 0 2) @@ [1;2];
  assert_equal (Lib.list_first_nth [] 0 2) @@ [];
  assert_equal (Lib.list_first_nth [1;2;3] 0 5) @@ [1;2;3];
  assert_equal (Lib.list_first_nth [1;2;3;5;7;5;3;1] 0 5) @@ [1;2;3;5;7];
  assert_equal (Lib.list_first_nth [1;23] 0 (-1)) @@ []

let test_compare_string_list _ =
  assert_bool "unmatch result" (Lib.compare_string_list ["abcd"] ["acbd"] 0 < 0);
  assert_bool "unmatch result" (Lib.compare_string_list ["aaa"; "abcd"] ["aaa"; "acbd"] 0 < 0);
  assert_bool "unmatch result" (Lib.compare_string_list ["abcd"] ["abcd"] 0 = 0);
  assert_bool "unmatch result" (Lib.compare_string_list ["abcd"; "sdsdsgyj"] ["abcd"; "asdasd"] 0 > 0)

let test_parse_initial_words _ =
  Lib.n := 3;
  parse_initial_words "\tH...ello world??";
  assert_equal !Lib.initial_words @@ ["hello"; "world"];
  (* assert_raises (Failure "empty sanitized string") (fun () -> parse_initial_words "  \tH...ello world?? asdgjasb !!"); *)
  Lib.n := 5;
  assert_raises (Failure "number of initial words smaller than (n-1)") (fun () -> parse_initial_words "\tH...ello world?? asdgjasb")

let test_sanity_check _ =
  assert_raises (Failure "insufficient arguments") Lib.sanity_check;
  Lib.n_most_frequent := 1;
  Lib.sample_length := 1;
  assert_raises (Failure "cannot use most_frequent and sample_length at the same time") Lib.sanity_check

let test_sanitize_all _ =
  assert_equal (Lib.sanitize_all ["@Sad?!"; "  " ; "Hello!!"]) @@ ["sad"; "hello"]

let test_freq_fl _ =
  assert_equal (Lib.freq_list_to_yojson [{ngram = []; frequency = (-1)}] |> Lib.freq_list_of_yojson) @@ (Lib.freq_list_to_yojson [{ngram = []; frequency = (-1)}] |> Lib.freq_list_of_yojson);
  assert_equal (Lib.freq_to_yojson {ngram = []; frequency = (-1)} |> Lib.freq_of_yojson) @@ (Lib.freq_to_yojson {ngram = []; frequency = (-1)} |> Lib.freq_of_yojson)
let section1_tests =
  "Section 1" >: test_list [
    "Chunks" >:: test_chunks;
    "SplitLast" >:: test_split_last;
    "Compare" >:: test_compare;
    "Sexp_invar" >:: test_sexp_invariants;
    "Sample" >:: test_sample;
    "NGram" >:: test_ngrams;
    "SampleSequence" >:: test_sample_sequence;
    "Sanitize" >:: test_sanitize;
  ]

let section2_tests =
  "Section2 " >: test_list [
    "Sanitize random" >:: test_sanitize_random;
    "Build Map" >:: test_build_map;
    "map to list" >:: test_map_to_list;
    "list_first_nth" >:: test_list_first_nth;
    "compare string list" >:: test_compare_string_list;
    "sanity check" >:: test_sanity_check;
    "parse init words" >:: test_parse_initial_words;
    "sanity check" >:: test_sanity_check;
    "sanitize all" >:: test_sanitize_all;
    "freq list" >:: test_freq_fl;
  ]

let series =
  "Assignment2 Tests" >::: [
    section1_tests;
    section2_tests;
  ]

let () = 
  run_test_tt_main series

