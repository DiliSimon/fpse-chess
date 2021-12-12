open Board;;
open Core;;

module type Evaluator =
sig
    type value
    val min_value : value
    val max_value : value
    val compare : value -> value -> int
    val eval : board -> player -> value
end

module type Bot =
sig
    type move
    (* None if no valid move can be made *)
    val get_best_move : board -> player -> move option
end

module BaseEval : Evaluator = 
struct
    type value = Finite of int | Inf | NInf

    let min_value = NInf
    let max_value = Inf

    let compare a b =
    match (a, b) with
      | (Inf, Inf) | (NInf, NInf) -> 0
      | (Inf, _) | (_, NInf) -> 1
      | (NInf, _) | (_, Inf) -> (-1)
      | (Finite a, Finite b) -> compare a b

    let get_piece_value (piece: chess) : int =
        match piece with
        | Pawn(owner) -> if equal_player Black owner then 100 else -100
        | Knight(owner) -> if equal_player Black owner then 320 else -320
        | Bishop(owner) -> if equal_player Black owner then 325 else -325
        | Rook(owner, _) -> if equal_player Black owner then 500 else -500
        | Queen(owner) -> if equal_player Black owner then 975 else -975
        | King(owner, _ ) -> if equal_player Black owner then 30000 else -30000

    let eval (board: board) (curr_player: player) : value =
        let score = List.fold board ~init:(0)
                    ~f:(fun accum1 col -> accum1 + (List.fold col ~init:(0)
                        ~f:(fun accum2 pos -> accum2 +
                            (match pos with | Empty -> (0) | Occupied(piece) -> get_piece_value piece))))
        in
        let curr_check = (if is_check board curr_player then (match curr_player with | Black -> 100 | White -> -100) else 0) in
        let curr_checkmate = (if is_checkmate board curr_player then (match curr_player with | Black -> 100000 | White -> -100000) else 0) in
        Finite(score + curr_check + curr_checkmate)

end

module MinimaxBot (Eval: Evaluator) : Bot =
struct
    type move = (int * int) * (int * int)
    
    let rec eval_board (board: board) ~(curr_player: player) ~(curr_depth: int) ~(limit: int) : (Eval.value) =
        if is_checkmate board Black then Eval.min_value
        else if is_checkmate board White then Eval.max_value
        else if curr_depth >= limit then (Eval.eval board curr_player)
        else
        let all_moves: ((int * int) * (int * int)) list = get_all_possible_moves board curr_player in
        let next_boards = List.map all_moves ~f:(fun (f, t) -> move board curr_player f t) in
        match curr_player with
        (* Black is maximizing player *)
        | Black  -> let next_board_scores = List.map next_boards 
                    ~f:(fun (b, _) -> eval_board b 
                        ~curr_player:White ~curr_depth:(curr_depth+1) ~limit:limit)
                    in 
                    List.fold next_board_scores 
                    ~init:(Eval.min_value) 
                    ~f:(fun curr_max curr_value -> if Eval.compare curr_value curr_max > 0 then curr_value else curr_max)
        (* White is minimizing player *)
        | White  -> let next_board_scores = List.map next_boards 
                    ~f:(fun (b, _) -> eval_board b 
                        ~curr_player:Black ~curr_depth:(curr_depth+1) ~limit:limit)
                    in 
                    List.fold next_board_scores 
                    ~init:(Eval.max_value) 
                    ~f:(fun curr_min curr_value -> if Eval.compare curr_value curr_min < 0 then curr_value else curr_min)

    let get_best_move (board: board) (curr_player: player) : move option =
        (*
        1. get all possible moves
        2. perform each move
        3. get score for each board after move
        4. pick move with best score
        *)
        let all_moves: ((int * int) * (int * int)) list = get_all_possible_moves board curr_player in
        let next_boards = List.map all_moves ~f:(fun (f, t) -> move board curr_player f t) in
        if List.length next_boards = 0 then None
        else
        match curr_player with
        | Black -> let next_board_scores = List.map next_boards 
                    ~f:(fun (b, _) -> eval_board b ~curr_player:White ~curr_depth:(0) ~limit:3)
                    in 
                    let (best_idx, _) = 
                        List.foldi next_board_scores 
                        ~init:((-1), Eval.min_value) 
                        ~f:(fun idx (best_idx, curr_max) curr_value -> 
                            if Eval.compare curr_value curr_max > 0 
                            then (idx, curr_value) 
                            else (best_idx, curr_max))
                    in Some(List.nth_exn all_moves best_idx)
        | White -> let next_board_scores = List.map next_boards 
                    ~f:(fun (b, _) -> eval_board b ~curr_player:Black ~curr_depth:(0) ~limit:3)
                    in 
                    let (best_idx, _) = 
                        List.foldi next_board_scores 
                        ~init:((-1), Eval.max_value) 
                        ~f:(fun idx (best_idx, curr_min) curr_value -> 
                            if Eval.compare curr_value curr_min < 0 
                            then (idx, curr_value) 
                            else (best_idx, curr_min))
                    in Some(List.nth_exn all_moves best_idx)

end