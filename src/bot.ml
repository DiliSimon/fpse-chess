open Board;;
open Core;;

module type Evaluator =
sig
    type value
    val lower_bound : value
    val upper_bound : value
    val eval : board -> player -> value
end

module BaseEval : Evaluator = struct
    type value = Finite of int | Inf | NInf
    let lower_bound = NInf
    let upper_bound = Inf

    let get_piece_value (curr_player: player) (piece: chess) : int =
        match piece with
        | Pawn(owner) -> if equal_player curr_player owner then 100 else -100
        | Knight(owner) -> if equal_player curr_player owner then 320 else -320
        | Bishop(owner) -> if equal_player curr_player owner then 325 else -325
        | Rook(owner, _) -> if equal_player curr_player owner then 500 else -500
        | Queen(owner) -> if equal_player curr_player owner then 975 else -975
        | King(owner, _ ) -> if equal_player curr_player owner then 30000 else -30000

    let eval (board: board) (curr_player: player) : value =
        let score = List.fold board ~init:(0)
                    ~f:(fun accum1 col -> accum1 + (List.fold col ~init:(0)
                        ~f:(fun accum2 pos -> accum2 +
                            (match pos with | Empty -> (0) | Occupied(piece) -> get_piece_value curr_player piece))))
        in
        let curr_check = (if is_check board (opponent_of curr_player) then -100 else 0) in
        let curr_checkmate = (if is_checkmate board (opponent_of curr_player) then -10000 else 0) in
        Finite(score + curr_check + curr_checkmate)

end