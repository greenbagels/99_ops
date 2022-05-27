
let rec nth lst = function
    | 0 -> List.hd lst
    | k -> nth (List.tl lst) (k-1)

(* The given solution is:
 *
 * let rec at k = function
 *     | [] -> None
 *     | h :: t -> if k = 1 then Some h else at (k-1) t
 *
 * This new function seems to differ from ours in that
 * it handles failure using option types rather than
 * exceptions, and that it uses the list rather than the
 * element index as the lambda function's parameter.
 *)
