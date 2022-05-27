
let rec last lst =
    match lst with
    | [] -> None
    | [fin] -> Some fin
    | front :: tail -> last tail

(*
 * Note: this function apparently is equivalent to:
 *
 * let rec last = function
 *     | [] -> None
 *     | [x] -> Some x
 *     | front :: tail -> last tail
 *
 * So I guess the function syntactic sugar is preferable.
 *)
