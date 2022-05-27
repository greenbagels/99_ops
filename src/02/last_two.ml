
let rec last_two = function
    | [] -> None
    | [x] -> None
    | [x; y] -> Some (x, y)
    | x :: tail -> last_two tail

(* The solution given is
 *
 * let rec last_two = function
 *     | [] | [_] -> None
 *     | [x; y] -> Some (x, y)
 *     | _ :: t -> last_two t
 *
 * Our original attempt was
 *
 * let rec last_two = function
 *     | [] | [x] -> None
 *     | [x; y] -> Some (x, y)
 *     | x :: tail -> last_two tail
 *
 * We obtained the error:
 *     Error: Variable x must occur on both sides of this | pattern
 *
 *)
