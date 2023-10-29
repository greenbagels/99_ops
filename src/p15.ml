let replicate l n =
  let rec ncons l x n =
    match n with | 0 -> l | n -> ncons (x :: l) x (n-1)
  in
  let rec aux l n acc =
    match l with
    | [] -> List.rev acc
    | x :: xs -> aux xs n (ncons acc x n)
  in aux l n []

let%test _ =
  replicate ["a"; "b"; "c"] 3 = ["a"; "a"; "a"; "b"; "b"; "b"; "c"; "c"; "c"]
