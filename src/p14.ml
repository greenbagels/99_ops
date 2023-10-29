let duplicate l =
  let rec aux l acc =
    match l with
    | [] -> List.rev acc
    | x :: xs -> aux xs (x :: x :: acc)
  in aux l []

let%test _ =
  duplicate ["a"; "b"; "c"; "c"; "d"] =
    ["a"; "a"; "b"; "b"; "c"; "c"; "c"; "c"; "d"; "d"]
