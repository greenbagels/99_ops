let rev l =
  let rec aux l rev_l =
    match l with
    | [] -> rev_l
    | x :: xs -> aux xs (x :: rev_l)
  in aux l []

let%test _ =
  let l = ["a"; "b"; "c"] in
  rev l = List.rev l
