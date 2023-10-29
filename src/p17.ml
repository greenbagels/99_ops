let split l n =
  let rec aux l n k acc =
    match l with
    | [] -> (List.rev acc, [])
    | x :: xs -> (
      match k with
      | 0 -> (List.rev acc, l)
      | _ ->
          aux xs n (k-1) (x :: acc)
      )
  in aux l n n []

let%test _ =
  split ["a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"; "i"; "j"] 3 =
    (["a"; "b"; "c"], ["d"; "e"; "f"; "g"; "h"; "i"; "j"])

