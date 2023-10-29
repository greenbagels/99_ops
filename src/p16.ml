let drop l n =
  let rec aux l n k acc =
    match l with
    | [] -> List.rev acc
    | x :: xs ->
        if k = 1 then
          aux xs n n acc
        else
          aux xs n (k-1) (x :: acc)
  in aux l n n []

let%test _ =
  drop ["a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"; "i"; "j"] 3 =
    ["a"; "b"; "d"; "e"; "g"; "h"; "j"]
