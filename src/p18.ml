let slice l n k =
  let rec aux l j acc =
    match l with
    | [] -> List.rev acc
    | x :: xs ->
        if j < n then
          aux xs (j+1) acc
        else if j > k then
          List.rev acc
        else
          aux xs (j+1) (x :: acc)
  in aux l 0 []

let%test _ =
  slice ["a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"; "i"; "j"] 2 6 =
    ["c"; "d"; "e"; "f"; "g"]
