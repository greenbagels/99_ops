let compress l =
  let rec aux l acc =
    match l with
    | [] -> List.rev acc
    | x :: xs ->
        if acc = [] || x <> List.hd acc then
          aux xs (x :: acc)
        else
          aux xs acc
  in aux l []

let%test _ =
  compress
    ["a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e"] =
    ["a"; "b"; "c"; "a"; "d"; "e"]

