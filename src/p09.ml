let pack l =
  let rec aux l cur acc =
    match l with
    | [] ->
        List.rev (cur :: acc)
    | x :: xs ->
        if cur = [] || x = List.hd cur then
          aux xs (x :: cur) acc
        else
          aux xs [x] (cur :: acc)
  in aux l [] []

let%test _ =
  pack ["a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e"] =
  [["a"; "a"; "a"; "a"]; ["b"]; ["c"; "c"]; ["a"; "a"]; ["d"]; ["e"; "e"; "e"; "e"]]

