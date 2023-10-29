let length l =
  let rec aux l n =
    match l with
    | [] -> n
    | _ :: xs -> aux xs (n+1)
  in aux l 0

let%test _ =
  let l = ["a"; "b"; "c"] in
  length l = List.length l
