let rec last_two l =
  match l with
  | [] | [_] -> None
  | [x; y] -> Some (x, y)
  | _ :: xs -> last_two xs

let%test _ =
  last_two ["a"; "b"; "c"; "d"] = Some ("c", "d")
