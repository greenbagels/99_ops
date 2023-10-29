let rec last l =
  match l with
  | [] -> None
  | [x] -> Some x
  | _ :: xs -> last xs

let%test _ =
  last ["a"; "b"; "c"; "d"] = Some "d"

