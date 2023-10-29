let rec nth l n =
  match n with
  | 0 -> List.hd l
  | _ -> nth (List.tl l) (n-1)

let%test _ =
  let l = ["a"; "b"; "c"; "d"; "e"] in
  nth l 2 = List.nth l 2
