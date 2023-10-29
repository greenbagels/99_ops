
type 'a rle =
  | One of 'a
  | Many of int * 'a

let encode l =
  let tup_to_rle (n, x) = if n = 1 then One x else Many (n, x) in
  let rec aux l cur acc =
    match l with
    | [] -> (
        match cur with
        | None -> acc
        | Some cur -> List.rev (tup_to_rle cur :: acc))
    | x :: xs -> (
        match cur with
        | None -> aux xs (Some (1, x)) acc
        | Some (n, y) ->
            if x = y then aux xs (Some (n + 1, x)) acc
            else aux xs (Some (1, x)) (tup_to_rle (n, y) :: acc))
  in
  aux l None []

let%test _ =
  encode ["a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e"] =
    [Many (4, "a"); One "b"; Many (2, "c"); Many (2, "a"); One "d";  Many (4, "e")]

