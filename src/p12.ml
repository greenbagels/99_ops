
type 'a rle =
  | One of 'a
  | Many of int * 'a

let decode l =
  let rec rep l n x =
    match n with
    | 0 -> l
    | n -> rep (x :: l) (n-1) x in
  let rec aux l acc =
    match l with
    | [] -> List.rev acc
    | (One x) :: xs ->
        aux xs (x :: acc)
    | (Many (n, x)) :: xs ->
        aux xs (rep acc n x)
  in aux l []

let%test _ =
  decode [Many (4, "a"); One "b"; Many (2, "c"); Many (2, "a"); One "d"; Many (4, "e")] =
    ["a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e"]

