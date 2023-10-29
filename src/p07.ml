(* OCaml doesn't natively support a list type whose elements can be different types  *)
(* So we define our own here, as in the 99 OCaml problems page. The type variable 'a *)
(* is the same as the 'a used in the list type.*)

type 'a node =
  | One of 'a
  | Many of 'a node list

let flatten l =
  (* core idea: we have an accumulator holding the list currently built, and
   * we use rem as a stack to hold a list of the next objects to be handled *)
  let rec aux l acc rem =
    match l with
    (* Basically, l represents the most recent unparsed node found during the
     * last execution of aux (at the start, it holds l). We parse it by peeling
     * away the head of the list (if it has type Many) and appending the tail
     * to rem, or appending the element to acc (if it has type One) and grabbing
     * the next unparsed node from our stack. *)
    | Many [] ->
        (match rem with
        | [] ->
            acc
        | y :: ys ->
            aux y acc ys)
    | Many (x :: xs) ->
        aux x acc (Many xs :: rem)
    | One x ->
        (match rem with
        | [] ->
            x :: acc
        | y :: ys ->
            aux y (x :: acc) ys)
  in aux l [] [] |> List.rev

let%test _ =
  flatten (Many [One "a"; Many [One "b"; Many [One "c"; One "d"]; One "e"]]) = ["a"; "b"; "c"; "d"; "e"]

