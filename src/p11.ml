
type 'a rle =
  | One of 'a
  | Many of int * 'a

let encode l =
  P10.encode l |> List.map (fun (n, x) ->
    if n = 1 then One x else Many (n, x))

let%test _ =
  encode ["a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e"] =
    [Many (4, "a"); One "b"; Many (2, "c"); Many (2, "a"); One "d";  Many (4, "e")]
