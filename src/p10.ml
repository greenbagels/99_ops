let encode l =
  List.map (fun sublist -> (List.length sublist, List.hd sublist)) (P09.pack l)

let%test _ =
  encode ["a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e"] =
    [(4, "a"); (1, "b"); (2, "c"); (2, "a"); (1, "d"); (4, "e")]
