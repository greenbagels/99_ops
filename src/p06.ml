let is_palindrome l =
  l = List.rev l

let%test _ =
  is_palindrome ["x"; "a"; "m"; "a"; "x"] = true

let%test _ =
  is_palindrome ["a"; "b";] = false
