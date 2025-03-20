(* ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: *)

let%test _ = Hw2.fib 4 = 3
let%test _ = Hw2.fib (-3) = -1

let%test _ = Hw2.fib_opt 40 = 102334155
let%test _ = Hw2.fib_opt (-3) = -1

let%test _ = try let _ = Hw2.last [] in false with Failure msg -> msg = "The given list is empty"
let%test _ = Hw2.last [3;2;1] = 1

let%test _ = try let _ = Hw2.second_last [1] in false with Failure msg -> msg = "The given list has a sole element"
let%test _ = Hw2.second_last [1;2;3] = 2

let%test _ = Hw2.len [] = 0
let%test _ = Hw2.len [1;2;3;4] = 4

let%test _ = Hw2.rev [] = []
let%test _ = Hw2.rev [1;2;3;4] = [4;3;2;1]

let%test _ = Hw2.is_palindrome [1;2;1] = true
let%test _ = Hw2.is_palindrome [1;2;3] = false
let%test _ = Hw2.is_palindrome [1;2;2;1] = true
let%test _ = Hw2.is_palindrome [1;2;3;1] = false
let%test _ = Hw2.is_palindrome [1;2;3;2;1] = true
let%test _ = Hw2.is_palindrome [1;2;3;4;1] = false

let%test _ = Hw2.compress "" = []
let%test _ = Hw2.compress "Z" = [(1, 'Z')]
let%test _ = Hw2.compress "AA" = [(2, 'A')]
let%test _ = Hw2.compress "AB" = [(1, 'A'); (1, 'B')]
let%test _ = Hw2.compress "AAB" = [(2, 'A'); (1, 'B')]
let%test _ = Hw2.compress "ABB" = [(1, 'A'); (2, 'B')]
let%test _ = Hw2.compress "GGGHH" = [(3, 'G'); (2, 'H')]
let%test _ = Hw2.compress "abba" = [(1, 'a'); (2, 'b'); (1, 'a')]

let%test _ = Hw2.sort (fun i j -> i < j) [] = []
let%test _ = Hw2.sort (fun i j -> i > j) [1] = [1]
let%test _ = Hw2.sort (fun i j -> i > j) [1;2;3;4] = [4;3;2;1]
let%test _ = Hw2.sort (fun i j -> i < j) [5;4;3;2;1] = [1;2;3;4;5]
let%test _ = Hw2.sort (fun i j -> i < j) [3;5;3;2;1] = [1;2;3;3;5]

let%test _ = Hw2.traverse (Hw2.Nil) = []
let%test _ = Hw2.traverse (Hw2.N(1, Hw2.N(2, Hw2.Nil, Hw2.Nil), Hw2.N(3, Hw2.Nil, Hw2.Nil))) = [2;3;1]

(* ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: *)