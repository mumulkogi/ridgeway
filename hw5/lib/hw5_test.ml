(* ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: *)

let%test _ = Store.add 
  "owo" (NumV 939) [] = 
  [("owo", NumV 939)]

let%test _ = Store.add 
  "owo" (NumV 949) [("owo", NumV 939)] = 
  [("owo", NumV 949)]

let%test _ = Store.add 
  "owo" (NumV 959) [("uwu", NumV 929); ("owo", NumV 939)] = 
  [("uwu", NumV 929); ("owo", NumV 959)]

let%test _ = Store.add 
  "owo" (NumV 969) [("uwu", NumV 929); ("owo", NumV 939); ("uwo", NumV 949)] = 
  [("uwu", NumV 929); ("owo", NumV 969); ("uwo", NumV 949)]

(* ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: *)

let%test _ = Store.find 
  "owo" [("owo", NumV 939)] = 
  NumV 939

let%test _ = Store.find 
  "omo" [("owo", NumV 939); ("omo", NumV 979)] 
  = NumV 979

let%test _ = Store.find 
  "omu" [("owo", NumV 939); ("omu", NumV 959); ("omo", NumV 979)] 
  = NumV 959

let%test _ = 
  try
    let _ = Store.find "omo" [] in
    false
  with
  | Failure msg -> msg = "Free identifier: omo"
  | _ -> false

(* ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: *)

let%test _ = Hw5.interp 
  (ParserMain.parse "1") [] = 
  NumV 1

let%test _ = Hw5.interp 
  (ParserMain.parse "1 + 2") [] = 
  NumV 3

let%test _ = Hw5.interp 
  (ParserMain.parse "3 - 4") [] 
  = NumV (-1)

let%test _ = Hw5.interp 
  (ParserMain.parse "let oxo = 11 in oxo + 22") [] 
  = NumV 33

let%test _ = Hw5.interp 
  (ParserMain.parse "let x = 2 in let y = 3 in let z = x + y in z") [] 
  = NumV 5

let%test _ = Hw5.interp 
  (ParserMain.parse "let x = 5 in let x = x + 1 in let x = x + 3 in x") [] 
  = NumV 9

(* ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: *)