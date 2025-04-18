(* ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: *)

let%test "store_add_0" = Store.add 
  "owo" 
  (ClosureV ("x", (Add (Id "x", Num 1)), [])) 
  [] 
  = [("owo", (ClosureV ("x", (Add (Id "x", Num 1)), [])))]

(* ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: *)

let%test "store_find_0" = Store.find 
  "owo" 
  [("owo", (ClosureV ("x", (Add (Id "x", Num 1)), [])))] 
  = ClosureV ("x", (Add (Id "x", Num 1)), [])

(* ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: *)

let%test "hw7_interp_0" =
  try
    let _ = Hw7.interp (ParserMain.parse "1 1") [] in false
  with
  | Failure msg -> msg = "Not a function: 1" 
  | _ -> false

let%test "hw7_interp_1" = Hw7.interp 
  (ParserMain.parse "1") []
  = NumV 1

let%test "hw7_interp_2" = Hw7.interp 
  (ParserMain.parse "1 - 2") []
  = NumV (-1)

let%test "hw7_interp_3" = Hw7.interp 
  (ParserMain.parse "let oxo = 11 in oxo + 22") [] 
  = NumV 33

let%test "hw7_interp_4" = Hw7.interp
  (ParserMain.parse "let x = 2 in let y = 3 in let z = x + y in z") [] 
  = NumV 5

let%test "hw7_interp_5" = Hw7.interp
  (ParserMain.parse "let x = 5 in let x = x + 1 in let x = x + 3 in x") [] 
  = NumV 9

(* ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: *)
