(* ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: *)

module HwX = Hw10

(* [`Store.add`] ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: *)

let%test "store_add_0" = Store.add 
  "owo" (ClosureV ("x", (Add (Id "x", Num 1)), [])) [] 
  = [("owo", (ClosureV ("x", (Add (Id "x", Num 1)), [])))]

let%test "store_add_1" = Store.add
  "owo" (NumV 939) [] 
  = [("owo", NumV 939)]

let%test "store_add_2" = Store.add
  "owo" (NumV 949) [("uwu", NumV 939)]
  = [("owo", NumV 949); ("uwu", NumV 939)]

let%test "store_add_3" = Store.add
  "owo" (NumV 959) [("uwu", NumV 929); ("owo", NumV 939)] = 
  [("owo", NumV 959); ("uwu", NumV 929);]

let%test "store_add_4" = Store.add
  "owo" (NumV 969) [("uwu", NumV 929); ("owo", NumV 939); ("uwo", NumV 949)] = 
  [("owo", NumV 969); ("uwu", NumV 929); ("uwo", NumV 949)]

(* [`Store.find`] :::::::::::::::::::::::::::::::::::::::::::::::::::::::::: *)

let%test "store_find_0" = Store.find 
  "owo" [("owo", (ClosureV ("x", (Add (Id "x", Num 1)), [])))] 
  = ClosureV ("x", (Add (Id "x", Num 1)), [])

let%test "store_find_1" = Store.find 
  "omo" [("owo", NumV 939); ("omo", NumV 979)] 
  = NumV 979

let%test "store_find_2" = Store.find 
  "omu" [("owo", NumV 939); ("omu", NumV 959); ("omo", NumV 979)] 
  = NumV 959

let%test "store_find_3" = 
  try
    let _ = Store.find "omo" [] in
    false
  with
    | Failure msg -> msg = "Free identifier: omo"
    | _ -> false

(* [`Store.remove`] :::::::::::::::::::::::::::::::::::::::::::::::::::::::: *)

let%test "store_remove_0" = Store.remove
  "" []
  = []

(* [`HwX.interp`] :::::::::::::::::::::::::::::::::::::::::::::::::::::::::: *)

let%test "HwX_interp_num_0" = HwX.interp 
  (ParserMain.parse "1") []
  = NumV 1

(* ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: *)

let%test "HwX_interp_bool_0" = HwX.interp 
  (ParserMain.parse "true") []
  = BoolV true

(* ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: *)

let%test "HwX_interp_add_0" = HwX.interp 
  (ParserMain.parse "1 + 2") []
  = NumV 3

let%test "HwX_interp_add_1" = 
  try
    let _ = HwX.interp (ParserMain.parse "1 + true") [] in 
    false
  with
    | Failure msg -> msg = "Not a number: 1 + true" 
    | _ -> false

(* ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: *)

let%test "HwX_interp_sub_0" = HwX.interp 
  (ParserMain.parse "1 - 2") []
  = NumV (-1) 

let%test "HwX_interp_sub_1" = 
  try
    let _ = HwX.interp (ParserMain.parse "1 - true") [] in 
    false
  with
    | Failure msg -> msg = "Not a number: 1 - true" 
    | _ -> false

(* ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: *)

let%test "HwX_interp_letin_0" = HwX.interp 
  (ParserMain.parse "let oxo = 11 in oxo + 22") [] 
  = NumV 33

let%test "HwX_interp_letin_1" = HwX.interp
  (ParserMain.parse "let x = 2 in let y = 3 in let z = x + y in z") [] 
  = NumV 5

let%test "HwX_interp_letin_2" = HwX.interp
  (ParserMain.parse "let x = 5 in let x = x + 1 in let x = x + 3 in x") [] 
  = NumV 9

(* ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: *)

let%test "HwX_interp_rletin_0" = HwX.interp
  (ParserMain.parse 
    "let rec f = (fun x -> if x < 1 then 0 else x + (f (x - 1))) in f 10"
  ) [] 
  = NumV 55

let%test "HwX_interp_rletin_1" =
  try
    let _ = HwX.interp (ParserMain.parse "let rec f = 1 in f 1") [] in 
    false
  with
    | Failure msg -> msg = "Not a function: 1" 
    | _ -> false

(* ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: *)

let%test "HwX_interp_app_0" = HwX.interp
  (ParserMain.parse "(fun x y -> x + y) 1 2") [] 
  = NumV 3

let%test "HwX_interp_app_1" =
  try
    let _ = HwX.interp (ParserMain.parse "1 1") [] in 
    false
  with
    | Failure msg -> msg = "Not a function: 1" 
    | _ -> false

(* ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: *)

let%test "HwX_interp_lambda_0" = HwX.interp
  (ParserMain.parse "let f = (fun x y -> x + y) in f 1 2") [] 
  = NumV 3

let%test "HwX_interp_lambda_1" = HwX.interp
  (ParserMain.parse "let f = (fun x y -> if x < y then y else x) in f 1 2") [] 
  = NumV 2

let%test "HwX_interp_lambda_2" = HwX.interp
  (ParserMain.parse 
    "let f = (fun x y -> y + y) (1 + true) in f 3"
  ) [] 
  = NumV 6

(* ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: *)

let%test "HwX_interp_cond_0" = HwX.interp
  (ParserMain.parse "if true then 1 else 2") [] 
  = NumV 1

let%test "HwX_interp_cond_1" = HwX.interp
  (ParserMain.parse "if false then 1 else 2") [] 
  = NumV 2

let%test "HwX_interp_cond_2" =
  try
    let _ = HwX.interp (ParserMain.parse "if 100 then 1 else 2") [] in 
    false
  with
    | Failure msg -> msg = "Not a bool: 100" 
    | _ -> false

(* ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: *)

let%test "HwX_interp_lessthan_0" = HwX.interp
  (ParserMain.parse "1 < 2") [] 
  = BoolV true

let%test "HwX_interp_lessthan_1" = HwX.interp
  (ParserMain.parse "3 < 2") [] 
  = BoolV false

let%test "HwX_interp_lessthan_2" =
  try
    let _ = HwX.interp (ParserMain.parse "true < false") [] in 
    false
  with
    | Failure msg -> msg = "Not a number: true < false" 
    | _ -> false

(* ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: *)