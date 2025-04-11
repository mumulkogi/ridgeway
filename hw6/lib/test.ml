(* ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: *)

let%test _ = Fstore.add 
  "add" 
  ["a"; "b"] 
  (Add ((Id "a"), (Id "b"))) 
  [] =

  [
    ("add", (["a"; "b"], (Add ((Id "a"), (Id "b")))))
  ]

let%test _ = Fstore.add 
  "sub" 
  ["a"; "b"] 
  (Sub ((Id "a"), (Id "b")))
  [
    ("add", (["a"; "b"], (Add ((Id "a"), (Id "b")))))
  ] =

  [
    ("add", (["a"; "b"], (Add ((Id "a"), (Id "b")))));
    ("sub", (["a"; "b"], (Sub ((Id "a"), (Id "b")))))
  ]

let%test _ = Fstore.add 
  "sub" 
  ["c"; "d"] 
  (Sub ((Id "c"), (Id "d")))
  [
    ("add", (["a"; "b"], (Add ((Id "a"), (Id "b")))));
    ("sub", (["a"; "b"], (Sub ((Id "a"), (Id "b")))))
  ] =
  
  [
    ("add", (["a"; "b"], (Add ((Id "a"), (Id "b")))));
    ("sub", (["c"; "d"], (Sub ((Id "c"), (Id "d")))))
  ]

(* ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: *)

let%test _ = Fstore.find 
  "add" 
  [
    ("add", (["a"; "b"], (Add ((Id "a"), (Id "b")))))
  ] = 

  (["a"; "b"], (Add ((Id "a"), (Id "b"))))

let%test _ = Fstore.find 
  "add"
  [
    ("add", (["a"; "b"], (Add ((Id "a"), (Id "b")))));
    ("sub", (["a"; "b"], (Sub ((Id "a"), (Id "b")))))
  ] = 
  
  (["a"; "b"], (Add ((Id "a"), (Id "b"))))

let%test _ = Fstore.find 
  "sub"
  [
    ("add", (["a"; "b"], (Add ((Id "a"), (Id "b")))));
    ("sub", (["c"; "d"], (Sub ((Id "c"), (Id "d")))))
  ] = 
  
  (["c"; "d"], (Sub ((Id "c"), (Id "d"))))

let%test _ = try
  let _ = Fstore.find "add" [] in
    false
  with
    | Failure msg -> msg = "Undefined function: add"
    | _ -> false

(* ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: *)

let%test _ = Hw6.interp_expr 
  [("add", (["a"; "b"], (Add ((Id "a"), (Id "b")))))] 
  [] 
  (Call ("add", [Num 2; Num 3]))
  = 
  NumV 5

let%test _ = Hw6.interp_expr 
  [("sub", (["a"; "b"], (Sub ((Id "a"), (Id "b")))))] 
  [] 
  (Call ("sub", [Num 2; Num 3]))
  = 
  NumV (-1)

let%test _ = Hw6.interp_expr 
  [
    ("add", (["a"; "b"], (Add ((Id "a"), (Id "b")))));
    ("sub", (["c"; "d"], (Sub ((Id "c"), (Id "d")))))
  ] 
  [("owo", NumV 939)] 
  (Call ("sub", [(Id "owo"); Num 9]))
  = 
  NumV 930

let%test _ = Hw6.interp_expr 
  [
    ("add", (["a"; "b"], (Add ((Id "a"), (Id "b")))));
    ("sub", (["c"; "d"], (Sub ((Id "c"), (Id "d")))))
  ] 
  [("owo", NumV 10); ("omo", NumV 15)] 
  (Call ("add", [(Id "owo"); (Id "omo")]))
  = 
  NumV 25

(* ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: *)

let%test _ = Hw6.interp_def 
  (Ast.FunDef ("add", ["a"; "b"], (Add ((Id "a"), (Id "b"))))) 
  [] 
  = 
  [("add", (["a"; "b"], (Add ((Id "a"), (Id "b")))))]

let%test _ = Hw6.interp_def 
  (Ast.FunDef ("sub", ["a"; "b"], (Sub ((Id "a"), (Id "b"))))) 
  [] 
  = 
  [("sub", (["a"; "b"], (Sub ((Id "a"), (Id "b")))))]

let%test _ = Hw6.interp_def 
  (Ast.FunDef ("add", ["c"; "d"], (Add ((Id "c"), (Id "d"))))) 
  [("add", (["a"; "b"], (Add ((Id "a"), (Id "b")))))] 
  = 
  [("add", (["c"; "d"], (Add ((Id "c"), (Id "d")))))]

let%test _ = Hw6.interp_def 
  (Ast.FunDef ("sub", ["c"; "d"], (Sub ((Id "c"), (Id "d"))))) 
  [("add", (["a"; "b"], (Add ((Id "a"), (Id "b")))))] 
  = 
  [
    ("add", (["a"; "b"], (Add ((Id "a"), (Id "b")))));
    ("sub", (["c"; "d"], (Sub ((Id "c"), (Id "d")))))
  ]

(* ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: *)

let%test _ = Hw6.interp_prog 
  (Ast.Prog ([], (Ast.Add (Num 3, Num 1)))) 
  = NumV 4

let%test _ = Hw6.interp_prog 
  (Ast.Prog ([], (Ast.Sub (Num 3, Num 1)))) 
  = NumV 2

(* `let x = 3 in add(x, 2)`*)
let%test _ = Hw6.interp_prog 
  (Ast.Prog (
    [(Ast.FunDef ("add", ["a"; "b"], (Add ((Id "a"), (Id "b")))))], 
    (Ast.LetIn ("x", Num 3, Call ("add", [(Id "x"); Num 2]))))
  )
  = NumV 5

(* `let x = 3 + 1 in add(x, 2)`*)
let%test _ = Hw6.interp_prog 
  (Ast.Prog (
    [(Ast.FunDef ("add", ["a"; "b"], (Add ((Id "a"), (Id "b")))))], 
    (
      Ast.LetIn (
        "x", 
        (Ast.Add (Num 3, Num 1)), 
        Call ("add", [(Id "x"); Num 2])
      )
    )
  ))
  = NumV 6

(* ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: *)