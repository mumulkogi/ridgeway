(* lex *)
let%test _ = Hw3.lex "1320" = NUMBER "1320"

let%test _ = Hw3.lex "let" = KW_LET
let%test _ = Hw3.lex "in" = KW_IN

let%test _ = Hw3.lex "lets" = IDENT "lets"
let%test _ = Hw3.lex "lETS" = IDENT "lETS"
let%test _ = Hw3.lex "tt" = IDENT "tt"

let%test _ = Hw3.lex "=" = OP_EQ
let%test _ = Hw3.lex "+" = OP_PLUS
let%test _ = Hw3.lex "-" = OP_MINUS

let%test _ = 
  try
    let _ = Hw3.lex " " in
    false
  with
  | Failure msg -> msg = "Lexing error:  "
  | _ -> false

let%test _ = 
  try
    let _ = Hw3.lex "0" in
    false
  with
  | Failure msg -> msg = "Lexing error: 0"
  | _ -> false

let%test _ = 
  try
    let _ = Hw3.lex "Answer" in
    false
  with
  | Failure msg -> msg = "Lexing error: Answer"
  | _ -> false

(* parse *)

let%test _ = Hw3.generate_tokens ["let"; "x"; "="; "1"; "in"] = [KW_LET; IDENT "x"; OP_EQ; NUMBER "1"; KW_IN]

let%test _ = Hw3.parse "x" = Id "x"
let%test _ = Hw3.parse "4" = Num "4"

let%test _ = Hw3.parse "x + 4" = Plus (Id "x", Num "4")

let%test _ = Hw3.parse "4 - x" = Minus (Num "4", Id "x")
let%test _ = Hw3.parse "4 + x - 1" = Plus (Num "4", Minus (Id "x", Num "1"))
let%test _ = Hw3.parse "x + y - 4" = Plus (Id "x", Minus (Id "y", Num "4"))

let%test _ = Hw3.parse "let x = 1 in y" = LetIn ("x", Num "1", Id "y")
let%test _ = Hw3.parse "let x = 1 in 4 + x" = LetIn ("x", Num "1", Plus (Num "4", Id "x"))
let%test _ = Hw3.parse "let t = 1 - 1 in tt" = LetIn ("t", Minus (Num "1", Num "1"), Id "tt")
let%test _ = Hw3.parse "let t = 1 + 2 - 3 in tt" = LetIn ("t", Plus (Num "1", Minus (Num "2", Num "3")), Id "tt")

let%test _ =
  try
    let _ = Hw3.parse "let x = 1 in +" in
    false
  with
  | Failure msg -> msg = "Parsing error: let x = 1 in +"
  |_ -> false