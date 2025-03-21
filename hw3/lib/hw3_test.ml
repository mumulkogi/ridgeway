(* lex *)
let%test _ = Hw3.lex "1320" = NUMBER "1320"

let%test _ = 
  try
    let _ = Hw3.lex "Answer" in
    false
  with
  | Failure msg -> msg = "Lexing error: Answer"
  |_ -> false

(* parse *)
let%test _ = Hw3.parse "let x = 1 in 4 + x" = LetIn ("x", Num "1", Plus (Num "4", Id "x"))

let%test _ =
  try
    let _ = Hw3.parse "let x = 1 in +" in
    false
  with
  | Failure msg -> msg = "Parsing error: let x = 1 in +"
  |_ -> false