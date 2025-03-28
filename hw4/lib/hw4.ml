(* ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: *)

let rec interp (e: Ast.expr): Value.t =
  match e with
    | Ast.Num n -> NumV n
    | Ast.Add (e1, e2) -> 
      let NumV n1 = interp e1 in
      let NumV n2 = interp e2 in
      NumV (n1 + n2)
    | Ast.Sub (e1, e2) ->
      let NumV n1 = interp e1 in
      let NumV n2 = interp e2 in
      NumV (n1 - n2)

(* ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: *)