(* ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: *)

let interp_expr (e: Ast.expr) (s: Env.t) (m: Mem.t): Value.t =
  match e with
    | Ast.Num n -> NumV n
    | Ast.Ref x -> AddrV (Env.find x s)
    | Ast.Id x -> Mem.find (Env.find x s) m
    | _ -> failwith "Not Implemented!"

(* ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: *)
