(* ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: *)

let tc_expr (e: Ast.expr): (GlobalTEnv.t * LocalTEnv.t) =
  match e with
    | _ -> failwith "Not Implemented!" 

(* ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: *)

let tc_stmt (s: Ast.stmt): (GlobalTEnv.t * LocalTEnv.t) =
  match s with
    | _ -> failwith "Not Implemented!" 

(* ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: *)

let tc_prog (p: Ast.prog): (GlobalTEnv.t * LocalTEnv.t) =
  match p with
    | _ -> failwith "Not Implemented!" 

(* ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: *)