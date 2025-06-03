(* ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: *)

module F = Format

(* ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: *)

let tc_expr (e: Ast.expr) (h: LocalTEnv.t): Ast.typ =
  match e with
    | Ast.Num _ -> Ast.TInt

    | Ast.Ref x ->
      let y: Ast.typ = (
        try
          LocalTEnv.find x h
        with
          | Failure _ -> failwith (
              F.asprintf "[Ill-typed] %a" Ast.pp_expr e
            )
      ) in (Ast.TPtr y)

    | _ -> failwith "Not Implemented!"

(* ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: *)

let tc_stmt (s: Ast.stmt) (gh: (GlobalTEnv.t * LocalTEnv.t)): 
  (GlobalTEnv.t * LocalTEnv.t) =
  let (_, _) = gh in
  match s with
    | _ -> failwith "Not Implemented!"

(* ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: *)

let tc_fundef (d: Ast.def) (gh: (GlobalTEnv.t * LocalTEnv.t)): 
  (GlobalTEnv.t * LocalTEnv.t) =
  let (_, _) = gh in
  match d with
    | _ -> failwith "Not Implemented!"

(* ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: *)

let tc_prog (p: Ast.prog): (GlobalTEnv.t * LocalTEnv.t) =
  match p with
    | _ -> failwith "Not Implemented!" 

(* ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: *)