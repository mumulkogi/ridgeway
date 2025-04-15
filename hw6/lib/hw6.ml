(* ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: *)

let interp_def (f: Ast.fundef) (mf: Fstore.t): Fstore.t = 
  let Ast.FunDef (k, v1, v2) = f in
  Fstore.add k v1 v2 mf

(* ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: *)

let rec interp_expr (mf: Fstore.t) (mv: Store.t) (e: Ast.expr): Store.value = 
  match e with
    | Ast.Num n -> NumV n
    | Ast.Id k -> Store.find k mv
    | Ast.Add (e1, e2) ->
      let NumV n1 = interp_expr mf mv e1 in
      let NumV n2 = interp_expr mf mv e2 in
      NumV (n1 + n2)
    | Ast.Sub (e1, e2) ->
      let NumV n1 = interp_expr mf mv e1 in
      let NumV n2 = interp_expr mf mv e2 in
      NumV (n1 - n2)
    | Ast.LetIn (k, e1, e2) ->
      interp_expr mf (Store.add k (interp_expr mf mv e1) mv) e2
    | Ast.Call (fi, args) -> 
      let (params, e) = Fstore.find fi mf in
      if List.length params <> List.length args then
        failwith "Unmatched number of arguments"
      else
        (* Map each function parameter to a corresponding argument *)
        let kvs: ((string * Ast.expr) list) = List.combine params args in

        (* Construct a new 'Store' for this function call *)
        let new_mv: Store.t = List.fold_left
          (fun acc (k, v) -> Store.add k (interp_expr mf mv v) acc) [] kvs in
        interp_expr mf new_mv e
  
(* ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: *)

let interp_prog (p: Ast.prog): Store.value = 
  let Ast.Prog (fs, e) = p in
  let mf: Fstore.t = List.fold_left (fun mf f -> interp_def f mf) [] fs in
  interp_expr mf [] e 

(* ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: *)