(* ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: *)

let rec interp_expr (e: Ast.expr) (s: Store.t): Store.value =
  match e with
    | Ast.Num n -> NumV n
    | Ast.Name x -> (Store.find x s)
    | Ast.Bool b -> BoolV b
    | Ast.Add (e1, e2) 
    | Ast.Sub (e1, e2)
    | Ast.Lt (e1, e2)
    | Ast.Gt (e1, e2) ->
      let v1: Store.value = interp_expr e1 s in
      let v2: Store.value = interp_expr e2 s in
      (
        match (v1, v2) with
          | (NumV n1, NumV n2) -> (
              match e with
                | Ast.Add _ -> NumV (n1 + n2)
                | Ast.Sub _ -> NumV (n1 - n2)
                | Ast.Lt _ -> BoolV (n1 < n2)
                | Ast.Gt _ -> BoolV (n1 > n2)
                | _ -> failwith "Unreachable!" [@coverage off]
            )
          | _ -> failwith (
            Format.asprintf "Not a number: %a"
            Ast.pp_expr e
          )
      )
    | Ast.Eq (e1, e2) ->
      let v1: Store.value = interp_expr e1 s in
      let v2: Store.value = interp_expr e2 s in
      (
        match (v1, v2) with
          | (NumV n1, NumV n2) -> BoolV (n1 == n2)
          | (BoolV b1, BoolV b2) -> BoolV (b1 == b2)
          | _ -> BoolV false
      )
    | Ast.And (e1, e2) ->
      let v1: Store.value = interp_expr e1 s in
      (
        match v1 with
          | BoolV b1 ->
            if b1 then (
              let v2: Store.value = interp_expr e2 s in
              (
                match v2 with
                  | BoolV b2 -> BoolV (b1 == b2)
                  | _ -> failwith (
                    Format.asprintf "Not a bool: %a"
                    Ast.pp_expr e
                  )
              )
            ) else v1
          | _ -> failwith (
            Format.asprintf "Not a bool: %a"
            Ast.pp_expr e
          )
      )
    | Ast.Or (e1, e2) ->
      let v1: Store.value = interp_expr e1 s in
      (
        match v1 with
          | BoolV b1 ->
            if not b1 then (
              let v2: Store.value = interp_expr e2 s in
              (
                match v2 with
                  | BoolV _ -> v2
                  | _ -> failwith (
                    Format.asprintf "Not a bool: %a"
                    Ast.pp_expr e
                  )
              )
            ) else v1
          | _ -> failwith (
            Format.asprintf "Not a bool: %a"
            Ast.pp_expr e
          )
      )

(* ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: *)

let rec interp_stmt (z: Ast.stmt) (s: Store.t): Store.t =
  match z with
    | Ast.AssignStmt (x, e) ->
      let v: Store.value = interp_expr e s in
      Store.add x v s
    | Ast.IfStmt (e, zl1, zl2) ->
      let v: Store.value = interp_expr e s in
      (
        match v with
          | BoolV b -> (
              if b then interp_stmts zl1 s
              else interp_stmts zl2 s
            )
          | _ -> failwith (
            Format.asprintf "Not a bool: %a"
            Ast.pp_expr e
          )
      )
    | Ast.LoopStmt (e, zl) -> 
      let v: Store.value = interp_expr e s in
      (
        match v with
          | BoolV b -> (
              if b then (
                let s1: Store.t = interp_stmts zl s in
                interp_stmt z s1
              ) else s
            )
          | _ -> failwith (
            Format.asprintf "Not a bool: %a"
            Ast.pp_expr e
          )
      )

and interp_stmts (zl: Ast.stmt list) (s: Store.t): Store.t =
  List.fold_left (fun s z -> interp_stmt z s) s zl

(* ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: *)

let interp_prog (p: Ast.prog): Store.t =
  let Ast.Program zl = p in interp_stmts zl []

(* ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: *)