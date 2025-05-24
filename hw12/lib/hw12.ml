(* ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: *)

let rec interp_expr (e: Ast.expr) (zm: (Env.t * Mem.t)): Value.t =
  let (z, m) = zm in
  match e with
    | Ast.Num n -> NumV n
    | Ast.Ref x -> AddrV (Env.find x z)
    | Ast.Id x -> Mem.find (Env.find x z) m
    | Ast.Bool b -> BoolV b
    | Ast.Add (e1, e2) 
    | Ast.Sub (e1, e2)
    | Ast.Lt (e1, e2)
    | Ast.Gt (e1, e2) ->
      let v1: Value.t = interp_expr e1 zm in
      let v2: Value.t = interp_expr e2 zm in
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
      let v1: Value.t = interp_expr e1 zm in
      let v2: Value.t = interp_expr e2 zm in
      (
        match (v1, v2) with
          | (NumV n1, NumV n2) -> BoolV (n1 == n2)
          | (BoolV b1, BoolV b2) -> BoolV (b1 == b2)
          | (AddrV a1, AddrV a2) -> BoolV (a1 == a2)
          | _ -> BoolV false
      )
    | Ast.And (e1, e2) ->
      let v1: Value.t = interp_expr e1 zm in
      (
        match v1 with
          | BoolV b1 ->
            if b1 then (
              let v2: Value.t = interp_expr e2 zm in
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
      let v1: Value.t = interp_expr e1 zm in
      (
        match v1 with
          | BoolV b1 ->
            if not b1 then (
              let v2: Value.t = interp_expr e2 zm in
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

let rec interp_stmt (s: Ast.stmt) (u: Fstore.t) (zm: (Env.t * Mem.t)):
  (Env.t * Mem.t) = 
  let (z, m) = zm in
  match s with
    | Ast.DefStmt (x, e) ->
      let v: Value.t = interp_expr e zm in
      let a: Env.addr = AddrManager.new_addr () in
      ((Env.add x a z), (Mem.add a v m))
    | Ast.IfStmt (e, sl1, sl2) ->
      let v: Value.t = interp_expr e zm in
      (
        match v with
          | BoolV b -> 
            let (_, m1) = interp_stmts (if b then sl1 else sl2) u zm in
            (z, m1)
          | _ -> failwith (
            Format.asprintf "Not a bool: %a"
            Ast.pp_expr e
          )
      )
    | Ast.LoopStmt (e, sl) -> 
      let v: Value.t = interp_expr e zm in
      (
        match v with
          | BoolV b -> (
              if b then (
                let (_, m1) = interp_stmts sl u zm in
                let (_, m2) = interp_stmt s u (z, m1) in
                (z, m2)
              ) else zm
            )
          | _ -> failwith (
            Format.asprintf "Not a bool: %a"
            Ast.pp_expr e
          )
      )
    | _ -> failwith "Not Implemented!"

and interp_stmts (sl: Ast.stmt list) (u: Fstore.t) (zm: (Env.t * Mem.t)):
  (Env.t * Mem.t) =
  let acc: (Env.t * Mem.t) = zm in
  List.fold_left (fun acc s -> interp_stmt s u acc) acc sl

(* ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: *)

let interp_prog (p: Ast.prog): (Env.t * Mem.t) =
  let _: unit = AddrManager.init () in
  let (Ast.Program (_, _)) = p in
  failwith "Not Implemented!"

(* ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: *)
