(* ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: *)

let rec interp_expr (e: Ast.expr) (z: Env.t) (m: Mem.t): Value.t =
  match e with
    | Ast.Num n -> NumV n
    | Ast.Ref x -> AddrV (Env.find x z)
    | Ast.Id x -> Mem.find (Env.find x z) m
    | Ast.Bool b -> BoolV b
    | Ast.Add (e1, e2) 
    | Ast.Sub (e1, e2)
    | Ast.Lt (e1, e2)
    | Ast.Gt (e1, e2) ->
      let v1: Value.t = interp_expr e1 z m in
      let v2: Value.t = interp_expr e2 z m in
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
      let v1: Value.t = interp_expr e1 z m in
      let v2: Value.t = interp_expr e2 z m in
      (
        match (v1, v2) with
          | (NumV n1, NumV n2) -> BoolV (n1 == n2)
          | (BoolV b1, BoolV b2) -> BoolV (b1 == b2)
          | (AddrV a1, AddrV a2) -> BoolV (a1 == a2)
          | _ -> BoolV false
      )
    | Ast.And (e1, e2) ->
      let v1: Value.t = interp_expr e1 z m in
      (
        match v1 with
          | BoolV b1 ->
            if b1 then (
              let v2: Value.t = interp_expr e2 z m in
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
      let v1: Value.t = interp_expr e1 z m in
      (
        match v1 with
          | BoolV b1 ->
            if not b1 then (
              let v2: Value.t = interp_expr e2 z m in
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

(* ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: *)
