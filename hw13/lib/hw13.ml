(* ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: *)

module F = Format

(* ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: *)

let rec interp_expr (e: Ast.expr) (zm: (Env.t * Mem.t)): Value.t =
  let (z, m) = zm in
  match e with
    | Ast.Num n -> NumV n

    | Ast.Ref x -> AddrV (Env.find x z)

    | Ast.Deref e ->
      let v: Value.t = interp_expr e zm in
      (
        match v with
          | AddrV a -> Mem.find a m
          | _ -> failwith (
            F.asprintf "Not an address: %a"
            Ast.pp_expr e
          )
      )

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
            F.asprintf "Not a number: %a"
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
                    F.asprintf "Not a bool: %a"
                    Ast.pp_expr e
                  )
              )
            ) else v1
          | _ -> failwith (
            F.asprintf "Not a bool: %a"
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
                    F.asprintf "Not a bool: %a"
                    Ast.pp_expr e
                  )
              )
            ) else v1
          | _ -> failwith (
            F.asprintf "Not a bool: %a"
            Ast.pp_expr e
          )
      )

(* ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: *)

let rec interp_stmt (s: Ast.stmt) (u: Fstore.t) (zm: (Env.t * Mem.t)):
  (Env.t * Mem.t) = 
  let (z, m) = zm in
  match s with
    | Ast.DefStmt (_, x, e) ->
      let v: Value.t = interp_expr e zm in
      let a: Env.addr = AddrManager.new_addr () in
      ((Env.add x a z), (Mem.add a v m))

    | Ast.StoreStmt (e1, e2) ->
      let v1: Value.t = interp_expr e1 zm in
      (
        match v1 with
          | AddrV a ->
            let v2: Value.t = interp_expr e2 zm in
            (z, (Mem.add a v2 m))
          | _ -> failwith (
            F.asprintf "Not an address: %a"
            Ast.pp_expr e1
          )
      )

    | Ast.IfStmt (e, sl1, sl2) ->
      let v: Value.t = interp_expr e zm in
      (
        match v with
          | BoolV b -> 
            let (_, m1) = interp_stmts (if b then sl1 else sl2) u zm in
            (z, m1)
          | _ -> failwith (
            F.asprintf "Not a bool: %a"
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
            F.asprintf "Not a bool: %a"
            Ast.pp_expr e
          )
      )

    | Ast.ReturnStmt (e) ->
      let a: Env.addr = AddrManager.ret_addr in 
      let v: Value.t = interp_expr e zm in
      (z, (Mem.add a v m))

    | Ast.CallStmt (x, f, el) ->
      let (pl, sl) = Fstore.find f u in 
      let vl: (Value.t list) = List.map
        (fun e -> interp_expr e zm) el in
      if List.length pl <> List.length el then
        failwith (
          F.asprintf 
            "The number of arguments not matched: actual %d, expected %d"
            (List.length el) (List.length pl)
        )
      else (
        (* Allocates new storage for the function arguments *)
        let al: (Env.addr list) = List.map 
          (fun _ -> AddrManager.new_addr ()) pl in
        
        (* Each parameter corresponds to an address in `al` *)
        let pal: (string * Env.addr) list = List.combine pl al in

        (* And each address corresponds to a value in `vl` *)
        let avl: (Env.addr * Value.t) list = List.combine al vl in
        (
          let z1: Env.t = List.fold_left 
          (fun acc (p, a) -> (Env.add p a acc)) z pal in
          let m1: Mem.t = List.fold_left
            (fun acc (a, v) -> (Mem.add a v acc)) m avl in
          (
            let (_, m2): (Env.t * Mem.t) = interp_stmts sl u (z1, m1) in
            let a: Env.addr = Env.find x z in
            let v: Value.t = Mem.find (AddrManager.ret_addr) m2 in
            (z, (Mem.add a v m2))
          )
        )
      )
    | InputStmt (x) ->
      let a: Env.addr = Env.find x z in 
      (
        match (read_int_opt ()) with
          | Some (n) ->
            let v: Value.t = (NumV n) in
            (z, (Mem.add a v m))
          | None -> failwith "Not Implemented!"
      ) [@coverage off]

and interp_stmts (sl: Ast.stmt list) (u: Fstore.t) (zm: (Env.t * Mem.t)):
  (Env.t * Mem.t) =
  let acc: (Env.t * Mem.t) = zm in
  List.fold_left (fun acc s -> interp_stmt s u acc) acc sl

(* ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: *)

let rec interp_fundef (d: Ast.def) (u: Fstore.t): Fstore.t =
  match d with
    | Ast.FunDef (_, f, pl, sl) -> 
      let pl_without_types: string list = 
        List.map (fun (_, x) -> x) pl
      in (Fstore.add f pl_without_types sl u)

and interp_fundefs (dl: Ast.def list) (u: Fstore.t): Fstore.t =
  let acc: Fstore.t = u in
  List.fold_left (fun acc d -> interp_fundef d acc) acc dl

(* ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: *)

let interp_prog (p: Ast.prog): (Env.t * Mem.t) =
  let _: unit = AddrManager.init () in
  let (Ast.Program (dl, sl)) = p in
  let u: Fstore.t = interp_fundefs dl [] in
  interp_stmts sl u ([], [])

(* ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: *)
