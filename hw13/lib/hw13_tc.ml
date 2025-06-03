(* ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: *)

module F = Format

(* ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: *)

let failwith_expr (e: Ast.expr): 'a = 
  failwith (F.asprintf "[Ill-typed] %a" Ast.pp_expr e)

let failwith_stmt (s: Ast.stmt): 'a = 
  failwith (F.asprintf "[Ill-typed] %a" Ast.pp_stmt s)

(* ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: *)

let rec tc_expr (e: Ast.expr) (h: LocalTEnv.t): Ast.typ =
  match e with
    | Ast.Num _ -> Ast.TInt

    | Ast.Ref x -> Ast.TPtr (tc_expr (Ast.Id x) h)

    | Ast.Deref e ->
      let y: Ast.typ = tc_expr e h in
      (
        match y with
          | Ast.TPtr y1 -> y1
          | _ -> failwith_expr e
      )

    | Ast.Id x -> (
        try
          LocalTEnv.find x h
        with
          | Failure _ -> failwith_expr e
      )

    | Ast.Bool _ -> Ast.TBool

    | Ast.Add (e1, e2) 
    | Ast.Sub (e1, e2)
    | Ast.Lt (e1, e2)
    | Ast.Gt (e1, e2) ->
      let y1: Ast.typ = tc_expr e1 h in
      let y2: Ast.typ = tc_expr e2 h in
      (
        match (y1, y2) with
          | (Ast.TInt, Ast.TInt) -> (
              match e with
                | Ast.Add _
                | Ast.Sub _ -> Ast.TInt

                | Ast.Lt _
                | Ast.Gt _ -> Ast.TBool

                | _ -> failwith "Unreachable!" [@coverage off]
            )
          | _ -> failwith_expr e
      )
    
    | Ast.Eq (e1, e2) ->
      let _: Ast.typ = tc_expr e1 h in
      let _: Ast.typ = tc_expr e2 h in
      Ast.TBool

    | Ast.And (e1, e2)
    | Ast.Or (e1, e2) ->
      let y1: Ast.typ = tc_expr e1 h in
      let y2: Ast.typ = tc_expr e2 h in
      (
        match (y1, y2) with
          | (Ast.TBool, Ast.TBool) -> Ast.TBool
          | _ -> failwith_expr e
      )

(* ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: *)

let rec tc_stmt (s: Ast.stmt) (gh: (GlobalTEnv.t * LocalTEnv.t)): 
  (GlobalTEnv.t * LocalTEnv.t) =
  let (g, h) = gh in
  match s with
    | Ast.DefStmt (y, x, e) ->
      let y1: Ast.typ = 
        try (tc_expr e h) with Failure _ -> failwith_stmt s in
      (
        if (y = y1) then (g, (LocalTEnv.add x y h))
        else failwith_stmt s
      )

    | Ast.StoreStmt (e1, e2) ->
      let y1: Ast.typ = 
        try (tc_expr e1 h) with Failure _ -> failwith_stmt s in
      let y2: Ast.typ = 
        try (tc_expr e2 h) with Failure _ -> failwith_stmt s in
      (
        if (y2 = Ast.TPtr y1) then gh
        else failwith_stmt s
      )

    | Ast.IfStmt (e, sl1, sl2) ->
      let y: Ast.typ = 
        try (tc_expr e h) with Failure _ -> failwith_stmt s in
      (
        if (y = Ast.TBool) then (
          let _ = tc_stmts sl1 gh in
          let _ = tc_stmts sl2 gh in
          gh
        ) else failwith_stmt s
      )

    | Ast.LoopStmt (e, sl) -> 
      let y: Ast.typ = 
        try (tc_expr e h) with Failure _ -> failwith_stmt s in
      (
        if (y = Ast.TBool) then (
          let (_, _) = tc_stmts sl gh in gh
        ) else failwith_stmt s
      )

    | Ast.ReturnStmt (e) ->
      let _: Ast.typ = 
        try (tc_expr e h) with Failure _ -> failwith_stmt s in
      gh

    | Ast.CallStmt (x, f, el) ->
      let yx: Ast.typ = 
        try (tc_expr (Ast.Id x) h) with Failure _ -> failwith_stmt s in
      let yf: Ast.typ = 
        try (GlobalTEnv.find f g) with Failure _ -> failwith_stmt s in
      (
        if List.is_empty el then (
          match yf with
            | Ast.TArrow (Ast.TUnit, yr) when yx = yr -> gh
            | _ -> failwith_stmt s
        ) else (
          let _: (Ast.typ list) = List.map
            (fun e -> tc_expr e h) el in
          failwith "Not Implemented!"
        )
      )
      
    | InputStmt (x) ->
      let _: Ast.typ = 
        try (tc_expr (Ast.Id x) h) with Failure _ -> failwith_stmt s in
      gh

and tc_stmts (sl: Ast.stmt list) (gh: (GlobalTEnv.t * LocalTEnv.t)):
  (GlobalTEnv.t * LocalTEnv.t) =
  let acc: (GlobalTEnv.t * LocalTEnv.t) = gh in
  List.fold_left (fun acc s -> tc_stmt s acc) acc sl

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