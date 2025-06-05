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

    | Ast.Deref x ->
      let y: Ast.typ = tc_expr x h in
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
        try (tc_expr e h) with Failure _ -> failwith_stmt s [@coverage off] in
      (
        if (y = y1) then (g, (LocalTEnv.add x y h))
        else failwith_stmt s
      )

    | Ast.StoreStmt (e1, e2) ->
      let y1: Ast.typ = 
        try (tc_expr e1 h) with Failure _ -> failwith_stmt s [@coverage off] in 
      let y2: Ast.typ = 
        try (tc_expr e2 h) with Failure _ -> failwith_stmt s [@coverage off] in
      (
        if (y1 = Ast.TPtr y2) then gh
        else failwith_stmt s
      )

    | Ast.IfStmt (e, sl1, sl2) ->
      let y: Ast.typ = 
        try (tc_expr e h) with Failure _ -> failwith_stmt s [@coverage off] in
      (
        if (y = Ast.TBool) then (
          let (_, _) = tc_stmts sl1 gh in
          let (_, _) = tc_stmts sl2 gh in
          gh
        ) else failwith_stmt s
      )

    | Ast.LoopStmt (e, sl) -> 
      let y: Ast.typ = 
        try (tc_expr e h) with Failure _ -> failwith_stmt s [@coverage off] in
      (
        if (y = Ast.TBool) then (
          let (_, _) = tc_stmts sl gh in gh
        ) else failwith_stmt s
      )

    | Ast.ReturnStmt (e) ->
      let _: Ast.typ = 
        try 
          (tc_expr e h) 
        with Failure _ -> failwith_stmt s [@coverage off] in
      gh

    | Ast.CallStmt (x, f, el) ->
      let yx: Ast.typ = 
        try 
          (tc_expr (Ast.Id x) h) 
        with Failure _ -> failwith_stmt s [@coverage off] in
      let yf: Ast.typ = 
        try 
          (GlobalTEnv.find f g) 
        with Failure _ -> failwith_stmt s [@coverage off] in
      (
        if List.is_empty el then (
          match yf with
            | Ast.TArrow (Ast.TUnit, yr) when yx = yr -> gh
            | _ -> failwith_stmt s
        ) else (
          let rec arrow_to_typ_list (yf: Ast.typ): (Ast.typ list) = (
            match yf with
              | Ast.TArrow (y1, y2) -> y1 :: (arrow_to_typ_list y2)
              | y -> [y]
          ) in
          let yl1: (Ast.typ list) = arrow_to_typ_list yf in
          let yl2: (Ast.typ list) = (List.map
            (fun e -> tc_expr e h) el) in
          let yr: Ast.typ = List.hd (List.rev yl1) in
          (
            if ((List.compare_lengths yl1 (yl2 @ [yr])) = 0)
              && (List.equal (=) yl1 (yl2 @ [yr])) && (yx = yr) then gh
            else failwith_stmt s
          )
        )
      )
      
    | InputStmt (x) ->
      let yx: Ast.typ = 
        try 
          (tc_expr (Ast.Id x) h)
        with Failure _ -> failwith_stmt s [@coverage off] in
      if yx = Ast.TInt then gh 
      else failwith_stmt s

and tc_stmts (sl: Ast.stmt list) (gh: (GlobalTEnv.t * LocalTEnv.t)):
  (GlobalTEnv.t * LocalTEnv.t) =
  let acc: (GlobalTEnv.t * LocalTEnv.t) = gh in
  List.fold_left (fun acc s -> tc_stmt s acc) acc sl

(* ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: *)

let rec tc_fundef (d: Ast.def) (gh: (GlobalTEnv.t * LocalTEnv.t)): 
  (GlobalTEnv.t * LocalTEnv.t) =
  let (g, h) = gh in
  let (Ast.FunDef (yr, f, pl, _)) = d in
  (
    let yf: Ast.typ = (
      match pl with
        | [] -> Ast.TArrow (Ast.TUnit, yr)
        | [(y, _)] -> Ast.TArrow (y, yr)
        | (y, _) :: tail -> (
          List.fold_right 
            (fun (y, _) acc -> Ast.TArrow (y, acc)) (tail @ [(yr, "")]) y
        )
    ) in ((GlobalTEnv.add f yf g), h)
  )

and tc_fundefs (dl: Ast.def list) (gh: (GlobalTEnv.t * LocalTEnv.t)):
  (GlobalTEnv.t * LocalTEnv.t) =
  let acc: (GlobalTEnv.t * LocalTEnv.t) = gh in
  List.fold_left (fun acc d -> tc_fundef d acc) acc dl

(* ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: *)

let tc_prog (p: Ast.prog): (GlobalTEnv.t * LocalTEnv.t) =
  let (Ast.Program (dl, sl)) = p in
  let (g, _) = tc_fundefs dl ([], []) in
  let (_, h) = tc_stmts sl (g, []) in
  (g, h)

(* ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: *)