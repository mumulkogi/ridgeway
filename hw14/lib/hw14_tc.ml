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

    | Ast.Array (y, e1, e2) ->
      let y1: Ast.typ = tc_expr e1 h in
      let y2: Ast.typ = tc_expr e2 h in
      if (y1 = Ast.TInt && y2 = y) then Ast.TArray y
      else failwith_expr e

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

    | Ast.Index (e1, e2) ->
      let y1: Ast.typ = tc_expr e1 h in
      let y2: Ast.typ = tc_expr e2 h in
      (
        match (y1, y2) with
          | (Ast.TArray y, Ast.TInt) -> y
          | _ -> failwith_expr e
      )

    | Ast.Tuple (e1, e2) -> 
      let y1: Ast.typ = tc_expr e1 h in
      let y2: Ast.typ = tc_expr e2 h in
      Ast.TTuple (y1, y2)

    | Ast.First (e1) 
    | Ast.Second (e1) ->
      let y: Ast.typ = tc_expr e1 h in
      (
        match y with
          | Ast.TTuple (y1, y2) -> 
            (
              match e with
                | Ast.First _ -> y1
                | Ast.Second _ -> y2
                | _ -> failwith "Unreachable!" [@coverage off]
            )

          | _ -> failwith_expr e1
      )

(* ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: *)

let rec tc_stmt (s: Ast.stmt) 
                (gh: (GlobalTEnv.t * LocalTEnv.t)) 
                (yr: Ast.typ): LocalTEnv.t =
  let (g, h) = gh in
  match s with
    | Ast.DefStmt (y, x, e) ->
      let ye: Ast.typ = (tc_expr e h) in
      (
        if (ye = y) then (LocalTEnv.add x y h)
        else failwith_stmt s
      )

    | Ast.DefArrInitStmt (y, x, el) -> 
      (
        match y with
          | Ast.TArray y1 -> 
            let _: Ast.typ = List.fold_left 
              (fun acc e -> 
                if ((tc_expr e h) = acc) then acc 
                else failwith_stmt s
              ) y1 el in
            (LocalTEnv.add x y h)

          | _ -> failwith_stmt s
      )

    | Ast.StoreStmt (e1, e2) ->
      let ye1: Ast.typ = (tc_expr e1 h) in
      let ye2: Ast.typ = (tc_expr e2 h) in
      (
        if (ye1 = Ast.TPtr ye2) then h
        else failwith_stmt s
      )

    | Ast.IfStmt (e, sl1, sl2) ->
      let ye: Ast.typ = (tc_expr e h) in
      (
        if (ye = Ast.TBool) then (
          let _: LocalTEnv.t = tc_stmts sl1 gh yr in
          let _: LocalTEnv.t = tc_stmts sl2 gh yr in
          h
        ) else failwith_stmt s
      )

    | Ast.LoopStmt (e, sl) -> 
      let ye: Ast.typ = (tc_expr e h) in
      (
        if (ye = Ast.TBool) then (
          let _: LocalTEnv.t = tc_stmts sl gh yr in h
        ) else failwith_stmt s
      )

    | Ast.ReturnStmt (e) ->
      let ye: Ast.typ = (tc_expr e h) in
      if (ye = yr) then h
      else failwith_stmt s

    | Ast.CallStmt (x, f, el) ->
      let yx: Ast.typ = (tc_expr (Ast.Id x) h) in
      let yf: Ast.typ = (GlobalTEnv.find f g) in
      (
        if List.is_empty el then (
          match yf with
            | Ast.TArrow (Ast.TUnit, y) when yx = y -> h
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
          let y: Ast.typ = List.hd (List.rev yl1) in
          (
            if ((List.compare_lengths yl1 (yl2 @ [y])) = 0)
              && (List.equal (=) yl1 (yl2 @ [y])) && (yx = y) then h
            else failwith_stmt s
          )
        )
      )
      
    | Ast.InputStmt (x) ->
      let yx: Ast.typ = (tc_expr (Ast.Id x) h) in
      if yx = Ast.TInt then h
      else failwith_stmt s

    | Ast.UpdateStmt (x, e1, e2) ->
      let yx: Ast.typ = (tc_expr (Ast.Id x) h) in
      let ye1: Ast.typ = (tc_expr e1 h) in
      let ye2: Ast.typ = (tc_expr e2 h) in 
      (
        match yx with
          | Ast.TArray y 
            when ye1 = Ast.TInt && ye2 = y -> h [@coverage off]
          
          | _ -> failwith_stmt s
      )

and tc_stmts (sl: Ast.stmt list) 
             (gh: (GlobalTEnv.t * LocalTEnv.t)) 
             (yr: Ast.typ): LocalTEnv.t =
  let (g, h) = gh in
  let acc: LocalTEnv.t = h in
  List.fold_left (fun acc s -> tc_stmt s (g, acc) yr) acc sl

(* ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: *)

let rec tc_fundef (d: Ast.def) (g: GlobalTEnv.t): (GlobalTEnv.t) =
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
    ) in 
    (GlobalTEnv.add f yf g)
  )

and tc_fundefs (dl: Ast.def list) (g: GlobalTEnv.t): (GlobalTEnv.t) =
  let acc: GlobalTEnv.t = g in
  List.fold_left (fun acc d -> tc_fundef d acc) acc dl

(* ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: *)

let tc_prog (p: Ast.prog): (GlobalTEnv.t * LocalTEnv.t) =
  let (Ast.Program (dl, sl)) = p in
  let g: GlobalTEnv.t = tc_fundefs dl [] in
  let _: unit = List.iter 
    (
      fun (Ast.FunDef (yr, _, pl, sl)) -> (
        let h: LocalTEnv.t = List.fold_left 
          (fun acc (y, x) -> LocalTEnv.add x y acc) [] pl in
        let _: LocalTEnv.t = tc_stmts sl (g, h) yr in ()
      )
    ) 
    dl in
  let h = tc_stmts sl (g, []) Ast.TUnit in
  (g, h)

(* ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: *)