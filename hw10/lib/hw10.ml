(* ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: *)

let rec interp (e: Ast.expr) (m: Store.t): Store.value =
  match e with
    | Ast.Num n -> NumV n
    | Ast.Bool b -> BoolV b
    | Ast.Id k ->
      let v: Store.value = Store.find k m in
      (
        match v with
          | FreezedV (e, m) -> interp e m
          | _ -> v
      )
    | Ast.Add (e1, e2) ->
      let v1: Store.value = interp e1 m in
      let v2: Store.value = interp e2 m in
      (
        match (v1, v2) with
          | (NumV n1, NumV n2) -> NumV (n1 + n2)
          | _ -> failwith (
            Format.asprintf "Not a number: %a + %a" 
            Ast.pp e1 Ast.pp e2
          )
      )
    | Ast.Sub (e1, e2) ->
      let v1: Store.value = interp e1 m in
      let v2: Store.value = interp e2 m in
      (
        match (v1, v2) with
          | (NumV n1, NumV n2) -> NumV (n1 - n2)
          | _ -> failwith (
            Format.asprintf "Not a number: %a - %a" 
            Ast.pp e1 Ast.pp e2
          )
      )
    | Ast.LetIn (k, e1, e2) -> interp e2 (Store.add k (interp e1 m) m)
    | Ast.RLetIn (_, _, _) -> failwith "Not Implemented!"
    | Ast.App (e1, e2) ->
      let f: Store.value = interp e1 m in
      (
        match f with
          | ClosureV (x, b, mf) -> 
            let v: Store.value = FreezedV (e2, m) in
            interp b (Store.add x v mf)
          | _ -> failwith (
            Format.asprintf "Not a function: %a" 
            Ast.pp e1
          )
      )
    | Ast.Lambda (x, b) -> ClosureV (x, b, m)
    | Ast.Cond (ec, e1, e2) -> 
      let vc: Store.value = interp ec m in
      (
        match vc with
          | BoolV b -> 
              let v1: Store.value = interp e1 m in
              let v2: Store.value = interp e2 m in
              if b then v1 else v2
          | _ -> failwith (
            Format.asprintf "Not a bool: %a" 
            Ast.pp ec
          )
      )
    | Ast.LessThan (e1, e2) ->
      let v1: Store.value = interp e1 m in
      let v2: Store.value = interp e2 m in
      (
        match (v1, v2) with
          | (NumV n1, NumV n2) -> BoolV (n1 < n2)
          | _ -> failwith (
            Format.asprintf "Not a number: %a < %a" 
            Ast.pp e1 Ast.pp e2
          )
      )

(* ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: *)