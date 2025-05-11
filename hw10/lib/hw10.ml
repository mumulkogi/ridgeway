(* ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: *)

let rec interp (e: Ast.expr) (m: Store.t): Store.value =
  match e with
    | Ast.Num n -> NumV n
    | Ast.Bool b -> BoolV b
    | Ast.Id k ->
      let v: Store.value = Store.find k m in
      (
        match v with
          | FreezedV (e1, m1) -> interp e1 m1
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
    | Ast.LetIn (k, e1, e2) -> 
      let v: Store.value = interp e1 m in
      interp e2 (Store.add k v m)
    | Ast.RLetIn (k, e1, e2) ->
      let f: Store.value = interp e1 m in
      (
        match f with
          | ClosureV (x, b, m1) ->
            let rec m2: Store.t = (k, Store.ClosureV (x, b, m2)) :: m1 in
            interp e2 m2
          | _ -> failwith (
            Format.asprintf "Not a function: %a" 
            Ast.pp e1
          )
      )
    | Ast.App (e1, e2) ->
      let f: Store.value = interp e1 m in
      (
        match f with
          | ClosureV (x, b, m1) -> 
            let v: Store.value = FreezedV (e2, m) in
            let m2: Store.t = Store.add x v m1 in
            interp b m2
          | _ -> failwith (
            Format.asprintf "Not a function: %a" 
            Ast.pp e1
          )
      )
    | Ast.Lambda (x, b) -> ClosureV (x, b, m)
    | Ast.Cond (e1, e2, e3) -> 
      let v1: Store.value = interp e1 m in
      (
        match v1 with
          | BoolV b -> if b then (interp e2 m) else (interp e3 m)
          | _ -> failwith (
            Format.asprintf "Not a bool: %a" 
            Ast.pp e1
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