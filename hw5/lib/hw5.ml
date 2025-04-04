(* ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: *)

(**
  @param e AST의 노드
  @param m 추상 메모리

  @return 결과 값
*)
let rec interp (e: Ast.expr) (m: Store.t): Store.value =
  match e with
    | Ast.Num n -> NumV n
    | Ast.Id k -> Store.find k m
    | Ast.Add (e1, e2) ->
      let NumV n1 = interp e1 m in
      let NumV n2 = interp e2 m in
      NumV (n1 + n2)
    | Ast.Sub (e1, e2) ->
        let NumV n1 = interp e1 m in
        let NumV n2 = interp e2 m in
        NumV (n1 - n2)
    | Ast.LetIn (k, e1, e2) ->
      interp e2 (Store.add k (interp e1 m) m)

(* ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: *)