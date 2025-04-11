(* ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: *)

type t = (string * (string list * Ast.expr)) list

(* ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: *)

let rec add (k: string) (v1: string list) (v2: Ast.expr) (m: t): t = 
  let v: (string list * Ast.expr) = (v1, v2) in
  match m with
    | [] -> (k, v) :: []
    | (x, n) :: tail ->
      if x = k then (k, v) :: tail
      else (x, n) :: (add k v1 v2 tail)

(* ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: *)

let rec find (k: string) (m: t): (string list * Ast.expr) = 
  match m with
    | [] -> failwith ("Undefined function: " ^ k)
    | (x, n) :: tail -> if x = k then n else find k tail

(* ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: *)