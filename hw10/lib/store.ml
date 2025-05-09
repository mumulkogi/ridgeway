(* ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: *)

type t = (string * value) list
  and value =
    | NumV of int
    | BoolV of bool
    | ClosureV of (string * Ast.expr * t)
    | FreezedV of (Ast.expr * t)

(* ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: *)

let rec remove (e: string) (m: t): t =
  match m with
    | [] -> []
    | (x, n) :: tail ->
      if x = e then tail
      else (x, n) :: (remove e tail)

let add (k: string) (v: value) (m: t): t =
  if List.exists (fun (l, _) -> l = k) m then (k, v) :: (remove k m)
  else (k, v) :: m

let rec find (k: string) (m: t): value =
  match m with
    | [] -> failwith ("Free identifier: " ^ k)
    | (x, n) :: tail -> if x = k then n else find k tail

(* ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: *)