(* ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: *)

type t = (string * Ast.typ) list

(* ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: *)

let rec remove (k: string) (z: t): t =
  match z with
    | [] -> []
    | (x, n) :: tail ->
      if x = k then tail
      else (x, n) :: (remove k tail)

(* ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: *)

let add (k: string) (v: Ast.typ) (z: t): t =
  if List.exists (fun (l, _) -> l = k) z then (k, v) :: (remove k z)
  else (k, v) :: z

(* ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: *)

let rec find (k: string) (z: t): Ast.typ =
  match z with
    | [] -> failwith "Not Implemented!"
    | (x, n) :: tail -> if x = k then n else find k tail

(* ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: *)