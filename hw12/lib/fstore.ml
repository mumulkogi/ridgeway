(* ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: *)

type t = (string * ((string list) * (Ast.stmt list))) list

(* ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: *)

let rec remove (k: string) (z: t): t =
  match z with
    | [] -> []
    | (x, n) :: tail ->
      if x = k then tail
      else (x, n) :: (remove k tail)

let add (k: string) (v1: string list) (v2: Ast.stmt list) (z: t): t =
  let v: ((string list) * (Ast.stmt list)) = (v1, v2) in
  if List.exists (fun (l, _) -> l = k) z then (k, v) :: (remove k z)
  else (k, v) :: z

(* ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: *)

let rec find (k: string) (z: t): ((string list) * (Ast.stmt list)) =
  match z with
    | [] -> failwith ("Unbound function: " ^ k)
    | (x, n) :: tail -> if x = k then n else find k tail

(* ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: *)