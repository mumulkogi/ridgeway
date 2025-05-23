(* ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: *)

module F = Format

(* ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: *)

type t = (Env.addr * Value.t) list

(* ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: *)

let rec remove (e: Env.addr) (z: t): t =
  match z with
    | [] -> []
    | (x, n) :: tail ->
      if x = e then tail
      else (x, n) :: (remove e tail)

let add (k: Env.addr) (v: Value.t) (z: t): t =
  if List.exists (fun (l, _) -> l = k) z then (k, v) :: (remove k z)
  else (k, v) :: z

let rec find (k: Env.addr) (z: t): Value.t =
  match z with
    | [] -> failwith (F.sprintf "Free address: %d" k)
    | (x, n) :: tail -> if x = k then n else find k tail

(* ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: *)