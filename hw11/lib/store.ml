(* ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: *)

type t = (string * value) list
  and value =
    | NumV of int
    | BoolV of bool

(* ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: *)

let rec remove (e: string) (s: t): t =
  match s with
    | [] -> []
    | (x, n) :: tail ->
      if x = e then tail
      else (x, n) :: (remove e tail)

let add (k: string) (v: value) (s: t): t =
  if List.exists (fun (l, _) -> l = k) s then (k, v) :: (remove k s)
  else (k, v) :: s

let rec find (k: string) (s: t): value =
  match s with
    | [] -> failwith ("Free identifier: " ^ k)
    | (x, n) :: tail -> if x = k then n else find k tail

(* ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: *)