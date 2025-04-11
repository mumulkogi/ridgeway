(* ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: *)

type value = NumV of int

(* ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: *)

type t = (string * value) list

(* ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: *)

let rec add (k: string) (v: value) (m: t): t =
  match m with
    | [] -> (k, v) :: []
    | (x, n) :: tail ->
      if x = k then (k, v) :: tail
      else (x, n) :: (add k v tail)

let rec find (k: string) (m: t): value =
  match m with
    | [] -> failwith ("Free identifier: " ^ k)
    | (x, n) :: tail -> if x = k then n else find k tail

(* ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: *)