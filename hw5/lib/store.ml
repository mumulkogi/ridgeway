(* ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: *)

type value = NumV of int

(* ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: *)

type t = (string * value) list

let empty: t = []

(* ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: *)

(**
  @param k 변수의 이름
  @param v 변수의 값
  @param m 추상 메모리

  @return 변수 이름과 값이 추가된 추상 메모리
*)
let rec add (k: string) (v: value) (m: t): t =
  match m with
    | [] -> (k, v) :: []
    | (x, n) :: tail ->
      if x = k then (k, v) :: tail
      else (x, n) :: (add k v tail)

(**
  @param k 변수의 이름
  @param m 추상 메모리

  @return 변수의 값
*)
let rec find (k: string) (m: t): value =
  match m with
    | [] -> failwith ("Free identifier: " ^ k)
    | (x, n) :: tail -> if x = k then n else find k tail

(* ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: *)