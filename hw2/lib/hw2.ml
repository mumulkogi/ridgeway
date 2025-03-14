(* ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: *)

type id = int
type tree = Nil | N of id * tree * tree

(* ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: *)

let rec fib (n: int): int = 
  match n with
    | 0 -> 0
    | 1 -> 1
    | n -> if n < 0 then -1 else fib (n - 1) + fib (n - 2)

(* ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: *)

let fib_opt (n: int): int = 
  (* `for (int i = 0; i < n; i++)` *)
  let rec fib_opt_helper (x: int) (y: int) (i: int): int =
    if n < 0 then -1
    else if i < n then fib_opt_helper y (x + y) (i + 1)
    else x
  in fib_opt_helper 0 1 0

(* ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: *)

let rec last (values: int list): int = 
  match values with
    | [] -> failwith "The given list is empty"
    | first :: [] -> first
    | _ :: the_others -> last the_others 

(* ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: *)

let rec second_last (values: int list): int = 
  match values with
    | [] -> failwith "The given list is empty"
    | _ :: [] -> failwith "The given list has a sole element"
    | first :: _ :: [] -> first
    | _ :: the_others -> second_last the_others

(* ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: *)

let rec len (values: int list): int = 
  match values with
    | [] -> 0
    | _ :: the_others -> 1 + len the_others

(* ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: *)

let rec rev (values: int list): int list = 
  failwith "Not Implemented!"

(* ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: *)

let is_palindrome (_: int list): bool = 
  failwith "Not Implemented!"

(* ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: *)

let compress (_: string): (int * char) list = 
  failwith "Not Implemented!"

(* ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: *)

let sort (_: int -> int -> bool) (_: int list): int list = 
  failwith "Not Implemented!"

(* ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: *)

let traverse (_: tree): id list = 
  failwith "Not Implemented!"

(* ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: *)