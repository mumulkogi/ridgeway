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
    | head :: [] -> head
    | _ :: tail -> last tail 

(* ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: *)

let rec second_last (values: int list): int = 
  match values with
    | [] -> failwith "The given list is empty"
    | _ :: [] -> failwith "The given list has a sole element"
    | head :: _ :: [] -> head
    | _ :: tail -> second_last tail

(* ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: *)

let rec len (values: int list): int = 
  match values with
    | [] -> 0
    | _ :: tail -> 1 + (len tail)

(* ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: *)

let rec rev (values: int list): int list = 
  match values with
    | [] -> []
    | head :: [] -> [head]
    | head :: tail -> (rev tail) @ [head]

(* ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: *)

let rec is_palindrome (values: int list): bool = 
  match values with
    | [] -> true
    | _ :: [] -> true
    | head :: tail :: [] -> (head = tail)
    | head :: tail -> (
      (head == last tail) &&
      match (rev tail) with
        | [] -> true
        | _ :: tail -> is_palindrome(tail)
    )

(* ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: *)

let increment_or_push (tuple: (int * char)) (value: char): (int * char) list =
  match tuple with
    | (occurrence, letter) ->
      if letter = value then [(occurrence + 1, letter)]
      else [(1, value); (occurrence, letter)]

let rec compress_helper (values: char list): (int * char) list = 
  match values with
    | [] -> []
    | head :: [] -> [(1, head)]
    | head :: tail :: [] -> increment_or_push ((1, tail)) head
    | head :: tail -> 
      match (compress_helper tail) with 
        | [] -> failwith "Not Implemented!"
        | hz :: tz -> (increment_or_push hz head) @ tz

let compress (str: string): (int * char) list = 
  let values: char list = List.of_seq (String.to_seq str) in
  compress_helper values

(* ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: *)

let rec merge (f: int -> int -> bool) (left: int list) (right: int list): 
  int list =
    match (left, right) with
    | ([], tail) -> tail
    | (head, []) -> head
    | (hl :: tl), (hr :: tr) ->
      if (f hl hr) then hl :: merge f tl (hr :: tr)
      else hr :: merge f (hl :: tl) tr

let split (values: int list): (int list * int list) = 
  let rec split_helper (values: int list) (left: int list) (right: int list): 
    (int list * int list) =
    match values with 
      | [] -> (left, right)
      | head :: [] -> (head :: left, right)
      | head1 :: head2 :: tail ->
        split_helper tail (head1 :: left) (head2 :: right)
  in split_helper values [] []

let rec sort (f: int -> int -> bool) (values: int list): int list =
  match values with
    | [] -> []
    | head :: [] -> [head]
    | _ -> let (left, right) = split values 
           in merge f (sort f left) (sort f right)

(* ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: *)

let rec traverse (root: tree): id list = 
  match root with
    | Nil -> []
    | N(value, left, right) -> (traverse left) @ (traverse right) @ [value]

(* ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: *)