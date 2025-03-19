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
    | head :: second :: [] -> (head = second)
    | head :: tail -> (
      (head == last tail) &&
      match (rev tail) with
        | [] -> true
        | _ :: tail -> is_palindrome(tail)
    )

(* ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: *)

let compress_helper (values: char list): (int * char) list = 
  match values with
    | [] -> []
    | head :: [] -> [(1, head)]
    | _ :: _ -> failwith "Not Implemented!"

let compress (str: string): (int * char) list = 
  let values: char list = List.of_seq (String.to_seq str) in
  compress_helper values

(* ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: *)

let split (values: int list): (int list * int list) = 
  let rec split_helper (values: int list) (left: int list) (right: int list): 
    (int list * int list) =
    match values with 
      | [] -> (left, right)
      | head :: [] -> (head :: left, right)
      | head :: second :: tail ->
        split_helper tail (head :: left) (second :: right)
  in split_helper values [] []

let rec merge (f: int -> int -> bool) (left: int list) (right: int list): 
  int list =
    match (left, right) with
    | ([], tail) -> tail
    | (head, []) -> head
    | (head1 :: tail1), (head2 :: tail2) ->
      if (f head1 head2) then head1 :: merge f tail1 (head2 :: tail2)
      else head2 :: merge f (head1 :: tail1) tail2

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