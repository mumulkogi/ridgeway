(* ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: *)

type state =
  | QE
  | Q0
  | Q1
  | Q2
  | Q3
  | Q4
  | Q5
  | Q6
  | Q7
  | Q8
  | Q9
  | Q10
  | Q11

type token =
  | IDENT of string   (* `[a-z][a-zA-Z_']*` *)
  | NUMBER of string  (* `[1-9][0-9]*` *)
  | KW_LET            (* `let` *)
  | KW_IN             (* `in` *)
  | OP_EQ             (* `=` *)
  | OP_PLUS           (* `+` *)
  | OP_MINUS          (* `-` *)

type expr =
  | LetIn of (string * expr * expr)  (* `e ::= KW_LET IDENT OP_EQ e KW_IN e` *)
  | Plus of (expr * expr)            (* `e ::= e OP_PLUS e` *)
  | Minus of (expr * expr)           (* `e ::= e OP_MINUS e` *)
  | Num of string                    (* `e ::= NUMBER` *)
  | Id of string                     (* `e ::= IDENT` *)

type parse_stack_elem =
  | T of token
  | E of expr

(* ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: *)

let transition (s: state) (c: char): state =
  match (s, c) with
    (* `KW_LET`, `KW_IN`, `[a-z][a-zA-Z_']*` *)

    | (Q0, 'a'..'z') -> if c = 'l' then Q1 else if c = 'i' then Q7 else Q4
    | (Q1, 'a'..'z') -> if c = 'e' then Q2 else Q4
    | (Q2, 'a'..'z') -> if c = 't' then Q3 else Q4
    | (Q3, 'a'..'z') -> Q4
    | (Q7, 'a'..'z') -> if c = 'n' then Q8 else Q4
    | (_, 'A'..'Z') | (_, '_') | (_, '\'') -> if s = Q0 then QE else Q4

(* ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: *)

    (* `[1-9][0-9]*` *)

    | (Q0, '1'..'9') -> Q6
    | (Q6, '0'..'9') -> Q6

(* ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: *)

    (* `OP_EQ`, `OP_PLUS`, `OP_MINUS` *)

    | (Q0, '=') -> Q9
    | (Q0, '+') -> Q10
    | (Q0, '-') -> Q11

(* ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: *)

    | (_, _) -> QE

(* ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: *)

let lex (str: string): token =
  let rec lex_helper (s: state) (cl: char list): token =
    match cl with
      | [] -> (
          match s with
            | Q4 -> IDENT str
            | Q6 -> NUMBER str
            | Q3 -> KW_LET
            | Q8 -> KW_IN
            | Q9 -> OP_EQ
            | Q10 -> OP_PLUS
            | Q11 -> OP_MINUS
            | _ -> failwith ("Lexing error: " ^ str)
        )
      | head :: tail -> lex_helper (transition s head) tail
  in lex_helper Q0 (String.to_seq str |> List.of_seq)

(* ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: *)

let rec generate_tokens (sl: string list): token list =
  match sl with
    | [] -> []
    | head :: tail -> [lex head] @ generate_tokens tail

(* ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: *)

let parse (str: string): expr =
  let _: token list = generate_tokens (String.split_on_char ' ' str) in
  failwith "Not Implemented!"

(* ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: *)