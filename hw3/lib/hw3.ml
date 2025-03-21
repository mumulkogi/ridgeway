(* ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: *)

type token =
  | IDENT of string   (* `[a-z][a-zA-Z_'] *)
  | NUMBER of string  (* `[1-9][0-9]` *)
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

let lex (_: string): token =
  failwith "Not Implemented!"

(* ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: *)

let parse (_: string): expr =
  failwith "Not Implemented!"

(* ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: *)