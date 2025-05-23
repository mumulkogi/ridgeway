module F = Format

type expr = 
  | Num of int 
  | Ref of string 
  | Id of string 
  | Bool of bool 
  | Add of expr * expr
  | Sub of expr * expr 
  | Lt of expr * expr 
  | Gt of expr * expr 
  | Eq of expr * expr
  | And of expr * expr 
  | Or of expr * expr

type stmt = 
  | DefStmt of string * expr
  | StoreStmt of expr * expr
  | LoadStmt of string * expr
  | IfStmt of expr * stmt list * stmt list
  | LoopStmt of expr * stmt list
  | ReturnStmt of expr
  | CallStmt of string * string * expr list

type def = 
  | FunDef of string * string list * stmt list

type prog = Program of def list * stmt list

let rec pp_expr fmt e = 
  match e with
  | Num n -> F.fprintf fmt "%d" n
  | Id id -> F.fprintf fmt "%s" id
  | Ref id -> F.fprintf fmt "&%s" id
  | Bool b -> F.fprintf fmt "%b" b
  | Add (e1, e2) -> F.fprintf fmt "%a + %a" pp_expr e1 pp_expr e2
  | Sub (e1, e2) -> F.fprintf fmt "%a - %a" pp_expr e1 pp_expr e2
  | Lt (e1, e2) -> F.fprintf fmt "%a < %a" pp_expr e1 pp_expr e2
  | Gt (e1, e2) -> F.fprintf fmt "%a > %a" pp_expr e1 pp_expr e2
  | Eq (e1, e2) -> F.fprintf fmt "%a == %a" pp_expr e1 pp_expr e2
  | And (e1, e2) -> F.fprintf fmt "%a && %a" pp_expr e1 pp_expr e2
  | Or (e1, e2) -> F.fprintf fmt "%a || %a" pp_expr e1 pp_expr e2

let rec pp_stmt fmt s = 
  match s with 
  | DefStmt (x, e) -> F.fprintf fmt "def %s = %a;" x pp_expr e
  | LoadStmt (x, e) -> F.fprintf fmt "%s = *%a;" x pp_expr e
  | StoreStmt (e1, e2) -> F.fprintf fmt "*%a = %a;" pp_expr e1 pp_expr e2
  | IfStmt (e, sl1, sl2) -> F.fprintf fmt "if %a { %a } else { %a }" pp_expr e pp_stmt_list sl1 pp_stmt_list sl2
  | LoopStmt (e, sl) -> F.fprintf fmt "while(%a) { %a }" pp_expr e pp_stmt_list sl
  | ReturnStmt e -> F.fprintf fmt "return %a;" pp_expr e
  | CallStmt (x, f, el) -> F.fprintf fmt "%s = %s(%a);" x f 
      (F.pp_print_list ~pp_sep:(fun fmt () -> F.fprintf fmt ", ") (fun fmt e -> F.fprintf fmt "%a" pp_expr e)) el

and pp_stmt_list fmt sl = 
  F.pp_print_list ~pp_sep:(fun fmt () -> F.fprintf fmt "\n") 
    (fun fmt s -> F.fprintf fmt "%a" pp_stmt s)
    fmt
    sl

let pp_def fmt d = 
  match d with
  | FunDef (f, pl, sl) ->
      F.fprintf fmt "def %s(%a) {\n%a\n}"
      f
      (F.pp_print_list ~pp_sep:(fun fmt () -> F.fprintf fmt ", ") (fun fmt p -> F.fprintf fmt "%s" p)) pl
      pp_stmt_list sl

let pp_def_list fmt dl =
  F.pp_print_list ~pp_sep:(fun fmt () -> F.fprintf fmt "\n")
  (fun fmt d -> F.fprintf fmt "%a" pp_def d)
  fmt
  dl

let pp fmt p =
  match p with
  | Program (dl, sl) -> (pp_def_list fmt dl; pp_stmt_list fmt sl)
