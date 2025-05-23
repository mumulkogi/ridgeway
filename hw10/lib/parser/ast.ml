module F = Format

type expr = Num of int
  | Bool of bool
  | Id of string
  | Add of expr * expr
  | Sub of expr * expr
  | LetIn of string * expr * expr
  | RLetIn of string * expr * expr
  | App of expr * expr
  | Lambda of string * expr
  | Cond of expr * expr * expr
  | LessThan of expr * expr

let true_encoding = Lambda ("x", Lambda ("y", Id "x"))

let false_encoding = Lambda ("x", Lambda ("y", Id "y"))

let rec pp fmt e = 
  match e with
  | Num n -> F.fprintf fmt "%d" n
  | Bool b -> F.fprintf fmt "%b" b
  | Id id -> F.fprintf fmt "%s" id
  | Add (e1, e2) -> F.fprintf fmt "%a + %a" pp e1 pp e2
  | Sub (e1, e2) -> F.fprintf fmt "%a - %a" pp e1 pp e2
  | LetIn (x, e1, e2) -> F.fprintf fmt "let %s = %a in %a" x pp e1 pp e2
  | RLetIn (x, e1, e2) -> F.fprintf fmt "let rec %s = %a in %a" x pp e1 pp e2
  | App (e1, e2) -> F.fprintf fmt "%a %a" pp e1 pp e2
  | Lambda (x, e) -> F.fprintf fmt "(fun %s -> %a)" x pp e 
  | Cond (e1, e2, e3) -> F.fprintf fmt "(%a? %a : %a)" pp e1 pp e2 pp e3
  | LessThan (e1, e2) -> F.fprintf fmt "%a < %a" pp e1 pp e2

let rec pp_ast fmt e = 
  match e with
  | Num n -> F.fprintf fmt "(Num %d)" n
  | Bool b -> F.fprintf fmt "(Bool %b)" b
  | Id id -> F.fprintf fmt "(Id %s)" id
  | Add (e1, e2) -> F.fprintf fmt "(Add (%a, %a))" pp_ast e1 pp_ast e2
  | Sub (e1, e2) -> F.fprintf fmt "(Sub (%a, %a))" pp_ast e1 pp_ast e2
  | LetIn (x, e1, e2) -> F.fprintf fmt "(LetIn (%s, %a, %a))" x pp_ast e1 pp_ast e2
  | RLetIn (x, e1, e2) -> F.fprintf fmt "(RLetIn (%s, %a, %a))" x pp_ast e1 pp_ast e2
  | App (e1, e2) -> F.fprintf fmt "(App (%a, %a))" pp_ast e1 pp_ast e2
  | Lambda (x, e) -> F.fprintf fmt "(Lambda (%s, %a))" x pp_ast e 
  | Cond (e1, e2, e3) -> F.fprintf fmt "(Cond (%a, %a, %a)" pp e1 pp e2 pp e3
  | LessThan (e1, e2) -> F.fprintf fmt "(LessThan (%a, %a))" pp_ast e1 pp_ast e2
