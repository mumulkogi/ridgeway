(* ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: *)

module HwX = Hw13
module HwXt = Hw13_tc

(* [`HwX.interp_expr`] ::::::::::::::::::::::::::::::::::::::::::::::::::::: *)

let%test "HwX_interp_expr_add_00" = HwX.interp_expr 
  (Ast.Add (Ast.Num 1, Ast.Num 2)) ([], [])
  = Value.NumV 3

let%test "HwX_interp_expr_add_01" = 
  try
    let _ = HwX.interp_expr 
      (Ast.Add (Ast.Num 1, Ast.Bool true)) ([], [])
    in false
  with
    | Failure msg -> msg = "Not a number: 1 + true"

(* ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: *)

let%test "HwX_interp_expr_sub_00" = HwX.interp_expr 
  (Ast.Sub (Ast.Num 1, Ast.Num 2)) ([], [])
  = Value.NumV (-1)

let%test "HwX_interp_expr_sub_01" = 
  try
    let _ = HwX.interp_expr 
      (Ast.Sub (Ast.Num 1, Ast.Bool true)) ([], [])
    in false
  with
    | Failure msg -> msg = "Not a number: 1 - true"

(* ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: *)

let%test "HwX_interp_expr_lt_00" = HwX.interp_expr 
  (Ast.Lt (Ast.Num 1, Ast.Num 2)) ([], [])
  = Value.BoolV true

let%test "HwX_interp_expr_lt_01" = 
  try
    let _ = HwX.interp_expr 
      (Ast.Lt (Ast.Num 1, Ast.Bool true)) ([], [])
    in false
  with
    | Failure msg -> msg = "Not a number: 1 < true"

(* ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: *)

let%test "HwX_interp_expr_gt_00" = HwX.interp_expr 
  (Ast.Gt (Ast.Num 1, Ast.Num 2)) ([], [])
  = Value.BoolV false

let%test "HwX_interp_expr_gt_01" = 
  try
    let _ = HwX.interp_expr 
      (Ast.Gt (Ast.Num 1, Ast.Bool true)) ([], [])
    in false
  with
    | Failure msg -> msg = "Not a number: 1 > true"

(* ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: *)

let%test "HwX_interp_expr_eq_00" = HwX.interp_expr 
  (Ast.Eq (Ast.Num 1, Ast.Num 1)) ([], [])
  = Value.BoolV true

let%test "HwX_interp_expr_eq_01" = HwX.interp_expr 
  (Ast.Eq (Ast.Num 1, Ast.Num 2)) ([], []) 
  = Value.BoolV false

let%test "HwX_interp_expr_eq_02" = HwX.interp_expr 
  (Ast.Eq (Ast.Num 1, Ast.Bool true)) ([], [])
  = Value.BoolV false

let%test "HwX_interp_expr_eq_03" = HwX.interp_expr 
  (Ast.Eq (Ast.Bool false, Ast.Bool true)) ([], [])
  = Value.BoolV false

let%test "HwX_interp_expr_eq_04" = HwX.interp_expr 
  (Ast.Eq (Ast.Ref "x", Ast.Ref "y")) ([("x", 0xDEAD); ("y", 0xC0DE)], [])
  = Value.BoolV false

(* ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: *)

let%test "HwX_interp_expr_and_00" = HwX.interp_expr 
  (Ast.And (Ast.Bool true, Ast.Bool true)) ([], [])
  = Value.BoolV true

let%test "HwX_interp_expr_and_01" = HwX.interp_expr 
  (Ast.And (Ast.Bool true, Ast.Bool false)) ([], [])
  = Value.BoolV false

let%test "HwX_interp_expr_and_02" = HwX.interp_expr 
  (Ast.And (Ast.Bool false, Ast.Bool true)) ([], [])
  = Value.BoolV false

let%test "HwX_interp_expr_and_03" = HwX.interp_expr 
  (Ast.And (Ast.Bool false, Ast.Bool false)) ([], [])
  = Value.BoolV false

let%test "HwX_interp_expr_and_04" = 
  try
    let _ = HwX.interp_expr 
      (Ast.And (Ast.Num 1, Ast.Bool true)) ([], [])
    in false
  with
    | Failure msg -> msg = "Not a bool: 1 && true"

let%test "HwX_interp_expr_and_05" = 
  try
    let _ = HwX.interp_expr 
      (Ast.And (Ast.Bool true, Ast.Num 1)) ([], [])
    in false
  with
    | Failure msg -> msg = "Not a bool: true && 1"

(* ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: *)

let%test "HwX_interp_expr_or_00" = HwX.interp_expr 
  (Ast.Or (Ast.Bool true, Ast.Bool true)) ([], [])
  = Value.BoolV true

let%test "HwX_interp_expr_or_01" = HwX.interp_expr 
  (Ast.Or (Ast.Bool true, Ast.Bool false)) ([], [])
  = Value.BoolV true

let%test "HwX_interp_expr_or_02" = HwX.interp_expr 
  (Ast.Or (Ast.Bool false, Ast.Bool true)) ([], [])
  = Value.BoolV true

let%test "HwX_interp_expr_or_03" = HwX.interp_expr 
  (Ast.Or (Ast.Bool false, Ast.Bool false)) ([], [])
  = Value.BoolV false

let%test "HwX_interp_expr_or_04" = 
  try
    let _ = HwX.interp_expr 
      (Ast.Or (Ast.Num 1, Ast.Bool true)) ([], [])
    in false
  with
    | Failure msg -> msg = "Not a bool: 1 || true"

let%test "HwX_interp_expr_or_05" = 
  try
    let _ = HwX.interp_expr 
      (Ast.Or (Ast.Bool false, Ast.Num 1)) ([], [])
    in false
  with
    | Failure msg -> msg = "Not a bool: false || 1"

(* [`HwX.interp_prog`] ::::::::::::::::::::::::::::::::::::::::::::::::::::: *)

let%test "HwX_interp_prog_00" = HwX.interp_prog (
  ParserMain.parse 
"               \
def x: int = 3; \
"
  ) = 
  (
    [("x", 0)],
    [(0, Value.NumV 3)]
  )

let%test "HwX_interp_prog_01" = HwX.interp_prog (
  ParserMain.parse 
"               \
def x: int = 3; \
def y: int = 4; \
"
  ) = 
  (
    [("y", 1); ("x", 0)],
    [(1, Value.NumV 4); (0, Value.NumV 3); ]
  )

let%test "HwX_interp_prog_02" = HwX.interp_prog (
  ParserMain.parse
"               \
def x: int = 3; \
def y: int = x; \
"
  ) = 
  (
    [("y", 1); ("x", 0)],
    [(1, Value.NumV 3); (0, Value.NumV 3); ]
  )

let%test "HwX_interp_prog_03" = 
  try
    let _ = HwX.interp_prog (
      ParserMain.parse 
"                   \
def x: int = 3;     \
def y: bool = true; \
                    \
z = x && y;         \
"
      ) in false
  with
    | Failure msg -> msg = "Free identifier: z"

let%test "HwX_interp_prog_04" = 
  try
    let _ = HwX.interp_prog (
      ParserMain.parse 
"                     \
def x: int = 3;       \
def y: bool = true;   \
                      \
def z: bool = x && y; \
"
      ) in false
  with
    | Failure msg -> msg = "Not a bool: x && y"

let%test "HwX_interp_prog_05" = HwX.interp_prog (
  ParserMain.parse 
"               \
def x: int = 3; \
                \
x = x + 1;      \
"
  ) = 
  (
    [("x", 0)],
    [(0, Value.NumV 4); ]
  )

let%test "HwX_interp_prog_06" = 
  try
    let _ = HwX.interp_prog (
      ParserMain.parse 
"               \
def x: int = 0; \
                \
x = *1;         \
"
    ) in false
  with
    | Failure msg -> msg = "Not an address: 1"

let%test "HwX_interp_prog_07" = 
  try
    let _ = HwX.interp_prog (
      ParserMain.parse 
"       \
*1 = 2; \
"
    ) in false
  with
    | Failure msg -> msg = "Not an address: 1"

let%test "HwX_interp_prog_08" = HwX.interp_prog (
  ParserMain.parse 
"               \
def x: int = 3; \
def y: int = 2; \
                \
x = *(&y);      \
"
  ) = 
  (
    [("y", 1); ("x", 0)],
    [(0, Value.NumV 2); (1, Value.NumV 2)]
  )

let%test "HwX_interp_prog_09" = HwX.interp_prog (
  ParserMain.parse 
"               \
def x: int = 1; \
def y: int = 2; \
                \
if (x < y) {    \
  x = y;        \
}               \
" 
  ) =
  (
    [("y", 1); ("x", 0)],
    [(0, Value.NumV 2); (1, Value.NumV 2)]
  )

let%test "HwX_interp_prog_10" = HwX.interp_prog (
  ParserMain.parse 
"               \
def x: int = 1; \
def y: int = 2; \
                \
if (x > y) {    \
  x = y;        \
} else {        \
  y = x;        \
}               \
" 
  ) =
  (
    [("y", 1); ("x", 0)],
    [(1, Value.NumV 1); (0, Value.NumV 1)]
  )

let%test "HwX_interp_prog_11" = 
  try
    let _ = HwX.interp_prog (
      ParserMain.parse 
"        \
if (0) { \
}        \
"
    ) in false
  with
    | Failure msg -> msg = "Not a bool: 0"

let%test "HwX_interp_prog_12" = HwX.interp_prog (
  ParserMain.parse 
"               \
def x: int = 5; \
def y: int = 0; \
                \
while (x < 0) { \
  y = y + x;    \
  x = x - 1;    \
}               \
"
  ) =
    (
      [("y", 1); ("x", 0)],
      [(1, Value.NumV 0); (0, Value.NumV 5);]
    )

let%test "HwX_interp_prog_13" = HwX.interp_prog (
  ParserMain.parse 
"               \
def x: int = 5; \
def y: int = 0; \
                \
while (x > 0) { \
  y = y + x;    \
  x = x - 1;    \
}               \
"
  ) =
  (
    [("y", 1); ("x", 0)],
    [(0, Value.NumV 0); (1, Value.NumV 15);]
  )

let%test "HwX_interp_prog_14" = 
  try
    let _ = HwX.interp_prog (
      ParserMain.parse 
"               \
def x: int = 0; \
                \
while (0) {     \
  x = 1;        \
}               \
"
  ) in false
  with
    | Failure msg -> msg = "Not a bool: 0"

let%test "HwX_interp_prog_15" = HwX.interp_prog (
  ParserMain.parse 
"                    \
fun f(a: int): int { \
  return a + 1;      \
}                    \
                     \
def x: int = 0;      \
                     \
x = f(1);            \
"
  ) = 
  (
    [("x", 0)],
    [(0, Value.NumV 2); (-1, Value.NumV 2); (1, Value.NumV 1)]
  )

let%test "HwX_interp_prog_16" = 
  try
    let _ = HwX.interp_prog (
      ParserMain.parse 
"                    \
fun f(a: int): int { \
  return a + 1;      \
}                    \
                     \
def x: int = 0;      \
                     \
x = f(1, 2);         \
"
    ) in false
  with
    | Failure msg -> msg = 
      "The number of arguments not matched: actual 2, expected 1"

let%test "HwX_interp_prog_17" = 
  try
    let _ = HwX.interp_prog (
      ParserMain.parse 
"               \
fun f(): int {  \
}               \
                \
def x: int = 0; \
                \
x = f();        \
"
    ) in false
  with
    | Failure msg -> msg = "Free address: -1"

let%test "HwX_interp_prog_18" = 
  try
    let _ = HwX.interp_prog (
      ParserMain.parse 
"               \
def x: int = 0; \
                \
x = f(1);       \
"
    ) in false
  with
    | Failure msg -> msg = "Unbound function: f"

let%test "HwX_interp_prog_19" = HwX.interp_prog (
  ParserMain.parse 
"               \
fun f(): int {  \
  return 99;    \
}               \
                \
def x: int = 0; \
                \
x = f();        \
"
  ) = 
  (
    [("x", 0)],
    [(0, Value.NumV 99); (-1, Value.NumV 99);]
  )

let%test "HwX_interp_prog_19" = HwX.interp_prog (
  ParserMain.parse 
"               \
fun f(): int {  \
  return 99;    \
}               \
                \
def x: int = 0; \
                \
x = f();        \
"
  ) = 
  (
    [("x", 0)],
    [(0, Value.NumV 99); (-1, Value.NumV 99);]
  )

(*
- : Hw13_test__.Env.t * Hw13_test__.Mem.t =
([("x", 0)], [(0, Hw13_test__.Value.NumV 10)])
*)

let%test "HwX_interp_prog_20" =
  try
    let _ = HwX.interp_prog (
      ParserMain.parse 
"               \
def x: int = 0; \
                \
x = input();    \
"
    ) in false
  with
    | End_of_file -> true
    | _ -> false

(* [`HwXt.tc_expr`] :::::::::::::::::::::::::::::::::::::::::::::::::::::::: *)

let%test "HwXt_tc_expr_ref_00" = HwXt.tc_expr 
  (Ast.Ref "x") ([("x", Ast.TInt)])
  = Ast.TPtr (Ast.TInt)

let%test "HwXt_tc_expr_ref_01" = 
  try
    let _ = HwXt.tc_expr 
      (Ast.Ref "x") ([])
    in false
  with
    | Failure msg -> msg = "[Ill-typed] x"

(* ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: *)

let%test "HwXt_tc_expr_add_00" = HwXt.tc_expr 
  (Ast.Add (Ast.Num 1, Ast.Num 2)) ([])
  = Ast.TInt

let%test "HwXt_tc_expr_add_01" = HwXt.tc_expr 
  (Ast.Add (Ast.Id "x", Ast.Num 1)) ([("x", Ast.TInt)])
  = Ast.TInt

(* ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: *)

let%test "HwXt_tc_expr_sub_00" = HwXt.tc_expr 
  (Ast.Sub (Ast.Num 1, Ast.Num 2)) ([])
  = Ast.TInt

let%test "HwXt_tc_expr_sub_01" = 
  try
    let _ = HwXt.tc_expr 
      (Ast.Sub (Ast.Id "x", Ast.Num 1)) ([("x", Ast.TBool)])
    in false
  with
    | Failure msg -> msg = "[Ill-typed] x - 1"

(* ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: *)

let%test "HwXt_tc_expr_lt_00" = HwXt.tc_expr 
  (Ast.Lt (Ast.Num 1, Ast.Num 2)) ([])
  = Ast.TBool


let%test "HwXt_tc_expr_lt_01" = 
  try
    let _ = HwXt.tc_expr 
      (Ast.Lt (Ast.Id "x", Ast.Num 1)) ([])
    in false
  with
    | Failure msg -> msg = "[Ill-typed] x"

(* ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: *)

let%test "HwXt_tc_expr_gt_00" = HwXt.tc_expr 
  (Ast.Gt (Ast.Num 1, Ast.Num 2)) ([])
  = Ast.TBool

let%test "HwXt_tc_expr_gt_01" = HwXt.tc_expr
  (Ast.Gt (Ast.Id "x", Ast.Id "y")) ([("x", Ast.TInt); ("y", Ast.TInt)])
  = Ast.TBool

(* ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: *)

let%test "HwXt_tc_expr_eq_00" = HwXt.tc_expr 
  (Ast.Eq (Ast.Num 1, Ast.Num 2)) ([])
  = Ast.TBool

let%test "HwXt_tc_expr_eq_01" = HwXt.tc_expr 
  (Ast.Eq (Ast.Bool true, Ast.Bool false)) ([])
  = Ast.TBool

(* ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: *)

let%test "HwXt_tc_expr_and_00" = HwXt.tc_expr 
  (Ast.And (Ast.Bool true, Ast.Bool false)) ([])
  = Ast.TBool

let%test "HwXt_tc_expr_and_01" = 
  try
    let _ = HwXt.tc_expr 
      (Ast.And (Ast.Id "x", Ast.Id "y")) ([("x", Ast.TInt); ("y", Ast.TBool)])
    in false
  with
    | Failure msg -> msg = "[Ill-typed] x && y"

(* ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: *)

let%test "HwXt_tc_expr_or_00" = HwXt.tc_expr 
  (Ast.Or (Ast.Bool true, Ast.Bool false)) ([])
  = Ast.TBool

(* [`HwXt.tc_prog`] :::::::::::::::::::::::::::::::::::::::::::::::::::::::: *)

(* ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: *)