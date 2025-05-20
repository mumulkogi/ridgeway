(* ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: *)

module HwX = Hw11

(* [`HwX.interp_expr`] ::::::::::::::::::::::::::::::::::::::::::::::::::::: *)

let%test "HwX_interp_expr_add_00" = HwX.interp_expr 
  (Ast.Add (Ast.Num 1, Ast.Num 2)) [] 
  = Store.NumV 3

let%test "HwX_interp_prog_add_01" = 
  try
    let _ = HwX.interp_expr 
      (Ast.Add (Ast.Num 1, Ast.Bool true)) [] 
    in false
  with
    | Failure msg -> msg = "Not a number: 1 + true"

(* ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: *)

let%test "HwX_interp_expr_sub_00" = HwX.interp_expr 
  (Ast.Sub (Ast.Num 1, Ast.Num 2)) [] 
  = Store.NumV (-1)

let%test "HwX_interp_prog_sub_01" = 
  try
    let _ = HwX.interp_expr 
      (Ast.Sub (Ast.Num 1, Ast.Bool true)) [] 
    in false
  with
    | Failure msg -> msg = "Not a number: 1 - true"

(* ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: *)

let%test "HwX_interp_expr_lt_00" = HwX.interp_expr 
  (Ast.Lt (Ast.Num 1, Ast.Num 2)) [] 
  = Store.BoolV true

let%test "HwX_interp_prog_lt_01" = 
  try
    let _ = HwX.interp_expr 
      (Ast.Lt (Ast.Num 1, Ast.Bool true)) [] 
    in false
  with
    | Failure msg -> msg = "Not a number: 1 < true"

(* ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: *)

let%test "HwX_interp_expr_gt_00" = HwX.interp_expr 
  (Ast.Gt (Ast.Num 1, Ast.Num 2)) [] 
  = Store.BoolV false

let%test "HwX_interp_prog_gt_01" = 
  try
    let _ = HwX.interp_expr 
      (Ast.Gt (Ast.Num 1, Ast.Bool true)) [] 
    in false
  with
    | Failure msg -> msg = "Not a number: 1 > true"

(* ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: *)

let%test "HwX_interp_expr_eq_00" = HwX.interp_expr 
  (Ast.Eq (Ast.Num 1, Ast.Num 1)) [] 
  = Store.BoolV true

let%test "HwX_interp_expr_eq_01" = HwX.interp_expr 
  (Ast.Eq (Ast.Num 1, Ast.Num 2)) [] 
  = Store.BoolV false

let%test "HwX_interp_expr_eq_02" = HwX.interp_expr 
  (Ast.Eq (Ast.Num 1, Ast.Bool true)) [] 
  = Store.BoolV false

let%test "HwX_interp_expr_eq_03" = HwX.interp_expr 
  (Ast.Eq (Ast.Bool false, Ast.Bool true)) [] 
  = Store.BoolV false

(* ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: *)

let%test "HwX_interp_expr_and_00" = HwX.interp_expr 
  (Ast.And (Ast.Bool true, Ast.Bool true)) [] 
  = Store.BoolV true

let%test "HwX_interp_expr_and_01" = HwX.interp_expr 
  (Ast.And (Ast.Bool true, Ast.Bool false)) [] 
  = Store.BoolV false

let%test "HwX_interp_expr_and_02" = HwX.interp_expr 
  (Ast.And (Ast.Bool false, Ast.Bool true)) [] 
  = Store.BoolV false

let%test "HwX_interp_expr_and_03" = HwX.interp_expr 
  (Ast.And (Ast.Bool false, Ast.Bool false)) [] 
  = Store.BoolV false

let%test "HwX_interp_expr_and_04" = 
  try
    let _ = HwX.interp_expr 
      (Ast.And (Ast.Num 1, Ast.Bool true)) [] 
    in false
  with
    | Failure msg -> msg = "Not a bool: 1 && true"

let%test "HwX_interp_expr_and_05" = 
  try
    let _ = HwX.interp_expr 
      (Ast.And (Ast.Bool true, Ast.Num 1)) [] 
    in false
  with
    | Failure msg -> msg = "Not a bool: true && 1"

(* ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: *)

let%test "HwX_interp_expr_or_00" = HwX.interp_expr 
  (Ast.Or (Ast.Bool true, Ast.Bool true)) [] 
  = Store.BoolV true

let%test "HwX_interp_expr_or_01" = HwX.interp_expr 
  (Ast.Or (Ast.Bool true, Ast.Bool false)) [] 
  = Store.BoolV true

let%test "HwX_interp_expr_or_02" = HwX.interp_expr 
  (Ast.Or (Ast.Bool false, Ast.Bool true)) [] 
  = Store.BoolV true

let%test "HwX_interp_expr_or_03" = HwX.interp_expr 
  (Ast.Or (Ast.Bool false, Ast.Bool false)) [] 
  = Store.BoolV false

let%test "HwX_interp_expr_or_04" = 
  try
    let _ = HwX.interp_expr 
      (Ast.Or (Ast.Num 1, Ast.Bool true)) [] 
    in false
  with
    | Failure msg -> msg = "Not a bool: 1 || true"

let%test "HwX_interp_expr_or_05" = 
  try
    let _ = HwX.interp_expr 
      (Ast.Or (Ast.Bool false, Ast.Num 1)) [] 
    in false
  with
    | Failure msg -> msg = "Not a bool: false || 1"

(* [`HwX.interp_stmt`] ::::::::::::::::::::::::::::::::::::::::::::::::::::: *)

let%test "HwX_interp_stmt_assignstmt_00" = HwX.interp_stmt
  (Ast.AssignStmt ("x", Ast.Num 3)) []
  = [
      ("x", Store.NumV 3)
    ]

(* [`HwX.interp_prog`] ::::::::::::::::::::::::::::::::::::::::::::::::::::: *)

let%test "HwX_interp_prog_00" = HwX.interp_prog
  (ParserMain.parse "x = 3;")
  = [
      ("x", Store.NumV 3)
    ]

let%test "HwX_interp_prog_01" = HwX.interp_prog
  (ParserMain.parse "x = 1; y = 2;")
  = [
      ("y", Store.NumV 2);
      ("x", Store.NumV 1);
    ]

let%test "HwX_interp_prog_02" = 
  try
    let _ = HwX.interp_prog 
      (ParserMain.parse "x = 3; y = true; z = x && y;")
    in false
  with
    | Failure msg -> msg = "Not a bool: x && y"

let%test "HwX_interp_prog_03" = HwX.interp_prog
  (ParserMain.parse "x = 1; y = 2; if (x < y) { x = y; }")
  = [
      ("x", Store.NumV 2);
      ("y", Store.NumV 2);
    ]

let%test "HwX_interp_prog_04" = HwX.interp_prog
  (ParserMain.parse "x = 1; y = 2; if (x > y) { x = y; } else { y = x; }")
  = [
      ("y", Store.NumV 1);
      ("x", Store.NumV 1);
    ]

let%test "HwX_interp_prog_05" = 
  try
    let _ = HwX.interp_prog 
      (ParserMain.parse "x = 0; if (0) { x = 1; }")
    in false
  with
    | Failure msg -> msg = "Not a bool: 0"

let%test "HwX_interp_prog_06" = HwX.interp_prog
  (ParserMain.parse "x = 5; y = 0; while (x < 0) { y = y + x; x = x - 1; }")
  = [
      ("y", Store.NumV 0);
      ("x", Store.NumV 5);
    ]

let%test "HwX_interp_prog_07" = HwX.interp_prog
  (ParserMain.parse "x = 5; y = 0; while (x > 0) { y = y + x; x = x - 1; }")
  = [
      ("x", Store.NumV 0);
      ("y", Store.NumV 15);
    ]

let%test "HwX_interp_prog_08" = 
  try
    let _ = HwX.interp_prog 
      (ParserMain.parse "x = 0; while (0) { x = 1; }")
    in false
  with
    | Failure msg -> msg = "Not a bool: 0"

(* ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: *)