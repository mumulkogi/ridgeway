(* ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: *)

module HwX = Hw12

(* [`HwX.interp_expr`] ::::::::::::::::::::::::::::::::::::::::::::::::::::: *)

let%test "HwX_interp_expr_add_00" = HwX.interp_expr 
  (Ast.Add (Ast.Num 1, Ast.Num 2)) [] []
  = Value.NumV 3

let%test "HwX_interp_expr_add_01" = 
  try
    let _ = HwX.interp_expr 
      (Ast.Add (Ast.Num 1, Ast.Bool true)) [] [] 
    in false
  with
    | Failure msg -> msg = "Not a number: 1 + true"

(* ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: *)

let%test "HwX_interp_expr_sub_00" = HwX.interp_expr 
  (Ast.Sub (Ast.Num 1, Ast.Num 2)) [] []
  = Value.NumV (-1)

let%test "HwX_interp_expr_sub_01" = 
  try
    let _ = HwX.interp_expr 
      (Ast.Sub (Ast.Num 1, Ast.Bool true)) [] []
    in false
  with
    | Failure msg -> msg = "Not a number: 1 - true"

(* ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: *)

let%test "HwX_interp_expr_lt_00" = HwX.interp_expr 
  (Ast.Lt (Ast.Num 1, Ast.Num 2)) [] []
  = Value.BoolV true

let%test "HwX_interp_prog_lt_01" = 
  try
    let _ = HwX.interp_expr 
      (Ast.Lt (Ast.Num 1, Ast.Bool true)) [] []
    in false
  with
    | Failure msg -> msg = "Not a number: 1 < true"

(* ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: *)

let%test "HwX_interp_expr_gt_00" = HwX.interp_expr 
  (Ast.Gt (Ast.Num 1, Ast.Num 2)) [] []
  = Value.BoolV false

let%test "HwX_interp_prog_gt_01" = 
  try
    let _ = HwX.interp_expr 
      (Ast.Gt (Ast.Num 1, Ast.Bool true)) [] []
    in false
  with
    | Failure msg -> msg = "Not a number: 1 > true"

(* ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: *)

let%test "HwX_interp_expr_eq_00" = HwX.interp_expr 
  (Ast.Eq (Ast.Num 1, Ast.Num 1)) [] []
  = Value.BoolV true

let%test "HwX_interp_expr_eq_01" = HwX.interp_expr 
  (Ast.Eq (Ast.Num 1, Ast.Num 2)) [] [] 
  = Value.BoolV false

let%test "HwX_interp_expr_eq_02" = HwX.interp_expr 
  (Ast.Eq (Ast.Num 1, Ast.Bool true)) [] []
  = Value.BoolV false

let%test "HwX_interp_expr_eq_03" = HwX.interp_expr 
  (Ast.Eq (Ast.Bool false, Ast.Bool true)) [] []
  = Value.BoolV false

let%test "HwX_interp_expr_eq_04" = HwX.interp_expr 
  (Ast.Eq (Ast.Ref "x", Ast.Ref "y")) [("x", 0xDEADBEEF); ("y", 0xDEADC0DE)] []
  = Value.BoolV false

(* ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: *)

let%test "HwX_interp_expr_and_00" = HwX.interp_expr 
  (Ast.And (Ast.Bool true, Ast.Bool true)) [] []
  = Value.BoolV true

let%test "HwX_interp_expr_and_01" = HwX.interp_expr 
  (Ast.And (Ast.Bool true, Ast.Bool false)) [] []
  = Value.BoolV false

let%test "HwX_interp_expr_and_02" = HwX.interp_expr 
  (Ast.And (Ast.Bool false, Ast.Bool true)) [] []
  = Value.BoolV false

let%test "HwX_interp_expr_and_03" = HwX.interp_expr 
  (Ast.And (Ast.Bool false, Ast.Bool false)) [] []
  = Value.BoolV false

let%test "HwX_interp_expr_and_04" = 
  try
    let _ = HwX.interp_expr 
      (Ast.And (Ast.Num 1, Ast.Bool true)) [] []
    in false
  with
    | Failure msg -> msg = "Not a bool: 1 && true"

let%test "HwX_interp_expr_and_05" = 
  try
    let _ = HwX.interp_expr 
      (Ast.And (Ast.Bool true, Ast.Num 1)) [] []
    in false
  with
    | Failure msg -> msg = "Not a bool: true && 1"

(* ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: *)

let%test "HwX_interp_expr_or_00" = HwX.interp_expr 
  (Ast.Or (Ast.Bool true, Ast.Bool true)) [] []
  = Value.BoolV true

let%test "HwX_interp_expr_or_01" = HwX.interp_expr 
  (Ast.Or (Ast.Bool true, Ast.Bool false)) [] []
  = Value.BoolV true

let%test "HwX_interp_expr_or_02" = HwX.interp_expr 
  (Ast.Or (Ast.Bool false, Ast.Bool true)) [] [] 
  = Value.BoolV true

let%test "HwX_interp_expr_or_03" = HwX.interp_expr 
  (Ast.Or (Ast.Bool false, Ast.Bool false)) [] [] 
  = Value.BoolV false

let%test "HwX_interp_expr_or_04" = 
  try
    let _ = HwX.interp_expr 
      (Ast.Or (Ast.Num 1, Ast.Bool true)) [] []
    in false
  with
    | Failure msg -> msg = "Not a bool: 1 || true"

let%test "HwX_interp_expr_or_05" = 
  try
    let _ = HwX.interp_expr 
      (Ast.Or (Ast.Bool false, Ast.Num 1)) [] [] 
    in false
  with
    | Failure msg -> msg = "Not a bool: false || 1"

(* ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: *)