(* ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: *)

(* let calc (operator: char) (x: int) (y: int): int *)
let calc operator x y =
  if operator = '+' then 
    x + y
  else if operator = '-' then 
    x - y
  else if operator = '*' then 
    x * y
  else if operator = '/' then 
    if y = 0 then
      failwith "Divide-by-Zero"
    else 
      x / y
  else
    failwith "Unsupported operation"

(* ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: *)

let%test "test_add" = calc '+' 1 2 = 3
let%test "test_sub" = calc '-' 1 2 = -1
let%test "test_mul" = calc '*' 3 4 = 12

let%test "test_div" =
  try let _ = calc '/' 1 0 in false
  with
    | Failure msg -> msg = "Divide-by-Zero"
    | _ -> false

let%test "test_mod" =
  try let _ = calc '%' 8 5 in false
  with
    | Failure msg -> msg = "Unsupported operation"
    | _ -> false

(* ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: *)