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