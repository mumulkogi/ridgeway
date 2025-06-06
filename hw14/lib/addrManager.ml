(* int *i = malloc(sizeof *i); *i = 0; *)
let i = ref 0

let new_addr () =
  (* int *addr = &i; *)
  let addr = !i in
  let _ = i := addr + 1 in
  addr

let init () = i := 0

let ret_addr = -1
