let%test _ = Hw4.interp (Num 42) = (NumV 42)
let%test _ = Hw4.interp (Add (Num 2, Num 3)) = (NumV 5)
let%test _ = Hw4.interp (Sub (Num 3, Num 2)) = (NumV 1)
let%test _ = Hw4.interp (Add (Num 1, Add (Num 2, Num 3))) = (NumV 6)