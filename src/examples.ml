(* Teste infinito
let t5 = App(Abs(App(Var 0, Var 0)), Abs(App(Var 0, Var 0)))
let v5 = exec t5
let () = Printf.printf "%s\n\n" (string_of_lambda v5)
*)

(* Teste infinito em eager
let t6 = App(App(Abs(Abs(Var 1)), Abs(Var 0)), App(Abs(App(Var 0, Var 0)), Abs(App(Var 0, Var 0))))
*)
