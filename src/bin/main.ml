open Lambda

let l_true  = Abs(Abs(Var 1))
let l_false = Abs(Abs(Var 0))

let l_not x = App(App(x,l_false),l_true)
let l_or x y = App(App(x,l_true),y)
let l_and x y = App(App(x,y),l_false)


(* Integers *)
let l_zero = Abs(Abs((Var 0)))
let l_succ n = App(Abs(Abs(Abs(App(Var 1, App(App(Var 2, Var 1), Var 0))))),n)

let l_add m n = 
  let nfx = App(App(Var 2, Var 1), Var 0) in
  let mfnfx = App(App(Var 3, Var 1), nfx) in  
  App(App(Abs(Abs(Abs(Abs(mfnfx)))), n), m)

let l_mul m n =
  let nf = App(Var 2, Var 1) in
  let mfnfx = App(App(Var 3, nf), Var 0) in  
  App(App(Abs(Abs(Abs(Abs(mfnfx)))), n), m)

let l_exp m n =
  let mn = App(Var 3, Var 2) in
  let mfnfx = App(App(mn, Var 1), Var 0) in  
  App(App(Abs(Abs(Abs(Abs(mfnfx)))), n), m)
  

let test_expression expr =
  let v = MachineEager.exec expr in
  Printf.printf "%s\n\n" (MachineEager.string_of_lambda v)

let () = Printf.printf "\n\n*** Integer Tests ***\n"

let () = test_expression (l_succ l_zero)
  
let () = test_expression (l_succ (l_succ (l_succ (l_succ (l_succ (l_succ l_zero))))))

let () = 
  let l_three = (l_succ(l_succ (l_succ l_zero))) in
  let l_five = l_succ(l_succ(l_succ(l_succ (l_succ l_zero)))) in
  test_expression (l_mul l_three l_five)


let () = Printf.printf "\n\n*** Boolean Tests ***\n"

let () = Printf.printf "\nAnd Truth Table:\n"
let () = test_expression (l_and l_true l_true)
let () = test_expression (l_and l_false l_true)
let () = test_expression (l_and l_true l_false)
let () = test_expression (l_and l_false l_false)

(* Teste infinito*)
(*
let t5 = App(Abs(App(Var 0, Var 0)), Abs(App(Var 0, Var 0)))
let v5 = exec t5
let () = Printf.printf "%s\n\n" (string_of_lambda v5)
*)

(* Teste infinito em eager *)
(*
let t6 = App(App(Abs(Abs(Var 1)), Abs(Var 0)), App(Abs(App(Var 0, Var 0)), Abs(App(Var 0, Var 0))))


let v6 = exec ~is_lazy:(true) t6
let () = Printf.printf "%s\n\n" (string_of_lambda v6)
*)

(*
let t6 = l_true

let v6 = MachineEager.to_cps t6
let () = Printf.printf "%s\n\n" (MachineEager.string_of_lambda v6)

let v6 = MachineEager.exec (App (v6, Abs (Var 0)))
let () = Printf.printf "%s\n\n" (MachineEager.string_of_lambda v6)



let l_two = l_succ (l_succ l_zero)
let l_three = (l_succ(l_succ (l_succ l_zero)))

let f = Abs(l_add (Var 0) (l_succ l_zero))
let f_cps = MachineEager.to_cps f

let () = Printf.printf "%s\n\n" (MachineEager.string_of_lambda f_cps)

let g x = App(App(f, (Abs (l_mul l_two (Var 0)))), x)

let vtest = MachineEager.exec (g l_three)
let () = Printf.printf "%s\n\n" (MachineEager.string_of_lambda vtest)
*)

let t8 = Abs(Abs(Abs(App(App(Var 2, Var 0), Var 1))))
let v8 = MachineEager.to_cps t8
let () = Printf.printf "%s\n\n" (MachineEager.string_of_lambda v8)