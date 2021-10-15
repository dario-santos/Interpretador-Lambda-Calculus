let l_true  = Lambda.Abs(Lambda.Abs((Lambda.Var 1)))
let l_false = Lambda.Abs(Lambda.Abs((Lambda.Var 0)))

let l_not x = Lambda.App(Lambda.App(x,l_false),l_true)
let l_or x y = Lambda.App(Lambda.App(x,l_true),y)
let l_and x y = Lambda.App(Lambda.App(x,y),l_false)


(* Integers *)
let l_zero = Lambda.Abs(Lambda.Abs((Lambda.Var 0)))
let l_succ n = Lambda.App(Lambda.Abs(Lambda.Abs(Lambda.Abs(Lambda.App(Lambda.Var 1, Lambda.App(Lambda.App(Lambda.Var 2, Lambda.Var 1), Lambda.Var 0))))),n)

let l_add m n = 
  let nfx = Lambda.App(Lambda.App(Lambda.Var 2, Lambda.Var 1), Lambda.Var 0) in
  let mfnfx = Lambda.App(Lambda.App(Lambda.Var 3, Lambda.Var 1), nfx) in  
  Lambda.App(Lambda.App(Lambda.Abs(Lambda.Abs(Lambda.Abs(Lambda.Abs(mfnfx)))), n), m)

let l_mul m n =
  let nf = Lambda.App(Lambda.Var 2, Lambda.Var 1) in
  let mfnfx = Lambda.App(Lambda.App(Lambda.Var 3, nf), Lambda.Var 0) in  
  Lambda.App(Lambda.App(Lambda.Abs(Lambda.Abs(Lambda.Abs(Lambda.Abs(mfnfx)))), n), m)

let l_exp m n =
  let mn = Lambda.App(Lambda.Var 3, Lambda.Var 2) in
  let mfnfx = Lambda.App(Lambda.App(mn, Lambda.Var 1), Lambda.Var 0) in  
  Lambda.App(Lambda.App(Lambda.Abs(Lambda.Abs(Lambda.Abs(Lambda.Abs(mfnfx)))), n), m)
  

let test_expression expr =
  let v = Lambda.exec ~is_lazy:false expr in
  Printf.printf "%s\n\n" (Lambda.string_of_lambda v)

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
let t5 = Lambda.App(Lambda.Abs(Lambda.App(Lambda.Var 0, Lambda.Var 0)), Lambda.Abs(Lambda.App(Lambda.Var 0, Lambda.Var 0)))
let v5 = Lambda.exec t5
let () = Printf.printf "%s\n\n" (Lambda.string_of_lambda v5)
*)

(* Teste infinito em eager *)
(*
let t6 = Lambda.App(Lambda.App(Lambda.Abs(Lambda.Abs(Lambda.Var 1)), Lambda.Abs(Lambda.Var 0)), Lambda.App(Lambda.Abs(Lambda.App(Lambda.Var 0, Lambda.Var 0)), Lambda.Abs(Lambda.App(Lambda.Var 0, Lambda.Var 0))))


let v6 = Lambda.exec ~is_lazy:(true) t6
let () = Printf.printf "%s\n\n" (Lambda.string_of_lambda v6)
*)