open Lambda

(* Booleans*)
let l_true  = Abs(Abs(Var 1))
let l_false = Abs(Abs(Var 0))

let l_not x = App(App(x,l_false),l_true)
let l_or x y = App(App(x,l_true),y)
let l_and x y = App(App(x,y), l_false)

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

module MachineFischer = Machine.MakeMachine (Beta_redux.Eager) (Cps.Fischer)

module MachinePlotkin = Machine.MakeMachine (Beta_redux.Eager) (Cps.Plotkin)

module MachineFischerPlotkin = Machine.MakeMachine (Beta_redux.Eager) (Cps.FischerPlotkin)


module MachineOnePass = Machine.MakeMachine (Beta_redux.Eager) (Cps.OnePassTail)


let tests = [
  Abs(Var 0); 
  l_true; 
  l_and l_true l_true;
  l_and l_true (l_or l_false l_true);
  App(App(Abs(l_add (Var 0) (l_succ l_zero)), (Abs (l_mul (l_succ (l_succ l_zero)) (Var 0)))), l_succ(l_succ (l_succ l_zero)));
  l_mul (l_succ(l_succ (l_succ l_zero))) (l_succ(l_succ(l_succ(l_succ (l_succ l_zero)))));
  l_succ (l_succ (l_succ (l_succ (l_succ (l_succ l_zero)))));
  l_succ l_zero
]

let machines = [(MachinePlotkin.to_cps, "Plotkin"); MachineFischer.to_cps, "Fischer"; (MachineFischerPlotkin.to_cps, "Fischer&Plotkin"); MachineOnePass.to_cps, "OnePass"]

let benchmark_machine machine t =
  let rec string_of_lambda = function 
    | Var id -> string_of_int id
    | Abs t1 -> string_of_lambda t1
    | App (t1,t2) -> (string_of_lambda t1) ^ (string_of_lambda t2)
  in

  let start_time = Sys.time () in
  let expr = machine t in
  let finish_time = Sys.time () in
  let time = finish_time -. start_time in
  
  let sz = String.length (string_of_lambda expr) in

  Printf.sprintf "%f,  %d\n" time sz

let _ =
  List.iter(fun (m, n) -> 
    Printf.printf "\n%s:\n" n;
    Printf.printf " #, time, size\n";

    List.iteri(fun i t ->
      Printf.printf " %d, %s" i (benchmark_machine m t)
    ) tests
  ) machines
  