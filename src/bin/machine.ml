open Lambda

module type BetaReduction =
sig
  val can_reduce : lambda -> bool
  val b_reduce : lambda -> lambda -> lambda
  val exec : lambda -> lambda
end

module type Cps =
sig
  val to_cps : lambda -> lambda
end

module MakeMachine(B : BetaReduction) (C : Cps) = 
struct
  let rec string_of_lambda = function 
  | Var id -> string_of_int id
  | Abs t1 -> "(λ."^(string_of_lambda t1)^")"
  | App (t1,t2) -> 
    let t1 = string_of_lambda t1 in 
    let t2 = string_of_lambda t2 in
    "(@"^t1^" "^t2^")"

  let to_cps = C.to_cps

  let exec = B.exec
end

module MachineOnePass = MakeMachine (Beta_redux.Eager) (Cps.OnePassTail)

module MachinePlotkin = MakeMachine (Beta_redux.Eager) (Cps.Plotkin)
