type lambda =
  | Var of int
  | Abs of lambda
  | App of lambda * lambda

module MachineEager : 
sig 
  val string_of_lambda : lambda -> string
  
  val to_cps : lambda -> lambda
  
  val exec : lambda -> lambda
end

module MachineLazy : 
sig 
  val string_of_lambda : lambda -> string
  
  val to_cps : lambda -> lambda
  
  val exec : lambda -> lambda
end