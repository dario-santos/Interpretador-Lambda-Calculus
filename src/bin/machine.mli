module MachineOnePass : 
sig 
  val string_of_lambda : Lambda.lambda -> string
  
  val to_cps : Lambda.lambda -> Lambda.lambda
  
  val exec : Lambda.lambda -> Lambda.lambda
end

module MachinePlotkin : 
sig 
  val string_of_lambda : Lambda.lambda -> string
  
  val to_cps : Lambda.lambda -> Lambda.lambda
  
  val exec : Lambda.lambda -> Lambda.lambda
end
