module Eager : sig 
  val exec : Lambda.lambda -> Lambda.lambda 
end

module Lazy : sig 
  val exec : Lambda.lambda -> Lambda.lambda 
end