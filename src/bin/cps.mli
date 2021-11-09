module FisherPlotkin : sig 
  val to_cps : Lambda.lambda -> Lambda.lambda
end

module Plotkin : sig 
  val to_cps : Lambda.lambda -> Lambda.lambda 
end

module OnePassTail : sig 
  val to_cps : Lambda.lambda -> Lambda.lambda
end