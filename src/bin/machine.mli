module type BetaReduction =
sig
  val can_reduce : Lambda.lambda -> bool
  val b_reduce : Lambda.lambda -> Lambda.lambda -> Lambda.lambda
  val exec : Lambda.lambda -> Lambda.lambda
end

module type Cps =
sig
  val to_cps : Lambda.lambda -> Lambda.lambda
end

module MakeMachine :
functor (B : BetaReduction) (C : Cps) ->
  sig
    val string_of_lambda : Lambda.lambda -> string
    val to_cps : Lambda.lambda -> Lambda.lambda
    val exec : Lambda.lambda -> Lambda.lambda
  end