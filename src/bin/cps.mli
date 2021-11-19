(** Fischer Transformation

  [x]    : λk.@k x
  [λx.M] : λk.@k (λk.λx.@[M] k)
  [@N M] : λk.[M] (λm.@[N] (λn. @(@(m k) n))
*)
module Fischer : sig 
  val to_cps : Lambda.lambda -> Lambda.lambda
end

(** Plotkin Transformation

    [x]    : x
    [λx.M] : λk.@k (λx.[M])
    [@N M] : λk.[M] (λm.@(@(m [N]) k))
*)
module Plotkin : sig 
  val to_cps : Lambda.lambda -> Lambda.lambda 
end

(** Fischer with the Colon  Optimization of Plotkin

  [x]    : λk.@k x
  [λx.M] : λk.@k (λx.[M])
  [@N M] : λk.[M] (λm.@[N] (λn. @(@(m n) k))
*)
module FischerPlotkin : sig 
  val to_cps : Lambda.lambda -> Lambda.lambda 
end

(** Plotkin One Pass without Beta Reductions

  [x]    : λk.@k x
  [λx.M] : λk.@k (λx.λk.@[[M]] k)
  [@N M] : λk.[M] (λm.@[N] (λn. @(@(m n) (λa.@k a)))

  [[x]]    : λk.@k x
  [[λx.M]] : λk.@k (λx.λk.@[[M]] k)
  [[@N M]] : λk.[M] (λm.@[N] (λn. @(@(m n) k))
*)
module OnePassTail : sig
  val to_cps : Lambda.lambda -> Lambda.lambda
end