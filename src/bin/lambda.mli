type lambda =
  | Var of int
  | Abs of lambda
  | App of lambda * lambda
