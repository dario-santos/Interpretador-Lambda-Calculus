type lambda = 
  | Var of int 
  | Abs of lambda 
  | App of lambda * lambda

val string_of_lambda : lambda -> string

val exec : ?is_lazy:bool -> lambda -> lambda
