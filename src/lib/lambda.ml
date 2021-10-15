(* De Bruijn index notation *)
type lambda =
| Var of int
| Abs of lambda
| App of lambda * lambda

let rec string_of_lambda = function 
  | Var id -> string_of_int id
  | Abs t1 -> "(\\."^(string_of_lambda t1)^")"
  | App (t1,t2) -> 
    let t1 = string_of_lambda t1 in 
    let t2 = string_of_lambda t2 in
    "(@"^t1^" "^t2^")"

let rec can_reduce = function
  | Var _ -> false
  | Abs _ -> true
  | App(t1, _) -> can_reduce t1

let b_reduce arg body =
  let rec aux arg body lv remove = match body with
    | Var id -> if id = lv then arg else (Var id)
    | Abs t1 ->
      let t1 = aux arg t1 (lv+1) false in
      if remove then t1 else (Abs t1)
    | App (t1, t2) ->
      let t1 = aux arg t1 lv false in 
      let t2 = aux arg t2 lv false in
      App(t1, t2)
    in
  aux arg body (-1) true

let rec exec ?(is_lazy=true) expr = match expr with
  | Var id -> Var id
  | Abs t1 -> Abs (exec ~is_lazy t1)
  | App (t1, t2) ->
    let t1 = exec ~is_lazy t1 in
    let t2 = if is_lazy then t2 else (exec ~is_lazy t2) in  (* <- Se for verdade é lazy *)    
    let tr = b_reduce t2 t1 in
    
    if can_reduce t1 then exec ~is_lazy tr else App(t1, t2) 
