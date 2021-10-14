module Lambda = struct
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
  
  let rec b_redux arg body lv remove = match body with
    | Var id -> if id = lv then (arg, true) else ((Var id), false)
    | Abs t1 ->
      let t1, removed_lambda = b_redux arg t1 (lv+1) false in
      if remove then (t1, true) else ((Abs t1), removed_lambda)
    | App (t1, t2) ->      
      let t1, t1_removed = b_redux arg t1 lv false in 
      let t2, t2_removed = b_redux arg t2 lv false in
      App(t1, t2), (t1_removed || t2_removed)

  let rec exec ?(is_lazy = true) expr = match expr with
    | Var id -> Var id
    | Abs t1 -> Abs (exec ~is_lazy:is_lazy t1)
    | App (t1, t2) ->
      let t1 = exec ~is_lazy:is_lazy t1 in
      let t2 = if is_lazy then t2 else (exec ~is_lazy:is_lazy t2) in  (* <- Se for verdade é lazy *)
      let redux, removed_lambda = b_redux t2 t1 (-1) true in

      if removed_lambda then exec ~is_lazy:is_lazy redux else App(t1, t2)     
end