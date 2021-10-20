type lambda =
  | Var of int
  | Abs of lambda
  | App of lambda * lambda

module type LambdaMachine =
sig
  val string_of_lambda : lambda -> string
  
  val can_reduce : lambda -> bool
  val b_reduce : lambda -> lambda -> lambda
  
  val to_cps : lambda -> lambda
end

module Machine = struct
  let rec string_of_lambda = function 
    | Var id -> string_of_int id
    | Abs t1 -> "(λ."^(string_of_lambda t1)^")"
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


  let to_cps expr =
    let rec aux expr k_cnt = match expr with
    | Var id -> Abs(App(Var 0, Var (id+k_cnt+1)))
    | Abs t  ->
      let t = aux t (k_cnt+1)in
      Abs(App(Var 0, Abs t))
    | App(f, e) ->
      let f = aux f (k_cnt+1) in
      let e = aux e (k_cnt+1) in
      Abs(App(f, Abs(App(e, Abs(App(App(Var 1, Var 0), Var 2))))))
    in
    aux expr (-1)
end

module Make_Eager(L : LambdaMachine) = 
struct
  let string_of_lambda = L.string_of_lambda
  let to_cps = L.to_cps

  let rec exec = function
  | Var id -> Var id
  | Abs t1 -> Abs (exec t1)
  | App (t1, t2) ->
    let t1 = exec t1 in
    let t2 = exec t2 in
    let tr = L.b_reduce t2 t1 in
    
    if L.can_reduce t1 then exec tr else App(t1, t2) 
end

module Make_Lazy(L : LambdaMachine) = 
struct
  let string_of_lambda = L.string_of_lambda
  let to_cps = L.to_cps

  let rec exec = function
  | Var id -> Var id
  | Abs t1 -> Abs (exec t1)
  | App (t1, t2) ->
    let t1 = exec t1 in
    let tr = L.b_reduce t2 t1 in
    
    if L.can_reduce t1 then exec tr else App(t1, t2) 
end

module MachineEager = Make_Eager(Machine)

module MachineLazy = Make_Lazy(Machine)

