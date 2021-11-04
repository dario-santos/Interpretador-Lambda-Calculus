type lambda =
  | Var of int
  | Abs of lambda
  | App of lambda * lambda


module type BetaReduction =
sig
  val can_reduce : lambda -> bool
  val b_reduce : lambda -> lambda -> lambda
  val exec : lambda -> lambda
end

module type Cps =
sig
  val to_cps : lambda -> lambda
end


module BetaEager = struct
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

  let rec exec = function
    | Var id -> Var id
    | Abs t1 -> Abs (exec t1)
    | App (t1, t2) ->
      let t1 = exec t1 in
      let t2 = exec t2 in
      let tr = b_reduce t2 t1 in
      
      if can_reduce t1 then exec tr else App(t1, t2) 

end


module BetaLazy = struct
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

  let rec exec = function
  | Var id -> Var id
  | Abs t1 -> Abs (exec t1)
  | App (t1, t2) ->
    let t1 = exec t1 in
    let tr = b_reduce t2 t1 in
    
    if can_reduce t1 then exec tr else App(t1, t2)
end

module FisherPlotkin = struct
  (* Transformação de Fischer e Plotkin *)
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


module Plotkin = struct
  (* Transformação de Plotkin *)
  let to_cps expr =
    let rec aux expr k_cnt = match expr with
    | Var id -> Var (id+k_cnt)
    | Abs m  ->
      let m = aux m (k_cnt+1) in
      Abs(App(Var 0, Abs m))
    | App(m, n) ->
      let m = aux m (k_cnt+1) in
      let n = aux n (k_cnt+1) in
      Abs(App(m, Abs(App(App(Var 0, n), Var 1))))
    in
    aux expr (-1)
end


module OnePassTail = struct
  (* Primeiro passo da transformação de Fischer *)
  let to_cps expr =
    let rec aux expr k_cnt = match expr with
    | Var id -> Var (id+k_cnt)
    | Abs t  ->
      let t = aux t (k_cnt+2) in
      Abs(App(Var 0, Abs (Abs (App (t, Var 1)))))
    | App(m, n) ->
      let m = aux m (k_cnt+1) in
      let n = aux n (k_cnt+1) in
      Abs(App(m, Abs(App(App(Var 0, Var 1), n))))
    in
    aux expr (-1)
end

module MakeMachine(B : BetaReduction) (C : Cps) = 
struct
  let rec string_of_lambda = function 
  | Var id -> string_of_int id
  | Abs t1 -> "(λ."^(string_of_lambda t1)^")"
  | App (t1,t2) -> 
    let t1 = string_of_lambda t1 in 
    let t2 = string_of_lambda t2 in
    "(@"^t1^" "^t2^")"

  let to_cps = C.to_cps

  let exec = B.exec
end

module MachineOnePass = MakeMachine (BetaEager) (OnePassTail)

module MachinePlotkin = MakeMachine (BetaEager) (Plotkin)
