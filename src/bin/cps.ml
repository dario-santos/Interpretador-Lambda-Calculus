open Lambda

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
  let rec update lvl expr = match expr with
   | Var id -> Var (id + (if id > lvl then 1 else 0))
   | Abs m -> Abs(update (lvl+1) m)
   | App (m, n) -> 
    let m = update lvl m in
    let n = update lvl n in
    App (m, n)
  
  let to_cps expr =
    let rec aux = function
    | Var id -> Var id
    | Abs m  ->
      let m = aux m in
      Abs(App(Var 0, Abs (update 0 m)))
    | App(m, n) ->
      let m = aux m in
      let n = aux n in
      Abs(App(update (-1) m, Abs(App(App(Var 0, update 0 (update (-1) n)), Var 1))))
    in
    aux expr
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
