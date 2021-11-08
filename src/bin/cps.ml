open Lambda

module FisherPlotkin = struct
  (* Transformação de Fischer e Plotkin *)
  let rec update lvl expr = match expr with
   | Var id -> Var (id + (if id > lvl then 1 else 0))
   | Abs m -> Abs(update (lvl+1) m)
   | App (m, n) -> 
    let m = update lvl m in
    let n = update lvl n in
    App (m, n)

  let to_cps expr =
    let rec aux expr = match expr with
    | Var id -> Abs(App(Var 0, Var (id+1)))
    | Abs m  ->
      let m = aux m in
      Abs(App(Var 0, Abs (update 0 m)))
    | App(m, n) ->
      let m = aux m in
      let n = aux n in
      Abs(App(update (-1) m, Abs(App(update 0 (update (-1) n), Abs(App(App(Var 1, Var 0), Var 2))))))
    in
    aux expr
end

(**  Plotkin Transformation

    [x]     : x
    [/\x.M] : /\k.@k (/\x.[M])
    [@N M]  : /\k.[M] (/\m.@(@(m [N]) k))
*)
module Plotkin = struct

  (* Update indexes of the bruin when theres a new abstraction added *)
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
  (* Plotkin sem reduções beta só eta *)
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
      let m = aux_line m in
      Abs(App(Var 0, Abs (update 0 m)))
    | App(m, n) ->
      let m = aux m in
      let n = aux n in
      Abs(App(update (-1) m, Abs(App(update 0 (update (-1) n), Abs(App(App( Var 1, Var 0), Var 2))))))
    
    and aux_line = function
    | Var id -> Abs(Var (id+1))
    | Abs m  ->
      let m = aux_line m in
      Abs(App(Var 0, Abs(Abs(App(update (-1) m, Var 0)))))
    | App(m, n) ->
      let m = aux m in
      let n = aux n in
      Abs(App(update (-1) m, Abs(App(update 0 (update (-1) n), Abs(App(App( Var 1, Var 0), (Abs(App(Var 2, Var 0)))))))))
    in
    aux expr
end
