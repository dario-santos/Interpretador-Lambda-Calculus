let rec can_reduce = function
  | Lambda.Var _ -> false
  | Lambda.Abs _ -> true
  | Lambda.App(t1, _) -> can_reduce t1

let b_reduce arg body =
  (*If it's not the id to change then subctract 1. Is this correct????? what about /\x./\y.y will not /\y.y go to -1?*)
  let rec aux arg body lv remove = match body with
    | Lambda.Var id -> 
      begin
        match id with
        | id when id = lv -> arg
        | id when id > lv -> Lambda.Var (id-1)
        | id when id < lv -> Lambda.Var id
      end
    | Lambda.Abs t1 ->
      let t1 = aux arg t1 (lv+1) false in
      if remove then t1 else (Lambda.Abs t1)
    | Lambda.App (t1, t2) ->
      let t1 = aux arg t1 lv false in 
      let t2 = aux arg t2 lv false in
      Lambda.App(t1, t2)
    in
  aux arg body (-1) true


module Eager = struct
  let rec exec = function
    | Lambda.Var id -> Lambda.Var id
    | Lambda.Abs t1 -> Lambda.Abs (exec t1)
    | Lambda.App (t1, t2) ->
      let t1 = exec t1 in
      let t2 = exec t2 in
      let tr = b_reduce t2 t1 in
      
      if can_reduce t1 then exec tr else Lambda.App(t1, t2)
end

module Lazy = struct
  let rec exec = function
    | Lambda.Var id -> Lambda.Var id
    | Lambda.Abs t1 -> Lambda.Abs (exec t1)
    | Lambda.App (t1, t2) ->
      let t1 = exec t1 in
      let tr = b_reduce t2 t1 in
    
      if can_reduce t1 then exec tr else Lambda.App(t1, t2)
end
