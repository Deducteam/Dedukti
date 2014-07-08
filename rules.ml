open Types

let is_linear r =
  let seen = Array.create (List.length r.ctx) false in
  let rec aux = function
    | Var (_,_,n) -> if seen.(n) then false else ( seen.(n) <- true ; true )
    | Pattern (_,_,_,args) -> List.for_all aux args
    | Brackets _ -> false
  in
    List.for_all aux r.args

let is_type_level r =
  let rec is_kind = function
    | Type _ -> true
    | Pi (_,_,_,b) -> is_kind b
    | _ -> false
  in
    is_kind (Env.get_type dloc r.md r.id)

let is_pi_rule r =
  let rec has_pi = function
    | App (b,_,_) | Lam (_,_,_,b) -> has_pi b
    | Pi (_,_,_,_) -> true
    | _ -> false
  in
    has_pi r.rhs

let list_rules_if condition =
  Env.rules_iter ( fun r -> if condition r then Global.print "%a" Pp.pp_rule r )

let list_rules () = list_rules_if (fun _ -> true)

let list_pi_rules () = list_rules_if is_pi_rule

let list_non_linear_rules () = list_rules_if (fun r -> not (is_linear r))

let list_type_level_rules () = list_rules_if is_type_level
