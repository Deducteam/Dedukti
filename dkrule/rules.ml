open Term
open Rule

let print fmt =
  Printf.kfprintf (fun _ -> print_newline () ) stdout fmt

let is_linear r =
  let seen = Array.create (List.length r.ctx) false in
  let rec aux = function
    | MatchingVar (_,_,n,[]) ->
        if seen.(n) then false else ( seen.(n) <- true ; true )
    | Pattern (_,_,_,args) -> List.for_all aux args
    | Brackets _ -> false
    | Joker _ -> true
    | _  -> failwith "Not Implemented" (*TODO*)
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

let print_rules_if condition =
  List.iter (fun r -> if condition r then print "%a" Pp.pp_rule r)

let print_all = List.iter (print "%a" Pp.pp_rule)

let print_pi_rules = print_rules_if is_pi_rule

let print_non_linear_rules = print_rules_if (fun r -> not (is_linear r))

let print_type_level_rules = print_rules_if is_type_level
