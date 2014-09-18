open Term
open Rule

let print out fmt =
  Printf.kfprintf (fun _ -> print_newline () ) stdout fmt

let is_non_linear r =
  let seen = Array.create (List.length r.ctx) false in
  let rec aux = function
    | Lambda (_,_,p) -> aux p
    | BoundVar (_,_,_,args) | Pattern (_,_,_,args) -> List.exists aux args
    | MatchingVar (_,_,n,_) ->
        if seen.(n) then true
        else ( seen.(n) <- true; false )
    | Brackets _ -> true
    | Joker _ -> false
  in
    List.exists aux r.args

let is_type_level r =
  let rec is_kind = function
    | Type _ -> true
    | Pi (_,_,_,b) -> is_kind b
    | _ -> false
  in
    is_kind (Env.get_type dloc r.md r.id)

let print_rule_list out = List.iter (print out "%a" Pp.pp_rule)

let print_rule_list_filter out condition =
    List.iter (fun r -> if condition r then print out "%a" Pp.pp_rule r)

let print_all out = List.iter (fun (_,lst) -> print_rule_list out lst)

let print_non_linear_rules out =
  List.iter (fun (_,lst) -> print_rule_list_filter out is_non_linear lst)

let print_type_level_rules out =
  List.iter (fun (_,lst) -> print_rule_list_filter out is_type_level lst)
