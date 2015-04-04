open Basics
open Term
open Rule

let print out fmt =
  Printf.kfprintf (fun _ -> print_newline () ) stdout fmt

let is_non_linear r =
  let seen = Array.make (List.length r.ctx) false in
  let rec aux k = function
    | Lambda (_,_,p) -> aux (k+1) p
    | Pattern (_,_,_,args) -> List.exists (aux k) args
    | Var (_,_,n,args) when n<k -> List.exists (aux k) args
    | Var (_,_,n,args) ->
        if seen.(n-k) then true
        else ( seen.(n-k) <- true; List.exists (aux k) args )
    | Brackets _ -> true
  in
    List.exists (aux 0) r.args

let is_type_level r =
  let rec is_kind = function
    | Type _ -> true
    | Pi (_,_,_,b) -> is_kind b
    | _ -> false
  in
    is_kind (Env.get_type dloc r.md r.id)

let print_rule_list out = List.iter (print out "%a" Pp.pp_frule)

let print_rule_list_filter out condition =
    List.iter (fun r -> if condition r then print out "%a" Pp.pp_frule r)

let print_all out = List.iter (fun (_,lst) -> print_rule_list out lst)

let print_non_linear_rules out =
  List.iter (fun (_,lst) -> print_rule_list_filter out is_non_linear lst)

let print_type_level_rules out =
  List.iter (fun (_,lst) -> print_rule_list_filter out is_type_level lst)
