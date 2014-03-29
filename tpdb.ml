open Printf
open Types

let rule_num = ref 0
let get_rule_num _ =
  incr rule_num ; !rule_num

let rec print_pattern num out = function
  | Var (x,n)          -> fprintf out "#VAR_%i_%i" num n
  | Pattern (m,v,args) ->
      begin
        for i=1 to Array.length args do fprintf out "#APP(" done ;
        fprintf out "%a.%a" pp_ident m pp_ident v ;
        Array.iter (fprintf out ",%a)" (print_pattern num)) args
      end
  | Brackets _ -> assert false

let rec print_term num i out = function
  | Const (m,v)          -> fprintf out "%a.%a" pp_ident m pp_ident v
  | Lam (_,ty,te)        ->
      fprintf out "#LAMBDA(%a,%a)"
        (print_term num i) ty
        (print_term num (i+1)) te
  | Pi (_,a,b)                  ->
      fprintf out "#PI(%a,%a)"
        (print_term num i) a
        (print_term num (i+1)) b
  | DB (x,n)                    ->
      if n>=i then fprintf out "#VAR_%i_%i" num n
      else fprintf out "#DB_%i" (i-n-1)
  | App (f::lst)                ->
      begin
        List.iter (fun _ -> fprintf out "#APP(" ) lst;
        print_term num i out f ;
        List.iter ( fprintf out ",%a)" (print_term num i) ) lst
      end
  | Kind | Type | Meta _ | App [] -> assert false

let print_rule out m v r =
  assert ( r.constraints = [] ) ; (*TODO*)
  let num = get_rule_num () in
    (* VAR *)
    fprintf out "(VAR";
    for i=0 to (r.env_size-1) do (*FIXME use variable names*)
      fprintf out " #VAR_%i_%i" num i
    done ;
    fprintf out ")\n";
    (* RULES *)
    fprintf out "(RULES %a -> %a )\n\n"
      (print_pattern num) (Pattern (m,v,r.pats))
      (print_term num 0) r.right
