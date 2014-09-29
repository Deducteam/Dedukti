open Basics
open Printf
open Term
open Rule

let rec pp_pattern k out = function
  | MatchingVar (_,x,n,args) ->
      begin
        List.iter (fun _ -> fprintf out "#APP(") args ;
        fprintf out "#VAR_%a" pp_ident x ;
        List.iter (fun (_,_,a) -> fprintf out ",DB_%i)" a) args
      end
  | BoundVar (_,_,n,args) ->
      begin
        List.iter (fun _ -> fprintf out "#APP(") args ;
        fprintf out "#DB_%i" n ;
        List.iter (fun pat -> fprintf out ",%a)" (pp_pattern k) pat) args
      end
  | Pattern (_,m,v,args) ->
      begin
        List.iter (fun _ -> fprintf out "#APP(") args ;
        fprintf out "%a.%a" pp_ident m pp_ident v ;
        List.iter (fun pat -> fprintf out ",%a)" (pp_pattern k) pat) args
      end
  | Lambda (_,_,pat) -> fprintf out "#LAMBDA(%a)" (pp_pattern (k+1)) pat
  | Joker lc ->
      let (l,c) = of_loc lc in
        fprintf out "#UNDERSCORE_%i_%i" l c
  | Brackets _ -> failwith "Not Implemented (conditionnal rule)."

let rec pp_term k out = function
  | Const (_,m,v) -> fprintf out "%a.%a" pp_ident m pp_ident v
  | Lam (_,_,_,te) -> fprintf out "#LAMBDA(%a)" (pp_term (k+1)) te
  | Pi (_,_,a,b) -> fprintf out "#PI(%a,%a)" (pp_term k) a (pp_term (k+1)) b
  | DB (_,x,n) ->
      if n>=k then fprintf out "#VAR_%a" pp_ident x
      else fprintf out "#DB_%i" (k-n-1)
  | App (f,a,args) ->
      ( List.iter (fun _ -> fprintf out "#APP(" ) (a::args);
        pp_term k out f ;
        List.iter ( fprintf out ",%a)" (pp_term k) ) (a::args) )
  | Kind | Type _  -> assert false

let rec pp_underscore out = function
  | MatchingVar _ -> ()
  | BoundVar (_,_,_,args) | Pattern (_,_,_,args) ->
      List.iter (pp_underscore out) args
  | Lambda (_,_,p) -> pp_underscore out p
  | Joker lc ->
      let (l,c) = of_loc lc in
        fprintf out " #UNDERSCORE_%i_%i" l c
  | Brackets _ -> failwith "Not Implemented (conditionnal rule)."

let pp_rule out r =
  let pat = (Pattern (r.l,r.md,r.id,r.args)) in
  fprintf out "(VAR";
  List.iter ( fun (v,_) -> fprintf out " #VAR_%a" pp_ident v ) r.ctx ;
  pp_underscore out pat ;
  fprintf out ")\n";
  fprintf out "(RULES %a -> %a )\n\n" (pp_pattern 0) pat (pp_term 0) r.rhs

let export out =
  List.iter (
    fun (md,lst) ->
      fprintf out "(COMMENT Rewrite rules for module '%s')\n" md;
      List.iter (pp_rule out) lst
  )
