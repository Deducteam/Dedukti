open Printf
open Types

let rec pp_pattern out = function
  | Var (_,v) -> fprintf out "#VAR_%a" pp_ident (Var.ident v)
  | Pattern (_,m,v,args) ->
      begin
        List.iter (fun _ -> fprintf out "#APP(") args ;
        fprintf out "%a.%a" pp_ident m pp_ident v ;
        List.iter (fun pat -> fprintf out ",%a)" pp_pattern pat) args
      end
  | Brackets _ -> failwith "Not Implemented: conditionnal rewriting in TPDB export."
  | Joker (_,v) -> fprintf out "#UNDERSCORE_%a" Var.pp v

let rec pp_term out = function
  | Const (_,m,v) -> fprintf out "%a.%a" pp_ident m pp_ident v
  | Lam (_,v,ty,te) -> fprintf out "#LAMBDA(%a,%a,%a)" Var.pp v pp_term ty pp_term te
  | Pi (_,None,a,b) -> fprintf out "#PI(%a,%a)" pp_term a pp_term b
  | Pi (_,Some v,a,b) -> fprintf out "#PI(%a,%a,%a)" Var.pp v pp_term a pp_term b
  | Let (_,v,a,b) -> fprintf out "#LET(%a,%a,%a)" Var.pp v pp_term a pp_term b
  | Var (_,v) -> fprintf out "#VAR_%a" Var.pp v
  | App (f,a,args) ->
      ( List.iter (fun _ -> fprintf out "#APP(" ) (a::args);
        pp_term out f ;
        List.iter ( fprintf out ",%a)" pp_term ) (a::args) )
  | Kind | Type _ | Meta _ -> assert false

let rec pp_underscore out = function
  | Var _ -> () | Brackets _ -> ()
  | Joker (_,v) -> fprintf out " #UNDERSCORE_%a" Var.pp v
  | Pattern (_,m,v,args) -> List.iter (pp_underscore out) args

let pp_rule out r =
  let pat = (Pattern (r.l,r.md,r.id,r.args)) in
  fprintf out "(VAR";
  List.iter ( fun (v,_) -> fprintf out " #VAR_%a" Var.pp v ) (VarMap.bindings r.ctx.var2ty) ;
  pp_underscore out pat ;
  fprintf out ")\n";
  fprintf out "(RULES %a -> %a )\n\n" pp_pattern pat pp_term r.rhs

let export out =
  List.iter (
    fun (md,lst) ->
      fprintf out "(COMMENT Rewrite rules for module '%s')\n" md;
      List.iter (pp_rule out) lst
  )
