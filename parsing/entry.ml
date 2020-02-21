open Kernel
open Basic
open Term

type is_opaque    = bool
type is_assertion = bool
type should_fail  = bool

type test =
  | Convert of term * term
  | HasType of term * term

type entry =
  | Decl  of loc * ident * Signature.scope * Signature.staticity * term
  | Def   of loc * ident * Signature.scope * is_opaque * term option * term
  | Rules of loc * Rule.partially_typed_rule list
  | Eval  of loc * Reduction.red_cfg * term
  | Check of loc * is_assertion * should_fail * test
  | Infer of loc * Reduction.red_cfg * term
  | Print of loc * string
  | DTree of loc * mident option * ident
  | Name  of loc * mident
  | Require of loc * mident

let pp_entry fmt e =
  let open Format in
  let scope_to_string = function
    | Signature.Public  -> ""
    | Signature.Private -> "private "
  in
  match e with
  | Decl(_,id,scope,Static,ty) ->
     fprintf fmt "@[<2>%s%a :@ %a.@]@.@." (scope_to_string scope)
       pp_ident id pp_term ty
  | Decl(_,id,scope,Definable Free,ty) ->
     fprintf fmt "@[<2>%sdef %a :@ %a.@]@.@." (scope_to_string scope)
       pp_ident id pp_term ty
  | Decl(_,id,scope,Injective,ty) ->
     fprintf fmt "@[<2>%sinjective %a :@ %a.@]@.@." (scope_to_string scope)
       pp_ident id pp_term ty
  | Decl(_,id,scope,Definable AC,ty) ->
     fprintf fmt "@[<2>%sdefac %a [@ %a].@]@.@."(scope_to_string scope)
       pp_ident id pp_term ty
  | Decl(_,id,scope,Definable ACU(neu),ty) ->
     fprintf fmt "@[<2>%sdefacu %a [@ %a, %a].@]@.@."(scope_to_string scope)
       pp_ident id pp_term ty pp_term neu
  | Def(_,id,scope,opaque,ty,te) ->
    let key = if opaque then "thm" else "def" in
    begin
      match ty with
      | None    -> fprintf fmt "@[<hv2>%s%s %a@ :=@ %a.@]@.@."
                     (scope_to_string scope) key
                     pp_ident id pp_term te
      | Some ty -> fprintf fmt "@[<hv2>%s%s %a :@ %a@ :=@ %a.@]@.@."
                     (scope_to_string scope) key
                     pp_ident id pp_term ty pp_term te
    end
  | Rules(_,rs)             ->
    fprintf fmt "@[<v0>%a@].@.@." (pp_list "" Rule.pp_part_typed_rule) rs
  | Eval(_,cfg,te)          ->
    fprintf fmt "#EVAL%a %a.@." Reduction.pp_red_cfg cfg pp_term te
  | Infer(_,cfg,te)         ->
    fprintf fmt "#INFER%a %a.@." Reduction.pp_red_cfg cfg pp_term te
  | Check(_,assrt,neg,test) ->
    let cmd = if assrt then "#ASSERT" else "#CHECK" in
    let neg = if neg then "NOT" else "" in
    begin
      match test with
      | Convert(t1,t2) ->
         fprintf fmt "%s%s %a ==@ %a.@." cmd neg pp_term t1 pp_term t2
      | HasType(te,ty) ->
         fprintf fmt "%s%s %a ::@ %a.@." cmd neg pp_term te pp_term ty
    end
  | DTree(_,m,v)            ->
     begin
       match m with
       | None   -> fprintf fmt "#GDT %a.@." pp_ident v
       | Some m -> fprintf fmt "#GDT %a.%a.@." pp_mident m pp_ident v
     end
  | Print(_, str)           ->
     fprintf fmt "#PRINT %S.@." str
  | Name(_,_)               -> ()
  | Require(_, md) ->
     fprintf fmt "#REQUIRE %a.@." pp_mident md

let loc_of_entry = function
  | Decl(lc,_,_,_,_)
  | Def(lc,_,_,_,_,_)
  | Rules(lc,_)
  | Eval(lc,_,_)
  | Infer(lc,_,_)
  | Check(lc,_,_,_)
  | DTree(lc,_,_)
  | Print(lc,_)
  | Name(lc,_)
  | Require(lc,_)    -> lc
