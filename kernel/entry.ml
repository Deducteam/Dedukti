open Basic
open Term

type is_opaque    = bool
type is_assertion = bool
type should_fail  = bool

type test =
  | Convert of term * term
  | HasType of term * term

type entry =
  | Decl  of loc * ident * Signature.staticity * term
  | Def   of loc * ident * is_opaque * term option * term
  | Rules of Rule.untyped_rule list
  | Eval  of loc * Reduction.red_cfg * term
  | Check of loc * is_assertion * should_fail * test
  | Infer of loc * Reduction.red_cfg * term
  | Print of loc * string
  | DTree of loc * mident option * ident
  | Name  of loc * mident

let pp_entry fmt e =
  let open Format in
  match e with
  | Decl(_,id,stat,ty)      ->
      let stat = if stat = Signature.Definable then "def " else "" in
      fprintf fmt "@[<2>%s%a :@ %a.@]@.@." stat pp_ident id pp_term ty
  | Def(_,id,opaque,ty,te)  ->
      let key = if opaque then "thm" else "def" in
      begin
        match ty with
        | None    -> fprintf fmt "@[<hv2>%s %a@ :=@ %a.@]@.@." key
                       pp_ident id pp_term te
        | Some ty -> fprintf fmt "@[<hv2>%s %a :@ %a@ :=@ %a.@]@.@." key
                       pp_ident id pp_term ty pp_term te
      end
  | Rules(rs)               ->
      fprintf fmt "@[<v0>%a@].@.@." (pp_list "" Rule.pp_untyped_rule) rs
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
  | Name(_,_)               ->
      ()
