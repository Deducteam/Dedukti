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
  | Rules of loc * Rule.untyped_rule list
  | Eval  of loc * Reduction.red_cfg * term
  | Check of loc * is_assertion * should_fail * test
  | Infer of loc * Reduction.red_cfg * term
  | Print of loc * string
  | DTree of loc * mident option * ident
  | Name  of loc * mident
  | Require of loc * mident

let pp_entry fmt e =
  let open Format in
  match e with
  | Decl(_,id,Signature.Static,ty) ->
    fprintf fmt "@[<2>%a :@ %a.@]@.@." pp_ident id pp_term ty
  | Decl(_,id,Signature.Definable,ty) ->
    fprintf fmt "@[<2>def %a :@ %a.@]@.@." pp_ident id pp_term ty
  | Def(_,id,opaque,ty,te)  ->
    let key = if opaque then "thm" else "def" in
    begin
      match ty with
      | None    -> fprintf fmt "@[<hv2>%s %a@ :=@ %a.@]@.@." key
                     pp_ident id pp_term te
      | Some ty -> fprintf fmt "@[<hv2>%s %a :@ %a@ :=@ %a.@]@.@." key
                     pp_ident id pp_term ty pp_term te
    end
  | Rules(_,rs)             ->
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
  | Require(_,m)            ->
    fprintf fmt "#REQUIRE %a.@." pp_mident m


let to_signature : string -> ?sg:Signature.t -> entry list -> Signature.t =
  fun path ?(sg=Signature.make path) entries ->
    (* FIXME: so hackish *)
    let md = Signature.get_name (Signature.make path) in
    let mk_entry = function
      | Decl(lc,id,st,ty) ->
        Signature.add_external_declaration sg lc (Basic.mk_name md id) st ty
      | Def(lc,id,op,Some ty,te) ->
        let open Rule in
        Signature.add_external_declaration sg lc (Basic.mk_name md id) Signature.Definable ty;
        let cst = Basic.mk_name md id in
        let rule = { name= Delta(cst) ; ctx = [] ; pat = Pattern(lc, cst, []); rhs = te ; } in
        Signature.add_rules sg [Rule.to_rule_infos rule]
      | Def(lc,id,op, None,te) -> assert false (*FIXME *)
      | Rules(lc,rs) ->
        Signature.add_rules sg (List.map Rule.to_rule_infos rs)
      | Require(lc,md) -> Signature.import sg lc md
      | _ -> ()
    in
    List.iter mk_entry entries;
    sg
