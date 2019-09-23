open Basic

module type S =
sig
  type env
  type t
  val handle_entry : env -> Entry.entry -> unit
  val get_data : env -> t
end

module TypeChecker (E:Env.S) : S with type env = E.t and type t = unit =
struct
  type env = E.t
  type t = unit

  open Debug

  let handle_entry sg e =
    let open Entry in
    match e with
    | Decl(lc,id,st,ty) ->
      debug D_notice "Declaration of constant '%a'." pp_ident id;
      E.declare sg lc id st ty
    | Def(lc,id,opaque,ty,te) ->
      let opaque_str = if opaque then " (opaque)" else "" in
      debug D_notice "Definition of symbol '%a'%s." pp_ident id opaque_str;
      E.define sg lc id opaque te ty
    | Rules(l,rs) ->
      let open Rule in
      List.iter (fun (r:untyped_rule) ->
          Debug.(debug D_notice "Adding rewrite rules: '%a'" (E.Printer.print_rule_name sg) r.name)) rs;
      let rs = E.add_rules sg rs in
      List.iter (fun (s,r) ->
          Debug.debug Debug.D_notice "%a@.with the following constraints: %a"
            pp_typed_rule r (Subst.Subst.pp (fun n -> let _,n,_ = List.nth r.ctx n in n)) s) rs
    | Eval(_,red,te) ->
      let te = E.reduction ~red sg te in
      Format.printf "%a@." (E.Printer.print_term sg) te
    | Infer(_,red,te) ->
      let  ty = E.infer sg te in
      let rty = E.reduction sg ~red ty in
      Format.printf "%a@." (E.Printer.print_term sg) rty
    | Check(l, assrt, neg, Convert(t1,t2)) ->
      let succ = (E.are_convertible sg t1 t2) <> neg in
      ( match succ, assrt with
        | true , false -> Format.printf "YES@."
        | true , true  -> ()
        | false, false -> Format.printf "NO@."
        | false, true  -> E.raise_env sg l Env.AssertError )
    | Check(l, assrt, neg, HasType(te,ty)) ->
      let succ = try E.check sg te ty; not neg with _ -> neg in
      ( match succ, assrt with
        | true , false -> Format.printf "YES@."
        | true , true  -> ()
        | false, false -> Format.printf "NO@."
        | false, true  -> E.raise_env sg l Env.AssertError )
    | DTree(lc,m,v) ->
      let m = match m with None -> E.get_name sg | Some m -> m in
      let cst = mk_name m v in
      let forest = E.get_dtree sg lc cst in
      Format.printf "GDTs for symbol %a:@.%a" pp_name cst Dtree.pp_dforest forest
    | Print(_,s) -> Format.printf "%s@." s
    | Name(_,n) ->
      if not (mident_eq n (E.get_name sg))
      then Debug.(debug D_warn "Invalid #NAME directive ignored.@.")
    | Require(lc,md) -> E.import sg lc md

  let get_data _ = ()
end

module SignatureBuilder (E:Env.S) : S with type env = E.t and type t = Signature.t =
struct
  type env = E.t
  type t = Signature.t

  let handle_entry env e =
    let sg = E.get_signature env in
    let md = E.get_name env in
    let open Entry in
    match e with
    | Decl(lc,id,st,ty) ->
      Signature.add_external_declaration sg lc (Basic.mk_name md id) st ty
    | Def(lc,id,op,Some ty,te) ->
      let open Rule in
      Signature.add_external_declaration sg lc (Basic.mk_name md id) Signature.Definable ty;
      let cst = Basic.mk_name md id in
      let rule = { name= Delta(cst) ; ctx = [] ; pat = Pattern(lc, cst, []); rhs = te ; } in
      Signature.add_rules sg [Rule.to_rule_infos rule]
    | Def(lc,id,op, None,te) ->
      E.raise_env env lc (Env.EnvErrorType(Typing.DomainFreeLambda lc))
    | Rules(lc,rs) ->
      Signature.add_rules sg (List.map Rule.to_rule_infos rs)
    | Require(lc,md) -> E.import env lc md
    | _ -> ()

  let get_data env = E.get_signature env

end

module EntryPrinter (E:Env.S) : S with type env = E.t and type t = unit =
struct
  type env = E.t
  type t = unit
  let handle_entry env = E.Printer.print_entry env Format.std_formatter
  let get_data = ignore
end

module Dependencies (E:Env.S) : S with type env = E.t and type t = Dep.t =
struct
  type env = E.t
  type t = Dep.t

  let handle_entry env e = Dep.handle (E.get_name env) (fun f -> f e)

  let get_data _ = Dep.deps
end
