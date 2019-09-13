open Basic

module type S =
sig
  type t

  val handle_entry : Entry.entry -> unit

  val get_data : unit -> t
end

module TypeChecker (E:Env.S) : S with type t = unit =
struct
  module Printer = E.Printer

  open Debug

  type t = unit

  let handle_entry e =
    let open Entry in
    match e with
    | Decl(lc,id,st,ty) ->
      debug D_notice "Declaration of constant '%a'." pp_ident id;
      E.declare lc id st ty
    | Def(lc,id,opaque,ty,te) ->
      let opaque_str = if opaque then " (opaque)" else "" in
      debug D_notice "Definition of symbol '%a'%s." pp_ident id opaque_str;
      E.define lc id opaque te ty
    | Rules(l,rs) ->
      let open Rule in
      List.iter (fun (r:untyped_rule) ->
          Debug.(debug D_notice "Adding rewrite rules: '%a'" Printer.print_rule_name r.name)) rs;
      let rs = E.add_rules rs in
      List.iter (fun (s,r) ->
          Debug.debug Debug.D_notice "%a@.with the following constraints: %a"
            pp_typed_rule r (Subst.Subst.pp (fun n -> let _,n,_ = List.nth r.ctx n in n)) s) rs
    | Eval(_,red,te) ->
      let te = E.reduction ~red te in
      Format.printf "%a@." Printer.print_term te
    | Infer(_,red,te) ->
      let  ty = E.infer te in
      let rty = E.reduction ~red ty in
      Format.printf "%a@." Printer.print_term rty
    | Check(l, assrt, neg, Convert(t1,t2)) ->
      let succ = (E.are_convertible t1 t2) <> neg in
      ( match succ, assrt with
        | true , false -> Format.printf "YES@."
        | true , true  -> ()
        | false, false -> Format.printf "NO@."
        | false, true  -> E.raise_env l Env.AssertError )
    | Check(l, assrt, neg, HasType(te,ty)) ->
      let succ = try E.check te ty; not neg with _ -> neg in
      ( match succ, assrt with
        | true , false -> Format.printf "YES@."
        | true , true  -> ()
        | false, false -> Format.printf "NO@."
        | false, true  -> E.raise_env l Env.AssertError )
    | DTree(lc,m,v) ->
      let m = match m with None -> E.get_name () | Some m -> m in
      let cst = mk_name m v in
      let forest = E.get_dtree lc cst in
      Format.printf "GDTs for symbol %a:@.%a" pp_name cst Dtree.pp_dforest forest
    | Print(_,s) -> Format.printf "%s@." s
    | Name(_,n) ->
      if not (mident_eq n (E.get_name()))
      then Debug.(debug D_warn "Invalid #NAME directive ignored.@.")
    | Require(lc,md) -> E.import lc md

  let get_data () = ()

end

module SignatureBuilder (E:Env.S) : S with type t = Signature.t =
struct
  type t = Signature.t

  let handle_entry e =
    let sg = E.get_signature () in
    let md = E.get_name      () in
    let open Entry in
    match e with
    | Decl(lc,id,st,ty) ->
      Signature.add_external_declaration sg lc (Basic.mk_name md id) st ty
    | Def(lc,id,op,Some ty,te) ->
      let open Rule in
      Signature.add_external_declaration sg lc (Basic.mk_name md id) (Signature.Definable Term.Free) ty;
      let cst = Basic.mk_name md id in
      let rule = { name= Delta(cst) ; ctx = [] ; pat = Pattern(lc, cst, []); rhs = te ; } in
      Signature.add_rules sg [Rule.to_rule_infos rule]
    | Def(lc,id,op, None,te) ->
      E.raise_env lc (Env.EnvErrorType(Typing.DomainFreeLambda lc))
    | Rules(lc,rs) ->
      Signature.add_rules sg (List.map Rule.to_rule_infos rs)
    | Require(lc,md) -> Signature.import sg lc md
    | _ -> ()

  let get_data () = E.get_signature ()

end

module EntryPrinter (E:Env.S) : S with type t = unit =
struct
  module Printer      = E.Printer

  type t = unit

  let handle_entry = Printer.print_entry Format.std_formatter

  let get_data () = ()

end

module Dependencies (E:Env.S) : S with type t = Dep.t =
struct
  type t = Dep.t

  let handle_entry e = Dep.handle (E.get_name ()) (fun f -> f e)

  let get_data () = Dep.deps
end
