open Basic

module type S =
sig
  type t

  val handle_entry : Env.t -> Entry.entry -> unit

  val get_data : unit -> t
end

module TypeChecker : S with type t = unit =
struct

  type t = unit

  let handle_entry env e =
    let open Entry in
    let (module Pp:Pp.Printer) = Env.get_printer env in
    match e with
    | Decl(lc,id,st,ty) ->
      Debug.debug Debug.D_notice "Declaration of constant '%a'." pp_ident id;
      Env.declare env lc id st ty
    | Def(lc,id,opaque,ty,te) ->
      let opaque_str = if opaque then " (opaque)" else "" in
      Debug.debug Debug.D_notice "Definition of symbol '%a'%s." pp_ident id opaque_str;
      Env.define env lc id opaque te ty
    | Rules(l,rs) ->
      let open Rule in
      List.iter (fun (r:untyped_rule) ->
          Debug.(debug D_notice "Adding rewrite rules: '%a'" Pp.print_rule_name r.name)) rs;
      let rs = Env.add_rules env rs in
      List.iter (fun (s,r) ->
          Debug.debug Debug.D_notice "%a@.with the following constraints: %a"
            pp_typed_rule r (Subst.Subst.pp (fun n -> let _,n,_ = List.nth r.ctx n in n)) s) rs
    | Eval(_,red,te) ->
      let te = Env.reduction env ~red te in
      Format.printf "%a@." Pp.print_term te
    | Infer(_,red,te) ->
      let  ty = Env.infer env te in
      let rty = Env.reduction env ~red ty in
      Format.printf "%a@." Pp.print_term rty
    | Check(l, assrt, neg, Convert(t1,t2)) ->
      let succ = (Env.are_convertible env t1 t2) <> neg in
      ( match succ, assrt with
        | true , false -> Format.printf "YES@."
        | true , true  -> ()
        | false, false -> Format.printf "NO@."
        | false, true  -> Env.raise_env env l Env.AssertError )
    | Check(l, assrt, neg, HasType(te,ty)) ->
      let succ = try Env.check env te ty; not neg with _ -> neg in
      ( match succ, assrt with
        | true , false -> Format.printf "YES@."
        | true , true  -> ()
        | false, false -> Format.printf "NO@."
        | false, true  -> Env.raise_env env l Env.AssertError )
    | DTree(lc,m,v) ->
      let m = match m with None -> Env.get_name env | Some m -> m in
      let cst = mk_name m v in
      let forest = Env.get_dtree env lc cst in
      Format.printf "GDTs for symbol %a:@.%a" pp_name cst Dtree.pp_dforest forest
    | Print(_,s) -> Format.printf "%s@." s
    | Name(_,n) ->
      if not (mident_eq n (Env.get_name env))
      then Debug.(debug D_warn "Invalid #NAME directive ignored.@.")
    | Require(lc,md) -> Env.import env lc md

  let get_data () = ()

end

module SignatureBuilder : S with type t = Signature.t =
struct
  type t = Signature.t

  let sg : Signature.t option ref = ref None

  let handle_entry env e =
    sg := Some (Env.get_signature env);
    let sg = Env.get_signature env in
    let md = Env.get_name      env in
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
      Env.raise_env env lc (Env.EnvErrorType(Typing.DomainFreeLambda lc))
    | Rules(lc,rs) ->
      Signature.add_rules sg (List.map Rule.to_rule_infos rs)
    | Require(lc,md) -> Signature.import sg lc md
    | _ -> ()

  let get_data () =
    match !sg with
    | None -> Signature.make "<not initialized>"
    | Some sg -> sg

end

module EntryPrinter : S with type t = unit =
struct

  type t = unit

  let handle_entry env e =
    let (module Pp:Pp.Printer) = (module Pp.Make(struct let get_name () = Env.get_name env end)) in
    Pp.print_entry Format.std_formatter e

  let get_data () = ()

end

module Dependencies : S with type t = Dep.t =
struct
  type t = Dep.t

  let handle_entry env e = Dep.handle (Env.get_name env) (fun f -> f e)

  let get_data () = Dep.deps
end
