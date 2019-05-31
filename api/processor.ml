open Basic
open Term
open Rule
open Typing
open Signature
open Env
open Entry

module type Processor =
sig
  module Printer : Pp.Printer
  module ErrorHandler : Errors.ErrorHandler

  val handle_entry : Entry.entry -> unit
end

module TypeChecker (E:Env.S) : Processor =
struct
  module Printer = E.Pp
  module ErrorHandler = Errors.Make(E)

  open E
  open Debug

  let handle_entry e =
    match e with
    | Decl(lc,id,st,ty) ->
      debug D_notice "Declaration of constant '%a'." pp_ident id;
      declare lc id st ty
    | Def(lc,id,opaque,ty,te) ->
      let opaque_str = if opaque then " (opaque)" else "" in
      debug D_notice "Definition of symbol '%a'%s." pp_ident id opaque_str;
      define lc id opaque te ty
    | Rules(l,rs) ->
      let open Rule in
      List.iter (fun (r:untyped_rule) ->
          Debug.(debug D_notice "Adding rewrite rules: '%a'" Pp.print_rule_name r.name)) rs;
      let rs = add_rules rs in
      List.iter (fun (s,r) ->
          Debug.debug Debug.D_notice "%a@.with the following constraints: %a"
            pp_typed_rule r (Subst.Subst.pp (fun n -> let _,n,_ = List.nth r.ctx n in n)) s) rs
    | Eval(_,red,te) ->
      let te = reduction ~red te in
      Format.printf "%a@." Pp.print_term te
    | Infer(_,red,te) ->
      let  ty = infer te in
      let rty = reduction ~red ty in
      Format.printf "%a@." Pp.print_term rty
    | Check(l, assrt, neg, Convert(t1,t2)) ->
      let succ = (are_convertible t1 t2) <> neg in
      ( match succ, assrt with
        | true , false -> Format.printf "YES@."
        | true , true  -> ()
        | false, false -> Format.printf "NO@."
        | false, true  -> raise_env l AssertError )
    | Check(l, assrt, neg, HasType(te,ty)) ->
      let succ = try check te ty; not neg with _ -> neg in
      ( match succ, assrt with
        | true , false -> Format.printf "YES@."
        | true , true  -> ()
        | false, false -> Format.printf "NO@."
        | false, true  -> raise_env l AssertError )
    | DTree(lc,m,v) ->
      let m = match m with None -> get_name () | Some m -> m in
      let cst = mk_name m v in
      let forest = get_dtree lc cst in
      Format.printf "GDTs for symbol %a:@.%a" pp_name cst Dtree.pp_dforest forest
    | Print(_,s) -> Format.printf "%s@." s
    | Name(_,n) ->
      if not (mident_eq n (E.get_name()))
      then Debug.(debug D_warn "Invalid #NAME directive ignored.@.")
    | Require(lc,md) -> import lc md

end

module DefaultTypeChecker : Processor = TypeChecker(Env.Default)

module SignatureBuilder (E:Env.S) : Processor =
struct
  module Printer      = E.Pp
  module ErrorHandler = Errors.Make(E)
  open E

  let sg = E.get_signature (* maybe replace sg with sg()*)
  let md = E.get_name      (* maybe replace md with md()*)

  let handle_entry e =
    let sg = E.get_signature() in
    let md = E.get_name() in
    match e with
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

end

module EntryPrinter : Processor =
struct
  module Printer      = Env.Default.Pp
  module ErrorHandler = Errors.Default
  let handle_entry = Printer.print_entry Format.std_formatter
end
