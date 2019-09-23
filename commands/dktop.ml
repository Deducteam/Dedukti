open Basic
open Parser
open Entry

module E = Env.Make(Reduction.Default)
module Printer = E.Printer
module ErrorHandler = Errors.Make(E)

let print fmt =
  Format.kfprintf (fun _ -> print_newline () ) Format.std_formatter fmt

let handle_entry sg e =
  match e with
  | Decl(lc,id,st,ty) ->
    E.declare sg lc id st ty;
    Format.printf "%a is declared.@." pp_ident id
  | Def(lc,id,op,ty,te) ->
    E.define sg lc id op te ty;
    Format.printf "%a is defined.@." pp_ident id
  | Rules(_,rs) ->
    let _ = E.add_rules sg rs in
    List.iter (fun r -> print "%a" Rule.pp_untyped_rule r) rs
  | Eval(lc,red,te) ->
    let te = E.reduction sg ~red te in
    Format.printf "%a@." (Printer.print_term sg) te
  | Infer(_,red,te) ->
    let  ty = E.infer sg te in
    let rty = E.reduction sg ~red ty in
    Format.printf "%a@." (Printer.print_term sg) rty
  | Check(l, assrt, neg, Convert(t1,t2)) ->
    let succ = (E.are_convertible sg t1 t2) <> neg in
    ( match succ, assrt with
      | true , false -> Format.printf "YES@."
      | true , true  -> ()
      | false, false -> Format.printf "NO@."
      | false, true  -> raise (Env.EnvError (Some (E.get_name sg),l,Env.AssertError)) )
  | Check(l, assrt, neg, HasType(te,ty)) ->
    let succ = try E.check sg te ty; not neg with _ -> neg in
    ( match succ, assrt with
      | true , false -> Format.printf "YES@."
      | true , true  -> ()
      | false, false -> Format.printf "NO@."
      | false, true  -> raise (Env.EnvError (Some (E.get_name sg),l,Env.AssertError)) )
  | DTree(lc,m,v) ->
    let m = match m with None -> E.get_name sg | Some m -> m in
    let cst = mk_name m v in
    let forest = E.get_dtree sg lc cst in
    Format.printf "GDTs for symbol %a:\n%a" pp_name cst Dtree.pp_dforest forest
  | Print(_,s) -> Format.printf "%s@." s
  | Name _     -> Format.printf "\"#NAME\" directive ignored.@."
  | Require _  -> Format.printf "\"#REQUIRE\" directive ignored.@."

let  _ =
  let sg = E.init "<toplevel>" in
  let md = E.get_name sg in
  let str = Parse_channel.from md stdin in
  Format.printf "\tDedukti (%s)@.@." Version.version;
  while true do
    Format.printf ">> ";
    try handle_entry sg (read str) with
    | End_of_file -> exit 0
    | Env.EnvError (md,l,Env.ParseError s)->
      ErrorHandler.fail_env_error sg (md,l,Env.ParseError s)
    | e -> Format.eprintf "Uncaught exception %S@." (Printexc.to_string e)
  done
