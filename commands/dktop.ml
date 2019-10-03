open Kernel
open Parsing
open Api

open Basic
open Parser
open Entry

module E = Env.Make(Reduction.Default)
module Printer = E.Printer
module ErrorHandler = Errors.Make(E)

let print fmt =
  Format.kfprintf (fun _ -> print_newline () ) Format.std_formatter fmt

let handle_entry md e =
  match e with
  | Decl(lc,id,st,ty) ->
    E.declare lc id st ty;
    Format.printf "%a is declared.@." pp_ident id
  | Def(lc,id,op,ty,te) ->
    E.define lc id op te ty;
    Format.printf "%a is defined.@." pp_ident id
  | Rules(_,rs) ->
    let _ = E.add_rules rs in
    List.iter (fun r -> print "%a" Rule.pp_part_typed_rule r) rs
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
      | false, true  -> raise (Env.EnvError (Some md,l,Env.AssertError)) )
  | Check(l, assrt, neg, HasType(te,ty)) ->
    let succ = try E.check te ty; not neg with _ -> neg in
    ( match succ, assrt with
      | true , false -> Format.printf "YES@."
      | true , true  -> ()
      | false, false -> Format.printf "NO@."
      | false, true  -> raise (Env.EnvError (Some md,l,Env.AssertError)) )
  | DTree(lc,m,v) ->
    let m = match m with None -> E.get_name () | Some m -> m in
    let cst = mk_name m v in
    let forest = E.get_dtree lc cst in
    Format.printf "GDTs for symbol %a:\n%a" pp_name cst Dtree.pp_dforest forest
  | Print(_,s) -> Format.printf "%s@." s
  | Name _     -> Format.printf "\"#NAME\" directive ignored.@."
  | Require _  -> Format.printf "\"#REQUIRE\" directive ignored.@."

let  _ =
  let md = E.init "<toplevel>" in
  let str = Parse_channel.from md stdin in
  Format.printf "\tDedukti (%s)@.@." Version.version;
  while true do
    Format.printf ">> ";
    try handle_entry md (read str) with
    | End_of_file -> exit 0
    | e -> ErrorHandler.graceful_fail None e
  done
