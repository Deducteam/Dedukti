open Basic
open Parser
open Entry
open Env
open Errors

let print fmt =
  Format.kfprintf (fun _ -> print_newline () ) Format.std_formatter fmt

let handle_entry e =
  match e with
  | Decl(lc,id,scope,st,ty) ->
    Env.declare lc id scope st ty;
    Format.printf "%a is declared.@." pp_ident id
  | Def(lc,id,op,ty,te) ->
    Env.define lc id op te ty;
    Format.printf "%a is defined.@." pp_ident id
  | Rules(_,rs) ->
    let _ = Env.add_rules rs in
    List.iter (fun r -> print "%a" Rule.pp_untyped_rule r) rs
  | Eval(lc,red,te) ->
    let te = Env.reduction ~red te in
    Format.printf "%a@." Pp.print_term te
  | Infer(_,red,te) ->
    let  ty = Env.infer te in
    let rty = Env.reduction ~red ty in
    Format.printf "%a@." Pp.print_term rty
  | Check(l, assrt, neg, Convert(t1,t2)) ->
    let succ = (Env.are_convertible t1 t2) <> neg in
    ( match succ, assrt with
      | true , false -> Format.printf "YES@."
      | true , true  -> ()
      | false, false -> Format.printf "NO@."
      | false, true  -> raise (Env.EnvError (l, Env.AssertError)) )
  | Check(l, assrt, neg, HasType(te,ty)) ->
    let succ = try Env.check te ty; not neg with _ -> neg in
    ( match succ, assrt with
      | true , false -> Format.printf "YES@."
      | true , true  -> ()
      | false, false -> Format.printf "NO@."
      | false, true  -> raise (Env.EnvError (l, Env.AssertError)) )
  | DTree(lc,m,v) ->
    let m = match m with None -> Env.get_name () | Some m -> m in
    let cst = mk_name m v in
    let forest = Env.get_dtree lc cst in
    Format.printf "GDTs for symbol %a:\n%a" pp_name cst Dtree.pp_dforest forest
  | Print(_,s) -> Format.printf "%s@." s
  | Name _     -> Format.printf "\"#NAME\" directive ignored.@."
  | Require _  -> Format.printf "\"#REQUIRE\" directive ignored.@."

let  _ =
  let md = Env.init "<toplevel>" in
  let str = Parse_channel.from md stdin in
  Format.printf "\tDedukti (%s)@.@." Version.version;
  while true do
    Format.printf ">> ";
    try handle_entry (read str) with
    | End_of_file -> exit 0
    | Env.EnvError (l, Env.ParseError s) -> Errors.fail_env_error l (Env.ParseError s)
    | e ->
      Format.eprintf "Uncaught exception %S@." (Printexc.to_string e)
  done
