open Basic
open Parser
open Entry
open Env
open Errors

let print fmt =
  Format.kfprintf (fun _ -> print_newline () ) Format.std_formatter fmt

let handle_entry env e =
  match e with
  | Decl(lc,id,st,ty) ->
    Env.declare env lc id st ty;
    Format.printf "%a is declared.@." pp_ident id
  | Def(lc,id,op,ty,te) ->
    Env.define env lc id op te ty;
    Format.printf "%a is defined.@." pp_ident id
  | Rules(_,rs) ->
    let _ = Env.add_rules env rs in
    List.iter (fun r -> print "%a" Rule.pp_untyped_rule r) rs
  | Eval(lc,red,te) ->
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
      | false, true  -> raise (Env.EnvError (env,l, Env.AssertError)) )
  | Check(l, assrt, neg, HasType(te,ty)) ->
    let succ = try Env.check env te ty; not neg with _ -> neg in
    ( match succ, assrt with
      | true , false -> Format.printf "YES@."
      | true , true  -> ()
      | false, false -> Format.printf "NO@."
      | false, true  -> raise (Env.EnvError (env,l, Env.AssertError)) )
  | DTree(lc,m,v) ->
    let m = match m with None -> Env.get_md env | Some m -> m in
    let cst = mk_name m v in
    let forest = Env.get_dtree env lc cst in
    Format.printf "GDTs for symbol %a:\n%a" pp_name cst Dtree.pp_dforest forest
  | Print(_,s) -> Format.printf "%s@." s
  | Name _     -> Format.printf "\"#NAME\" directive ignored.@."
  | Require _  -> Format.printf "\"#REQUIRE\" directive ignored.@."

let  _ =
  let env = Env.init "<toplevel>" in
  let md = Env.get_md env in
  let str = from_channel md stdin in
  Format.printf "\tDedukti (%s)@.@." Version.version;
  while true do
    Format.printf ">> ";
    try handle_entry env (read str) with
    | End_of_file -> exit 0
    | Env.EnvError (env, l, Env.ParseError s) -> Errors.fail_env_error env l (Env.ParseError s)
    | e ->
      Format.eprintf "Uncaught exception %S@." (Printexc.to_string e)
  done
