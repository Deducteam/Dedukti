open Basic
open Parser
open Entry

let print fmt =
  Format.kfprintf (fun _ -> print_newline () ) Format.std_formatter fmt

let handle_entry e =
  match e with
  | Decl(lc,id,st,ty) ->
    Errors.fail_if_err (Env.declare lc id st ty);
    Format.printf "%a is declared.@." pp_ident id
  | Def(lc,id,op,ty,te) ->
    Errors.fail_if_err (Env.define ~loc:lc id op te ty);
    Format.printf "%a is defined.@." pp_ident id
  | Rules(rs) ->
    let _ = Errors.fail_if_err (Env.add_rules rs) in
    List.iter (fun r -> print "%a" Rule.pp_untyped_rule r) rs
  | Eval(lc,red,te) ->
    let te = Errors.fail_if_err (Env.reduction ~red te) in
    Format.printf "%a@." Pp.print_term te
  | Check(l,assrt,neg,test) ->
    begin
      match test with
      | Convert(t1,t2) ->
        begin
          match Env.are_convertible t1 t2 with
          | OK ok when ok = not neg -> Format.printf "YES@."
          | OK _  when assrt        -> failwith (Format.sprintf "At line %d: Assertion failed." (fst (of_loc l)))
          | OK _                    -> Format.printf "NO@."
          | Err e                   -> Errors.fail_env_error e
        end
      | HasType(te,ty) ->
        begin
          match Env.check te ty with
          | OK () when not neg -> Format.printf "YES@."
          | Err _ when neg     -> Format.printf "YES@."
          | OK () when assrt   -> failwith (Format.sprintf "At line %d: Assertion failed." (fst (of_loc l)))
          | Err _ when assrt   -> failwith (Format.sprintf "At line %d: Assertion failed." (fst (of_loc l)))
          | _                  -> Format.printf "NO@."
        end
    end
  | Infer(_,red,te) ->
    let  ty = Errors.fail_if_err (Env.infer te) in
    let rty = Errors.fail_if_err (Env.reduction ~red ty) in
    Format.printf "%a@." Pp.print_term rty
  | DTree(lc,m,v) ->
    let m = match m with None -> Env.get_name () | Some m -> m in
    let cst = mk_name m v in
    let forest = Errors.fail_if_err (Env.get_dtree lc cst) in
    Format.printf "GDTs for symbol %a:\n%a" pp_name cst Dtree.pp_dforest forest
  | Print(_,s)   -> Format.printf "%s@." s
  | Name(_,_)    -> Format.printf "\"#NAME\" directive ignored.@."
  | Require(_,_) -> Format.printf "\"#REQUIRE\" directive ignored.@."

let  _ =
  let md = Env.init "<toplevel>" in
  let str = from_channel md stdin in
  Format.printf "\tDedukti (%s)@.@." Version.version;
  while true do
    Format.printf ">> ";
    try handle_entry (read str) with
    | End_of_file      -> exit 0
    | Parse_error(_,s) -> Format.eprintf "Parse error: %s@." s
    | e                ->
      Format.eprintf "Uncaught exception %S@." (Printexc.to_string e)
  done
