open Basic
open Parser

let print fmt =
  Format.kfprintf (fun _ -> print_newline () ) Format.std_formatter fmt

let handle_entry e =
  match e with
  | Decl(lc,id,st,ty)       ->
      begin
        match Env.declare lc id st ty with
        | OK () -> Format.printf "%a is declared.@." pp_ident id
        | Err e -> Errors.fail_env_error e
      end
  | Def(lc,id,op,ty,te)     ->
      begin
        let define = if op then Env.define_op else Env.define in
        match define lc id te ty with
        | OK () -> Format.printf "%a is defined.@." pp_ident id
        | Err e -> Errors.fail_env_error e
      end
  | Rules(rs)               ->
      begin
        match Env.add_rules rs with
        | OK _  -> List.iter (fun r -> print "%a" Rule.pp_untyped_rule r) rs
        | Err e -> Errors.fail_env_error e
      end
  | Eval(lc,red,te)         ->
      begin
        match Env.reduction ~red te with
        | OK te -> Format.printf "%a@." Pp.print_term te
        | Err e -> Errors.fail_env_error e
      end
  | Check(_,assrt,neg,test) ->
      begin
        match test with
        | Convert(t1,t2) ->
            begin
              match Env.are_convertible t1 t2 with
              | OK ok when ok = not neg -> Format.printf "YES@."
              | OK _  when assrt        -> failwith "Assertion failed."
              | OK _                    -> Format.printf "NO@."
              | Err e                   -> Errors.fail_env_error e
            end
        | HasType(te,ty) ->
            begin
              match Env.check te ty with
              | OK () when not neg -> Format.printf "YES@."
              | Err _ when neg     -> Format.printf "YES@."
              | OK () when assrt   -> failwith "Assertion failed."
              | Err _ when assrt   -> failwith "Assertion failed."
              | _                  -> Format.printf "NO@."
            end
      end
  | Infer(_,red,te)         ->
      begin
        match Env.infer te with
        | OK ty ->
            begin
              match Env.reduction ~red ty with
              | OK ty -> Format.printf "%a@." Pp.print_term ty
              | Err e -> Errors.fail_env_error e
            end
        | Err e -> Errors.fail_env_error e
      end
  | DTree(lc,m,v)           ->
      begin
        let m = match m with None -> Env.get_name () | Some m -> m in
        let cst = mk_name m v in
        match Env.get_dtree lc cst with
        | OK (Some(i,g)) -> Format.printf "%a\n" Dtree.pp_rw (cst,i,g)
        | _              -> Format.printf "No GDT.@."
      end
  | Print(_,s)          ->
      Format.printf "%s@." s
  | Name(_,_)           ->
      Format.printf "\"#NAME\" directive ignored.@."

let  _ =
  let md = Basic.mk_mident "<toplevel>" in
  Env.init md;
  let str = from_channel md stdin in
  Printf.printf "\tDedukti (%s)\n\n%!" Version.version;
  while true do
    Printf.printf ">> %!";
    try handle_entry (read str) with
    | End_of_file      -> exit 0
    | Parse_error(_,s) -> Printf.eprintf "Parse error: %s\n%!" s
    | e                ->
        Printf.eprintf "Uncaught exception %S\n%!" (Printexc.to_string e)
  done
