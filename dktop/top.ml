open Basic
open Cmd

let print fmt =
  Format.kfprintf (fun _ -> print_newline () ) Format.std_formatter fmt

let mk_prelude _ _ = failwith "Top.mk_prelude"

let mk_declaration lc id st pty =
  match Env.declare lc id st pty with
    | OK () -> Format.printf "%a is declared.@." pp_ident id
    | Err e -> Errors.fail_env_error e

let mk_definition lc id pty_opt pte =
  match Env.define lc id pte pty_opt with
    | OK () -> Format.printf "%a is defined.@." pp_ident id
    | Err e -> Errors.fail_env_error e

let mk_opaque lc id pty_opt pte =
  match Env.define_op lc id pte pty_opt with
    | OK () -> Format.printf "%a is declared.@." pp_ident id
    | Err e -> Errors.fail_env_error e

let mk_rules lst =
  match Env.add_rules lst with
    | OK _ -> List.iter (fun r -> print "%a" Rule.pp_untyped_rule r) lst
    | Err e -> Errors.fail_env_error e

let mk_command lc = function
  | Whnf te          ->
      ( match Env.reduction Reduction.Whnf  te with
          | OK te -> Format.printf "%a@." Pp.print_term te
          | Err e -> Errors.fail_env_error e )
  | Hnf te           ->
      ( match Env.reduction Reduction.Hnf te with
          | OK te -> Format.printf "%a@." Pp.print_term te
          | Err e -> Errors.fail_env_error e )
  | Snf te           ->
      ( match Env.reduction Reduction.Snf te with
          | OK te -> Format.printf "%a@." Pp.print_term te
          | Err e -> Errors.fail_env_error e )
  | OneStep te       ->
      ( match Env.reduction (Reduction.NSteps 1) te with
          | OK te -> Format.printf "%a@." Pp.print_term te
          | Err e -> Errors.fail_env_error e )
  | NSteps (n,te)    ->
      ( match Env.reduction (Reduction.NSteps n) te with
          | OK te -> Format.printf "%a@." Pp.print_term te
          | Err e -> Errors.fail_env_error e )
  | Conv (te1,te2)  ->
        ( match Env.are_convertible te1 te2 with
            | OK true -> Format.printf "YES@."
            | OK false -> Format.printf "NO@."
            | Err e -> Errors.fail_env_error e )
  | Check (te,ty) ->
        ( match Env.check te ty with
            | OK () -> Format.printf "YES@."
            | Err e -> Errors.fail_env_error e )
  | Infer te         ->
      ( match Env.infer (Reduction.NSteps 0) te with
          | OK ty -> Format.printf "%a@." Pp.print_term ty
          | Err e -> Errors.fail_env_error e )
  | InferSnf te         ->
      ( match Env.infer Reduction.Snf te with
          | OK ty -> Format.printf "%a@." Pp.print_term ty
          | Err e -> Errors.fail_env_error e )
  | Gdt (m0,v)         ->
    let m = match m0 with None -> Env.get_name () | Some m -> m in
    let cst = mk_name m v in
        ( match Env.get_dtree lc cst with
            | OK (Some (i,g)) ->
                Format.printf "%a\n" Dtree.pp_rw (m,v,i,g)
            | _ -> Format.printf "No GDT.@." )
  | Print str         -> Format.printf "%s@." str
  | Other (cmd,_)     -> Format.eprintf "Unknown command '%s'.@." cmd

let mk_ending _ = ()
