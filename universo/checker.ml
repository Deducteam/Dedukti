open Basic
open Pp
open Rule
open Entry

let mk_entry md e =
    match e with
    | Decl (lc, id, st, ty) -> (
      match Env.declare lc id st ty with
      | OK () -> ()
      | Err e -> Errors.fail_env_error (Env.get_signature ()) e )
    | Def (lc, id, opaque, ty, te) -> (
        let define = if opaque then Uenv.define_op else Uenv.define in
        match define lc id te ty with
        | OK () -> ()
        | Err e -> Errors.fail_env_error (Env.get_signature ()) e )
    | Rules rs -> (
        let open Rule in
        match Env.add_rules rs with
        | OK rs -> ()
        | Err e -> Errors.fail_env_error (Env.get_signature ()) e )
    | Eval (_, red, te) -> (
      match Env.reduction ~red te with
      | OK te -> Format.printf "%a@." Pp.print_term te
      | Err e -> Errors.fail_env_error (Env.get_signature ()) e )
    | Infer (_, red, te) -> (
      match Env.infer te with
      | OK ty -> (
        match Env.reduction ~red ty with
        | OK ty -> Format.printf "%a@." Pp.print_term ty
        | Err e -> Errors.fail_env_error (Env.get_signature ()) e )
      | Err e -> Errors.fail_env_error (Env.get_signature ()) e )
    | Check (_, assrt, neg, test) -> (
      match test with
      | Convert (t1, t2) -> (
        match Env.are_convertible t1 t2 with
        | OK ok when ok = not neg -> if not assrt then Format.printf "YES@."
        | OK _ when assrt -> failwith "Assertion failed."
        | OK _ -> Format.printf "NO@."
        | Err e -> Errors.fail_env_error (Env.get_signature ()) e )
      | HasType (te, ty) ->
        match Env.check te ty with
        | OK () when not neg -> if not assrt then Format.printf "YES@."
        | Err _ when neg -> if not assrt then Format.printf "YES@."
        | OK () when assrt -> failwith "Assertion failed."
        | Err _ when assrt -> failwith "Assertion failed."
        | _ -> Format.printf "NO@." )
    | DTree (lc, m, v) -> (
        let m = match m with None -> Env.get_name () | Some m -> m in
        let cst = mk_name m v in
        match Env.get_dtree lc cst with
        | OK forest ->
            Format.printf "GDTs for symbol %a:@.%a" pp_name cst
              Dtree.pp_dforest forest
        | Err e -> Errors.fail_signature_error e )
    | Print (_, s) -> Format.printf "%s@." s
    | Name (_, n) ->
        if not (mident_eq n md) then
          Printf.eprintf "[Warning] invalid #NAME directive ignored.\n%!"
    | Require (lc, md) ->
      match Env.import lc md with
      | OK () -> ()
      | Err e -> Errors.fail_signature_error e
