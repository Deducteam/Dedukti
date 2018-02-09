open Basic
open Cmd

(* ********************************* *)

let verbose = ref false

let eprint lc fmt =
  if !verbose then (
  let (l,c) = of_loc lc in
    Format.eprintf "line:%i column:%i " l c;
    Format.kfprintf (fun _ -> prerr_newline () ) Format.err_formatter fmt
  ) else
    Format.ifprintf Format.err_formatter fmt

(* ********************************* *)

let mk_prelude lc name =
  eprint lc "Module name is '%a'." pp_mident name;
  Env.init name;
  Confluence.initialize ()

let mk_declaration lc id st pty : unit =
  eprint lc "Declaration of constant '%a'." pp_ident id;
  match Env.declare lc id st pty with
    | OK () -> ()
    | Err e -> Errors.fail_env_error e

let mk_definition lc id pty_opt pte : unit =
  eprint lc "Definition of symbol '%a'." pp_ident id ;
  match Env.define lc id pte pty_opt with
    | OK () -> ()
    | Err e -> Errors.fail_env_error e

let mk_opaque lc id pty_opt pte =
  eprint lc "Opaque definition of symbol '%a'." pp_ident id ;
  match Env.define_op lc id pte pty_opt with
    | OK () -> ()
    | Err e -> Errors.fail_env_error e

let get_infos = function
  | Rule.Pattern (l,cst,_) -> (l,cst)
  | _ -> (dloc,mk_name (mk_mident "") qmark)

let mk_rules = Rule.( function
  | [] -> ()
  | (rule::_) as lst ->
    begin
      let (l,cst) = get_infos rule.pat in
      eprint l "Adding rewrite rules for '%a'" pp_name cst;
      match Env.add_rules lst with
      | OK lst2 ->
        List.iter ( fun rule ->
            eprint (get_loc_pat rule.pat) "%a" pp_typed_rule rule
          ) lst2 ;
      | Err e -> Errors.fail_env_error e
    end
  )

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
  | Conv (expected,asserted,te1,te2)  ->
    ( match Env.are_convertible te1 te2 with
      | OK b ->
        begin
          if b=expected
          then
            if asserted
            then ()
            else
              Format.printf "YES@.As expected %a and %a are %s convertible@."
                Pp.print_term te1 Pp.print_term te2
                (if asserted then "" else "not")
          else
            if asserted
            then
              let chose_error=
                if expected
                then Typing.Unconvertible (lc,te1,te2)
                else Typing.Convertible (lc,te1,te2)
              in
              Errors.fail_env_error (Env.EnvErrorType chose_error)
            else
              Format.printf "NO@.%a and %a are %s convertible@."
                Pp.print_term te1 Pp.print_term te2 (if asserted then "not" else "")
          end
      | Err e -> Errors.fail_env_error e )
  | Inhabit (expected,asserted,te,ty) ->
    ( match Env.check te ty with
      | OK ()                ->
        begin
          if expected
          then
            if asserted then () else Format.printf "YES@.As expected %a : %a@."
                Pp.print_term te Pp.print_term ty
          else
            if asserted
            then
              Errors.fail_env_error (Env.EnvErrorType (Inhabit (lc,te,ty)))
            else
              Format.printf "NO@.%a does not have type %a@."
                Pp.print_term te Pp.print_term ty
          end
      | Err (EnvErrorType e) ->
        begin
          if expected
          then
            if asserted
            then Errors.fail_env_error (EnvErrorType e)
            else Format.printf "NO@.%a : %a@."
                Pp.print_term te Pp.print_term ty
          else
            if asserted
            then
              ()
            else
              Format.printf "YES@.%a does not have type %a@."
                Pp.print_term te Pp.print_term ty
          end
      | Err e                -> Errors.fail_env_error e )
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
                Format.printf "%a\n" Dtree.pp_rw (cst,i,g)
            | _ -> Format.printf "No GDT.@." )
  | Print str         -> Format.printf "%s@." str
  | Require m         ->
    ( match Env.import lc m with
      | OK () -> ()
      | Err e -> Errors.fail_signature_error e )
  | Other (cmd,_)     -> Format.eprintf "Unknown command '%s'.@." cmd

let export = ref false

let mk_ending () =
  ( if !export then
    if not (Env.export ()) then
      Errors.fail dloc "Fail to export module '%a'." pp_mident (Env.get_name ()) );
  Confluence.finalize ()
