open Basic
open Term
open Pp
open Typing

type command =
  (* Reduction *)
  | Whnf of term
  | Hnf of term
  | Snf of term
  | OneStep of term
  (*Typing*)
  | Conv of bool*bool*term*term
  | Inhabit of bool*bool*term*term
  | Infer of term
  (* Misc *)
  | Gdt of ident option*ident
  | Print of string
  | Other of string*term list

let print s = Format.printf "%s\n" s

let mk_command lc =
  let open Pp in
  function
  | Whnf te          ->
    ( match Env.whnf te with
      | OK te -> Format.printf "%a\n" print_term te
      | Err e -> Errors.fail_env_error e )
  | Hnf te           ->
    ( match Env.hnf te with
      | OK te -> Format.printf "%a\n" print_term te
      | Err e -> Errors.fail_env_error e )
  | Snf te           ->
    ( match Env.snf te with
      | OK te -> Format.printf "%a\n" print_term te
      | Err e -> Errors.fail_env_error e )
  | OneStep te       ->
    ( match Env.one te with
      | OK (Some te) -> Format.printf "%a\n" print_term te
      | OK None -> Format.printf "%a\n" print_term te
      | Err e -> Errors.fail_env_error e )
  | Conv (expected,asserted,te1,te2)  ->
    ( match Env.are_convertible te1 te2 with
      | OK b ->
        begin
          if b=expected
          then
            if asserted then () else Format.printf "YES@."
          else
            if asserted
            then
              let chose_error=
                if expected
                then Unconvertible (lc,te1,te2)
                else Convertible (lc,te1,te2)
              in
              Errors.fail_env_error (Env.EnvErrorType chose_error)
            else
              Format.printf "NO@."
          end
      | Err e -> Errors.fail_env_error e )
  | Inhabit (expected,asserted,te,ty) ->
    ( match Env.check te ty with
      | OK ()                ->
        begin
          if expected
          then
            if asserted then () else Format.printf "YES@."
          else
            if asserted
            then
              Errors.fail_env_error (Env.EnvErrorType (Inhabit (lc,te,ty)))
            else
              Format.printf "NO@."
          end
      | Err (EnvErrorType e) ->
        begin
          if expected
          then
            if asserted
            then Errors.fail_env_error (EnvErrorType e)
            else Format.printf "NO@."
          else
            if asserted
            then
              ()
            else
              Format.printf "YES@."
          end
      | Err e                -> Errors.fail_env_error e )
  | Infer te         ->
    ( match Env.infer te with
      | OK ty -> Format.printf "%a\n" print_term ty
      | Err e -> Errors.fail_env_error e )
  | Gdt (m0,v)         ->
    let m = match m0 with None -> Env.get_name () | Some m -> m in
    ( match Env.get_dtree lc m v with
      | OK (Some (i,g)) ->
        Printf.fprintf stdout "%a\n" Rule.pp_rw (m,v,i,g)
      | _ -> print "No GDT." )
  | Print str         -> Format.printf "%s" str
  | Other (cmd,_)     -> prerr_string ("Unknown command '"^cmd^"'.\n")

let print_command out c =
  match c with
  | Whnf te                  -> Format.fprintf out "#WHNF@ %a." print_term te
  | Hnf te                   -> Format.fprintf out "#HNF@ %a." print_term te
  | Snf te                   -> Format.fprintf out "#SNF@ %a." print_term te
  | OneStep te               -> Format.fprintf out "#STEP@ %a." print_term te
  | Conv (exp,ass,t1,t2) when exp && (not ass)
                             -> Format.fprintf out "#CHECK@ %a==@ %a."
                                  print_term t1 print_term t2
  | Conv (exp,ass,t1,t2) when (not exp) && (not ass)
                             -> Format.fprintf out "#CHECKNOT@ %a==@ %a."
                                  print_term t1 print_term t2
  | Inhabit (exp,ass,te,ty) when exp && (not ass)
                             -> Format.fprintf out "#CHECK@ %a::@ %a."
                                  print_term te print_term ty
  | Inhabit (exp,ass,te,ty) when (not exp) && (not ass)
                             -> Format.fprintf out "#CHECKNOT@ %a::@ %a."
                                  print_term te print_term ty
  | Conv (exp,ass,t1,t2) when exp && ass
                             -> Format.fprintf out "#ASSERT@ %a==@ %a."
                                  print_term t1 print_term t2
  | Conv (exp,ass,t1,t2) when (not exp) && ass
                             -> Format.fprintf out "#ASSERTNOT@ %a==@ %a."
                                  print_term t1 print_term t2
  | Inhabit (exp,ass,te,ty) when exp && ass
                             -> Format.fprintf out "#ASSERT@ %a::@ %a."
                                  print_term te print_term ty
  | Inhabit (exp,ass,te,ty) when (not exp) && ass
                             -> Format.fprintf out "#ASSERTNOT@ %a::@ %a."
                                  print_term te print_term ty
  | Inhabit (_,_,_,_)-> assert false
  | Conv (_,_,_,_)   -> assert false
  | Infer te         -> Format.fprintf out "#INFER@ %a." print_term te
  | Gdt (m0,v)       ->
    begin match m0 with
      | None -> Format.fprintf out "#GDT@ %a." print_ident v
      | Some m -> Format.fprintf out "#GDT@ %a.%a." print_ident m print_ident v
    end
  | Print str         -> Format.fprintf out "#PRINT \"%s\"." str
  | Other (cmd,_)     -> failwith ("Unknown command '"^cmd^"'.\n")
