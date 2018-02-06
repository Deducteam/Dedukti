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
  | Conv of term*term
  | Inhabit of term*term
  | ConvNot of term*term
  | InhabitNot of term*term
  | Infer of term
  | AssertConv of term*term
  | AssertInhabit of term*term
  | AssertConvNot of term*term
  | AssertInhabitNot of term*term
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
  | Conv (te1,te2)  ->
        ( match Env.are_convertible te1 te2 with
            | OK true -> Format.printf "YES@."
            | OK false -> Format.printf "NO@."
            | Err e -> Errors.fail_env_error e )
  | Inhabit (te,ty) ->
        ( match Env.check te ty with
            | OK ()                -> Format.printf "YES@."
            | Err (EnvErrorType _) -> Format.printf "NO@."
            | Err e                -> Errors.fail_env_error e )
  | ConvNot (te1,te2)  ->
        ( match Env.are_convertible te1 te2 with
            | OK true -> Format.printf "NO@."
            | OK false -> Format.printf "YES@."
            | Err e -> Errors.fail_env_error e )
  | InhabitNot (te,ty) ->
        ( match Env.check te ty with
            | OK ()                -> Format.printf "NO@."
            | Err (EnvErrorType _) -> Format.printf "YES@."
            | Err e                -> Errors.fail_env_error e )
  | AssertConv (te1,te2) ->
        ( match Env.are_convertible te1 te2 with
            | OK true -> ()
            | OK false ->
              Errors.fail_env_error
                (Env.EnvErrorType
                   (Unconvertible (lc,te1,te2))
                )
            | Err e -> Errors.fail_env_error e )
  | AssertInhabit (te,ty) ->
        ( match Env.check te ty with
            | OK ()                -> ()
            | Err e                -> Errors.fail_env_error e )
  | AssertConvNot (te1,te2)  ->
        ( match Env.are_convertible te1 te2 with
            | OK true -> 
              Errors.fail_env_error
                (Env.EnvErrorType
                   (Convertible (lc,te1,te2))
                )
            | OK false -> ()
            | Err e -> Errors.fail_env_error e )
  | AssertInhabitNot (te,ty) ->
        ( match Env.check te ty with
            | OK ()                -> 
              Errors.fail_env_error
                (Env.EnvErrorType
                   (Inhabit (lc,te,ty))
                )
            | Err (EnvErrorType _) -> ()
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
  | Conv (t1,t2)             -> Format.fprintf out "#CHECK@ %a==@ %a."
                          print_term t1 print_term t2
  | ConvNot (t1,t2)          -> Format.fprintf out "#CHECKNOT@ %a==@ %a."
                                  print_term t1 print_term t2
  | Inhabit (te,ty)          -> Format.fprintf out "#CHECK@ %a::@ %a."
                                  print_term te print_term ty
  | InhabitNot (te,ty)       -> Format.fprintf out "#CHECKNOT@ %a::@ %a."
                                  print_term te print_term ty
  | AssertConv (t1,t2)       -> Format.fprintf out "#ASSERT@ %a==@ %a."
                                  print_term t1 print_term t2
  | AssertConvNot (t1,t2)    -> Format.fprintf out "#ASSERTNOT@ %a==@ %a."
                                  print_term t1 print_term t2
  | AssertInhabit (te,ty)    -> Format.fprintf out "#ASSERT@ %a::@ %a."
                                  print_term te print_term ty
  | AssertInhabitNot (te,ty) -> Format.fprintf out "#ASSERTNOT@ %a::@ %a."
                                  print_term te print_term ty        
  | Infer te         -> Format.fprintf out "#INFER@ %a." print_term te
  | Gdt (m0,v)       ->
      begin match m0 with
      | None -> Format.fprintf out "#GDT@ %a." print_ident v
      | Some m -> Format.fprintf out "#GDT@ %a.%a." print_ident m print_ident v
      end
  | Print str         -> Format.fprintf out "#PRINT \"%s\"." str
  | Other (cmd,_)     -> failwith ("Unknown command '"^cmd^"'.\n")
