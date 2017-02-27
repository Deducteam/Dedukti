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
  | Conv of term*term
  (*Typing*)
  | Check of term*term
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
  | Conv (te1,te2)  ->
        ( match Env.are_convertible te1 te2 with
            | OK true -> print "YES"
            | OK false -> print "NO"
            | Err e -> Errors.fail_env_error e )
  | Check (te,ty) ->
        ( match Env.check te ty with
            | OK () -> print "YES"
            | Err e -> Errors.fail_env_error e )
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
  | Whnf te          -> Format.fprintf out "#WHNF@ %a." print_term te
  | Hnf te           -> Format.fprintf out "#HNF@ %a." print_term te
  | Snf te           -> Format.fprintf out "#SNF@ %a." print_term te
  | OneStep te       -> Format.fprintf out "#STEP@ %a." print_term te
  | Conv (te1,te2)   -> Format.fprintf out "#CONV@ %a,@ %a." print_term te1 print_term te2
  | Check (te,ty)    -> Format.fprintf out "#CHECK@ %a,@ %a." print_term te print_term ty
  | Infer te         -> Format.fprintf out "#INFER@ %a." print_term te
  | Gdt (m0,v)       ->
      begin match m0 with
      | None -> Format.fprintf out "#GDT@ %a." print_ident v
      | Some m -> Format.fprintf out "#GDT@ %a.%a." print_ident m print_ident v
      end
  | Print str         -> Format.fprintf out "#PRINT \"%s\"." str
  | Other (cmd,_)     -> failwith ("Unknown command '"^cmd^"'.\n")
