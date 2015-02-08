open Basics
open Term
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

let print s= print_string s; print_newline ()

let mk_command lc = function
  | Whnf te          ->
      let jdg = whnf (inference te) in
        Printf.fprintf stdout "%a\n" Pp.pp_term jdg.te
  | Hnf te           ->
      let jdg = hnf (inference te) in
        Printf.fprintf stdout "%a\n" Pp.pp_term jdg.te
  | Snf te           ->
      let jdg = snf (inference te) in
        Printf.fprintf stdout "%a\n" Pp.pp_term jdg.te
  | OneStep te       ->
      let jdg = one (inference te) in
        Printf.fprintf stdout "%a\n" Pp.pp_term jdg.te
  | Conv (te1,te2)  ->
      let j1 = inference te1 in
      let j2 = inference te2 in
        if conv_test j1 j2 then print "YES"
        else print "NO"
  | Check (te,ty) ->
      let jty = inference ty in
      let jte = inference te in
        if check_test jte jty then print "YES"
        else print "NO"
  | Infer te         ->
      let jdg = inference te in
        Printf.fprintf stdout "%a\n" Pp.pp_term jdg.ty
  | Gdt (m0,v)         ->
      let m = match m0 with None -> Env.get_name () | Some m -> m in
        ( match Env.get_dtree lc m v with
            | Signature.DoD_Dtree (i,g) ->
                Printf.fprintf stdout "%a\n" Pp.pp_rw (m,v,i,g)
            | _                 -> print "No GDT." )
  | Print str         -> output_string stdout str
  | Other (cmd,_)     -> prerr_string ("Unknown command '"^cmd^"'.\n")
