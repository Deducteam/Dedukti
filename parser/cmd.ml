open Basics
open Term

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
      let jdg = Judgment.whnf (Typing.inference te) in
        Printf.fprintf stdout "%a\n" Pp.pp_term jdg.Judgment.te
  | Hnf te           ->
      let jdg = Judgment.hnf (Typing.inference te) in
        Printf.fprintf stdout "%a\n" Pp.pp_term jdg.Judgment.te
  | Snf te           ->
      let jdg = Judgment.snf (Typing.inference te) in
        Printf.fprintf stdout "%a\n" Pp.pp_term jdg.Judgment.te
  | OneStep te       ->
      let jdg = Judgment.one (Typing.inference te) in
        Printf.fprintf stdout "%a\n" Pp.pp_term jdg.Judgment.te
  | Conv (te1,te2)  ->
      let j1 = Typing.inference te1 in
      let j2 = Typing.inference te2 in
        if Judgment.conv j1 j2 then print "YES"
        else print "NO"
  | Check (te,ty) ->
      let jty = Typing.inference ty in
      let jte = Typing.inference te in
        if Judgment.check jte jty then print "YES"
        else print "NO"
  | Infer te         ->
      let jdg = Typing.inference te in
        Printf.fprintf stdout "%a\n" Pp.pp_term jdg.Judgment.ty
  | Gdt (m0,v)         ->
      let m = match m0 with None -> Env.get_name () | Some m -> m in
        ( match Env.get_dtree lc m v with
            | Env.DoD_Dtree (i,g) ->
                Printf.fprintf stdout "%a\n" Pp.pp_rw (m,v,i,g)
            | _                 -> print "No GDT." )
  | Print str         -> output_string stdout str
  | Other (cmd,_)     -> prerr_string ("Unknown command '"^cmd^"'.\n")
