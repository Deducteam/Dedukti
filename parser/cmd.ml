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
      let _ = Inference.infer [] te in
        Printf.fprintf stdout "%a\n" Pp.pp_term (Reduction.whnf te)
  | Hnf te           ->
      let _ = Inference.infer [] te in
        Printf.fprintf stdout "%a\n" Pp.pp_term (Reduction.hnf te)
  | Snf te           ->
      let _ = Inference.infer [] te in
        Printf.fprintf stdout "%a\n" Pp.pp_term (Reduction.snf te)
  | OneStep te       ->
      let _ = Inference.infer [] te in
        ( match Reduction.one_step te with
            | None    -> print "Already in weak head normal form."
            | Some t' -> Printf.fprintf stdout "%a\n" Pp.pp_term t' )
  | Conv (te1,te2)  ->
      let _ = Inference.infer [] te1 in
      let _ = Inference.infer [] te2 in
        if Reduction.are_convertible te1 te2 then print "YES"
        else print "NO"
  | Check (te,ty1) ->
      let _ = Inference.infer [] ty1 in
      let ty2 = Inference.infer [] te in
        if Reduction.are_convertible ty1 ty2 then print "YES"
        else print "NO"
  | Infer te         ->
      let ty = Inference.infer [] te in
        Printf.fprintf stdout "%a\n" Pp.pp_term ty
  | Gdt (m0,v)         ->
      let m = match m0 with None -> Env.get_name () | Some m -> m in
        ( match Env.get_dtree lc m v with
            | Env.DoD_Dtree (i,g) ->
                Printf.fprintf stdout "%a\n" Pp.pp_rw (m,v,i,g)
            | _                 -> print "No GDT." )
  | Print str         -> output_string stdout str
  | Other (cmd,_)     -> prerr_string ("Unknown command '"^cmd^"'.\n")
