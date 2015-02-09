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
      Printf.fprintf stdout "%a\n" Pp.pp_term (Env.whnf te)
  | Hnf te           ->
      Printf.fprintf stdout "%a\n" Pp.pp_term (Env.hnf te)
  | Snf te           ->
      Printf.fprintf stdout "%a\n" Pp.pp_term (Env.snf te)
  | OneStep te       ->
      begin
        match Env.one te with
          | None -> Printf.fprintf stdout "%a\n" Pp.pp_term te
          | Some te' -> Printf.fprintf stdout "%a\n" Pp.pp_term te'
      end
  | Conv (te1,te2)  ->
        if Env.are_convertible te1 te2 then print "YES"
        else print "NO"
  | Check (te,ty) ->
        if Env.check te ty then print "YES"
        else print "NO"
  | Infer te         ->
      Printf.fprintf stdout "%a\n" Pp.pp_term (Env.infer te)
  | Gdt (m0,v)         ->
      let m = match m0 with None -> Env.get_name () | Some m -> m in
        ( match Env.get_dtree lc m v with
            | Signature.DoD_Dtree (i,g) ->
                Printf.fprintf stdout "%a\n" Pp.pp_rw (m,v,i,g)
            | _ -> print "No GDT." )
  | Print str         -> output_string stdout str
  | Other (cmd,_)     -> prerr_string ("Unknown command '"^cmd^"'.\n")
