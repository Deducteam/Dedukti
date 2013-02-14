open Types
open Printf

let rec iskind = function
  | Type          -> true
  | Pi (_,_,t)    -> iskind t
  | _             -> false

(* Type Checking *)

let typecheck_decl gname loc ty = 
  (*fprintf !Global.out " -- [[ Type checking %s. ]]\n" gname ;*)
  fprintf !Global.out "\nprint_debug(\"%s\tChecking declaration %s\t\t\")\n" (Debug.string_of_loc loc) gname ;
  (if iskind ty then fprintf !Global.out "chkkind(" else fprintf !Global.out "chktype(") ;
  CodeGeneration.gen_term ty ;
  fprintf !Global.out ")\n"

let typecheck_def gname loc te ty =
  (*fprintf !Global.out " -- [[ Type checking %s. ]]\n" gname ;*)
  fprintf !Global.out "\nprint_debug(\"%s\tChecking definition %s\t\t\")\n" (Debug.string_of_loc loc) gname ;
  fprintf !Global.out "chk( " ;
  CodeGeneration.gen_term te ;
  fprintf !Global.out " , " ;
  CodeGeneration.gen_code0 ty ;
  fprintf !Global.out ")\n"

let gen_env ((id,loc),te) =
  fprintf !Global.out "\nprint_debug(\"%s\tChecking variable %s\t\t\")\n" (Debug.string_of_loc loc) id ;
  (if iskind te then fprintf !Global.out "chkkind(" else fprintf !Global.out "chktype(");
  CodeGeneration.gen_term te ;
  fprintf !Global.out ")\nlocal %s_c = { co = ccon ; id = \"%s\" ; arity = 0 ; args = { } ; f = function() return nil end}\n" id id ; 
  (*fprintf !Global.out " print(\"%s_c = \" .. string_of_code(0,%s_c))\n" id id ;*)
  fprintf !Global.out "local %s_t = { te = tbox, ctype = function() return " id ;
  CodeGeneration.gen_code0 te ;
  fprintf !Global.out " end }\n" 
    (*;fprintf !Global.out " print(\"%s_t = \" .. string_of_term(0,%s_t))\n" id id *)

let typecheck_rule id i (loc,ctx,dots,pats,te) =
  fprintf !Global.out "\ndo";
  List.iter gen_env ctx ; 
  fprintf !Global.out "\nprint_debug(\"%s\tChecking rule %i for %s\t\t\")\n" (Debug.string_of_loc loc) (i+1) id ;
  fprintf !Global.out "local ty = type_synth(0, ";
  CodeGeneration.gpterm (Pat (Global.get_gvar_name id,dots,pats));
  fprintf !Global.out ")\n";
  (*fprintf !Global.out " print(\"ty = \" .. string_of_code(0,ty))\n" ;*)
  fprintf !Global.out "chk(";
  CodeGeneration.gen_term te ;
  fprintf !Global.out ", ty)\nend\n"

(* Code Generation *)

let generate_decl gname ty =
  fprintf !Global.out "%s_c = { co = ccon ; id = \"%s\" ; arity = 0 ; args = { } ; f = function() return nil end }\n" gname gname ;
    (* fprintf !Global.out " print(\"%s_c = \" .. string_of_code(0,%s_c))\n" gname gname ; *)
  fprintf !Global.out "%s_t = { te = tbox ; ctype = function() return " gname ;
  CodeGeneration.gen_code0 ty ;
  fprintf !Global.out " end }\n" 
(*; fprintf !Global.out " print(\"%s_t = \" .. string_of_term(0,%s_t))\n" gname gname *)

let generate_def gname te =
  fprintf !Global.out "%s_c = " gname ;
  CodeGeneration.gen_code0 te ;
  fprintf !Global.out "\n" ;
  (*fprintf !Global.out " print(\"%s_c = \" .. string_of_code(0,%s_c))\n" gname gname ; *)
  fprintf !Global.out "%s_t = " gname ;
  CodeGeneration.gen_term te ;
  fprintf !Global.out "\n"
    (*;fprintf !Global.out " print(\"%s_t = \" .. string_of_term(0,%s_t))\n" gname gname *)


