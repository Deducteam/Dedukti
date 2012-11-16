open Types
open LuaGenerationBase
open PatternMatching

let generate_decl ((id,_),ty) = 
  let gname = !Global.name^"."^id in    (*FIXME*)
    if !Global.check then
      begin
        generate_decl_check gname ty ;
        generate_decl_code  gname ;
        generate_decl_term  gname ty
      end
    else
      generate_decl_code  gname 
        
let rec generate_rules (id,rs) = 
  let gname = !Global.name^"."^id in      (*FIXME*)
    if !Global.check then
      begin
        Array.iteri (generate_rule_check gname) rs ;
        generate_rules_code gname rs ;
      end
    else
      generate_rules_code gname rs




        
    if arity = 0 then 
      begin
        emit ("--[[ Type checking the definition of "^gname^". ]]\n") ;
        emit ("chkbeg(\"definition of "^gname^"\")\n") ;
        emit "chk(" ; gen_term ri ; emit (", "^gname^"_t.tbox[1])\n") ;
        emit ("chkend(\"definition of "^gname^"\")\n") ;
        emit (gname^"_c = ") ; gen_code ri ; emit "\n\n"
      end 
    else 
      begin
        (*Checking*)
        emit ("--[[ Type checking rules of "^gname^". ]]\n") ;
        emit ("function check_rules()\nchkbeg(\"rules of "^gname^"\")\n") ;
        Array.iteri (
          fun i (ctx,dots,pats,te) ->
            emit ("chkbeg(\"rule " ^ (string_of_int (i+1)) ^ "\")\n") ;
            List.iter gen_env ctx ; 
            emit "do\nlocal ty = synth(0, ";
            gpterm (Pat (id,dots,pats));
            emit ")\nchk(";
            gen_term te ;
            emit (", ty)\nend\nchkend(\"rule " ^ (string_of_int (i+1)) ^ "\")\n")
        ) rules ;
        emit ("chkend(\"rules of "^gname^"\")\nend\ncheck_rules()\n") ;

        (*Compiling*)
        emit ("--[[ Compiling rules of "^gname^". ]]\n") ;
        emit (gname ^ "_c = { ck = clam, arity = " ^ (string_of_int  arity) ^ ", args = { }, clam =\nfunction (y1") ;
        (for i=2 to arity do emit (", y" ^ (string_of_int i )) done );
        emit ")\n" ;
        cc id (new_pMat rules) ;
        emit "\nend }\n\n"
      end 




        ;
        emit ("--[[ Type checking "^gname^". ]]\n") ;
        emit ("chkbeg(\""^gname^"\")\n") ;
        (if iskind ty then emit "chkkind(" else emit "chktype(") ;
        gen_term ty ;
        emit (")\nchkend(\""^gname^"\")\n") ;
      end
    else () ;
  emit (gname^"_c = { ck = ccon, ccon = \""^gname^"\", args = { } }\n") ;
  emit (gname^"_t = { tk = tbox, tbox = { ") ;
  gen_code ty ;
  emit (", "^gname^"_c } }\n\n")

let rec gen_rules (id,rs) = 
  let rules = Array.of_list rs                          in (*assert (Array.length rs>0) ;*)
  let (_,dots,pats,ri) = rules.(0)                      in
  let arity = Array.length dots + Array.length pats     in
  let gname = !Global.name^"."^id                       in
        
    if arity = 0 then 
      begin
        emit ("--[[ Type checking the definition of "^gname^". ]]\n") ;
        emit ("chkbeg(\"definition of "^gname^"\")\n") ;
        emit "chk(" ; gen_term ri ; emit (", "^gname^"_t.tbox[1])\n") ;
        emit ("chkend(\"definition of "^gname^"\")\n") ;
        emit (gname^"_c = ") ; gen_code ri ; emit "\n\n"
      end 
    else 
      begin
        (*Checking*)
        emit ("--[[ Type checking rules of "^gname^". ]]\n") ;
        emit ("function check_rules()\nchkbeg(\"rules of "^gname^"\")\n") ;
        Array.iteri (
          fun i (ctx,dots,pats,te) ->
            emit ("chkbeg(\"rule " ^ (string_of_int (i+1)) ^ "\")\n") ;
            List.iter gen_env ctx ; 
            emit "do\nlocal ty = synth(0, ";
            gpterm (Pat (id,dots,pats));
            emit ")\nchk(";
            gen_term te ;
            emit (", ty)\nend\nchkend(\"rule " ^ (string_of_int (i+1)) ^ "\")\n")
        ) rules ;
        emit ("chkend(\"rules of "^gname^"\")\nend\ncheck_rules()\n") ;

        (*Compiling*)
        emit ("--[[ Compiling rules of "^gname^". ]]\n") ;
        emit (gname ^ "_c = { ck = clam, arity = " ^ (string_of_int  arity) ^ ", args = { }, clam =\nfunction (y1") ;
        (for i=2 to arity do emit (", y" ^ (string_of_int i )) done );
        emit ")\n" ;
        cc id (new_pMat rules) ;
        emit "\nend }\n\n"
      end 


