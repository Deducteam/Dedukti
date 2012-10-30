open Types

let rec iskind = function
  | Type          -> true
  | Pi (_,_,t)    -> iskind t
  | _             -> false

let emit str   = output_string !Global.out str                  (*print_string str*)
let emit_int i = output_string !Global.out (string_of_int i)    (*print_int i*)

(* Code Generation corresponding to a declaration *)

let rec get_vars a s = function
  | Lam (x,_,t)   -> get_vars (a+1) (s^","^x^"_c") t
  | te            -> (a,s,te)

let rec gen_code = function
  | Type                -> emit "{ ck = ctype }"
  | GVar v              -> emit  (!Global.name^"."^v^"_c")
  | EVar v              -> emit  (v^"_c")
  | Var v               -> emit  (v^"_c")
  | App (f,a)           -> ( emit "ap(" ; gen_code f ; emit ", " ; gen_code a ; emit ")" )
  | Lam (v,_,te)        -> 
      begin
        let (arity,vars,te2) = get_vars 1 (v^"_c") te in (* uncurryfication *)
          emit "{ ck = clam, arity = " ; 
          emit_int arity ; 
          emit ", args = { }, clam = function (" ; 
          emit vars ; 
          emit ") return ";
          gen_code te2 ;
          emit " end }"
      end
  | Pi (v0,ty,te)       ->
      begin
        emit "{ ck = cpi, cpi = { ";
        gen_code ty ;
        ( match v0 with
            | Some v        -> ( emit ", function (" ; emit (v^"_c") ; emit ") return " )
            | None          ->   emit ", function (dummy_c) return "  );
        gen_code te ;
        emit " end } }"
      end 

let rec gen_term = function
  | Type                -> emit "{ tk = ttype }"
  | GVar v              -> emit  (!Global.name^"."^v^"_t")
  | EVar v              -> emit  (v^"_t")
  | Var v               -> emit  (v^"_t")
  | App (f,a)           -> 
      begin 
        emit "{ tk = tapp, tapp = { " ; 
        gen_term f ; emit ", " ; 
        gen_term a ; emit ", " ; 
        gen_code a ; emit " } }"
      end
  | Lam (v,oty,te)      -> 
      begin
        emit "{ tk = tlam, tlam = { " ;
        ( match oty with
            | Some ty      -> gen_code ty (* emit "nil" *)
            | None          -> emit "nil"   );
        emit (", function ("^v^"_t, "^v^"_c) return ") ; 
        gen_term te;
        emit " end } }";
      end
  | Pi  (ov,ty,t)       -> 
      begin 
        emit "{ tk = tpi; tpi = { " ;
        gen_term ty ; emit ", " ; gen_code ty ;
        ( match ov with
            | Some v    -> emit (", function ("^v^"_t, "^v^"_c) return ")
            | None      -> emit ", function (dummy_t, dummy_c) return " );
        gen_term t;
        emit " end } }"
      end

(* Code Generation corresponding to set of rules *)

let rec gpcode = function
  | Id v                -> emit (v^"_c")
  | Pat (c,dots,pats)   ->
      begin
        let arity = Array.length dots + Array.length pats -1 in
          for i=0 to arity do emit "ap(" done ;
          emit (!Global.name^"."^c^"_c");
          Array.iter ( fun t -> emit ", " ; gen_code t ; emit ")" ) dots ;
          Array.iter ( fun p -> emit ", " ; gpcode   p ; emit ")" ) pats
      end

let rec gpterm = function 
  | Id v                -> emit (v^"_t")
  | Pat (c,dots,pats)   ->
      begin
        let arity = Array.length dots + Array.length pats -1 in
          for i=0 to arity do emit "{ tk = tapp, tapp = { " done ;
          emit (!Global.name^"."^c^"_t") ;
          Array.iter ( fun t -> emit ", " ; gen_term t ; emit ", " ; gen_code t ; emit " } }" ) dots ;
          Array.iter ( fun p -> emit ", " ; gpterm   p ; emit ", " ; gpcode   p ; emit " } }" ) pats
      end

let gen_env (id,te) =
  emit ("chkbeg(\""^id^"\")\n") ;
  (if iskind te then emit "chkkind(" else emit "chktype(");
  gen_term te ;
  emit ")\nlocal " ; 
  emit id ; emit "_c = { ck = ccon, ccon = \"" ; emit id ; emit "\", args = { } }\nlocal " ;
  emit id ; emit "_t = { tk = tbox, tbox = { " ; gen_code te ; emit ", " ; emit id ; emit "_c } }\n"  ;
  emit ("chkend(\""^id^"\")\n")

