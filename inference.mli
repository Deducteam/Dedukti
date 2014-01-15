
open Types

(* *** Type Inference & Type Checking *** *)

(* ctx |- te:ty *)
val infer       : (*ctx:*)context -> (*te:*)pterm -> (*(te,ty):*)term*term 

(* ctx |- ty : Type or ctx |- ty : Kind *)
val check_type  : (*ctx*)context -> (*ty*)pterm -> (*ty*)term 

(* ctx |- te:ty *)
val check_term  : (*ctx*)context -> (*te*)pterm -> (*ty*)term -> (*te*)term  

(* Checks that a rule is well-typed *)
(* [ctx] id args --> r *)
(* args' is the version of args where placeholders have been filled *)
val check_rule  : prule -> loc * (*ctx*)context* (*id*)ident *(*(args,args')*)(pattern array*pattern array) * (*r*)term 
