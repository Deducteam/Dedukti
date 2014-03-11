
open Types

(* ctx |- te:ty *)
val infer      : (*ctx:*)context -> (*te:*)preterm -> (*(te,ty):*)term*term

(* ctx |- ty : Type or ctx |- ty : Kind *)
val check_type : (*ctx*)context -> (*ty*)preterm -> term(*ty*)

(* ctx |- te:ty *)
val check_term : (*ctx*)context -> (*te*)preterm -> (*ty*)term -> term(*te*)

val infer_ptop : context -> ptop -> ident * pattern array * term 
(*
val is_well_typed : context -> term -> bool
 *)
val check_context : pcontext -> context

val check_rule : prule -> rule
