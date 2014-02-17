
open Types

(* ctx |- te:ty *)
val infer      : (*ctx:*)context -> (*te:*)preterm -> (*(te,ty):*)term*term

(* ctx |- ty : Type or ctx |- ty : Kind *)
val check_type : (*ctx*)context -> (*ty*)preterm -> term(*ty*)

(* ctx |- te:ty *)
val check_term : (*ctx*)context -> (*te*)preterm -> (*ty*)term -> term(*te*)

val infer_ptop : context -> ptop -> ( int * pattern * term * (term*term) list )

val is_well_typed : context -> term -> bool

val infer_pattern2 : pattern -> (term*term) list
