
open Types

(* *** Type inference *** *)

val infer               : (*ctx:*)context -> (*te:*)preterm -> (*(te,ty):*)term*term    (* ctx |- te:ty *)

(* *** Type checking *** *)

val check_type          : (*ctx*)context -> (*ty*)preterm -> term(*ty*)                 (* ctx |- ty : Type or ctx |- ty : Kind *)

val check_term          : (*ctx*)context -> (*te*)preterm -> (*ty*)term -> term(*te*)   (* ctx |- te:ty *)

val check_rule          : prule -> rule                                                 
