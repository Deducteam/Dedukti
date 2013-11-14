
open Types

(* *** Type inference *** *)

val infer               : (*ctx:*)context -> (*te:*)pterm -> (*(te,ty):*)term*term (* ctx |- te:ty *)

(* *** Type checking *** *)

val check_type          : (*ctx*)context -> (*ty*)pterm -> term(*ty*) (* ctx |- ty : Type or ctx |- ty : Kind *)

val check_term          : (*ctx*)context -> (*te*)pterm -> (*ty*)term -> term(*te*)  (* ctx |- te:ty *)

val check_rule          : prule -> rule  (* Checks that a rule is well-typed *)
