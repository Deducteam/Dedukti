
open Types

(* *** Type inference *** *)

val infer               : context -> term -> term

val infer_pattern       : context -> pattern -> term*(term*term) list 

(* *** Type checking *** *)

val check_type          : (*ctx*)context -> (*ty*)term -> unit (* ctx |- ty : Type or ctx |- ty : Kind *)

val check_term          : (*ctx*)context -> (*te*)term -> (*ty*)term -> unit  (* ctx |- te:ty *)

val check_rule          : rule -> unit  (* Checks that a rule is well-typed *)
