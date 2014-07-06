
open Types

val infer : (*te*)preterm -> term*term (* (te,ty) *)

val check : (*te*)preterm -> (*ty*)preterm -> term*term(* (te,ty) *)

val is_a_type : (*te*)preterm -> term (*te*)

val check_rule : prule -> rule
