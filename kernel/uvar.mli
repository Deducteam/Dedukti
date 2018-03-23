type uvar

exception Not_uvar

val is_uvar : Term.term -> bool

val ident_of_uvar : Term.term -> Basic.ident

val fresh_uvar : Signature.t -> Term.term
