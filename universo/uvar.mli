type uvar

exception Not_uvar

val is_uvar : Term.term -> bool

val name_of_uvar : Term.term -> Basic.name

val fresh_uvar : Signature.t -> Term.term
