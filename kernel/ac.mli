open Basic
open Term

type ac_ident = ident * ident * algebra

val ac_ident_eq : ac_ident -> ac_ident -> bool

val is_acu : ac_ident -> bool

val pp_ac_ident : Format.formatter -> ac_ident -> unit

val force_flatten_AC_terms : (term -> term) ->
                             ident -> ident -> term list -> term list

val force_flatten_AC_term  : (term -> term) ->
                             ident -> ident -> term      -> term list

val flatten_AC_terms : ident -> ident -> term list -> term list

val flatten_AC_term  : ident -> ident -> term      -> term list

val unflatten_AC : ac_ident -> term list -> term
