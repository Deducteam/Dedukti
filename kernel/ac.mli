open Basic
open Term

type ac_ident = name * algebra

val ac_ident_eq : ac_ident -> ac_ident -> bool

val is_acu : ac_ident -> bool

val pp_ac_ident : ac_ident printer

val force_flatten_AC_terms : (term -> term) ->
                             name -> term list -> term list

val force_flatten_AC_term  : (term -> term) ->
                             name -> term      -> term list

val flatten_AC_terms : name -> term list -> term list

val flatten_AC_term  : name -> term -> term list

val unflatten_AC : ac_ident -> term list -> term
