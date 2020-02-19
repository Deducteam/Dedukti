open Basic
open Term

type ac_ident = name * algebra

val ac_ident_eq : ac_ident -> ac_ident -> bool

val pp_ac_ident : ac_ident printer

val force_flatten_AC_term :
  (term -> term) ->
  (term -> term -> bool) ->
  ac_ident -> term -> term list
(** [force_flatten_AC_term snf are_convertible aci t]
    returns the list [t1 ; ... ; tn] where
    [t] is convertible with  [t1 + ... + tn] and
    [aci] represents the AC(U) operator [+] while
    [whnf] is used to reduce to head normal form to check for [+] symbol
    at the head. All [ti] are reduced with [whnf].
    [are_convertible] checks convertibility to neutral element if needed.
 *)

val flatten_AC_terms : name -> term list -> term list

val flatten_AC_term  : name -> term -> term list

val unflatten_AC : ac_ident -> term list -> term
