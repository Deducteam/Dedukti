open Basic
open Format

(** {2 PreTerms} *)
(** This module regroup types before the scoping. That is to say before variables are scoped with De Bruijn indices *)


type preterm =
  | PreType of loc
  | PreId   of loc * string
  | PreQId  of loc * string * string
  | PreApp  of preterm * preterm * preterm list
  | PreLam  of loc * string * preterm option * preterm
  | PrePi   of loc * string option * preterm * preterm

val pp_preterm : formatter -> preterm -> unit

type prepattern =
  | PCondition  of preterm
  | PPattern    of loc * string option * string * prepattern list
  | PLambda     of loc * string * prepattern
  | PJoker      of loc

val pp_prepattern : formatter -> prepattern -> unit

type pdecl      = loc * string

val pp_pdecl : formatter -> pdecl -> unit

type pcontext   = pdecl list

val pp_pcontext : formatter -> pcontext -> unit

type prule      = loc * (string option * string) option * pdecl list * string option * string * prepattern list * preterm

val pp_prule : formatter -> prule -> unit
