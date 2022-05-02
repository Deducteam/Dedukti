open Kernel.Basic
open Format

exception AppliedGuardedTerm of loc

exception BetaRedexInLHS of loc

(** {2 PreTerms} *)

(** This module defines structures representing terms before their scoping.
    That is to say before variables are scoped with De Bruijn indices *)

type preterm =
  | PreType of loc
  | PreId of loc * ident
  | PreQId of loc * name
  | PreApp of preterm * preterm * preterm list
  | PreLam of loc * ident * preterm option * preterm
  | PrePi of loc * ident option * preterm * preterm

val pp_preterm : formatter -> preterm -> unit

type prepattern =
  | PCondition of preterm
  | PPattern of loc * mident option * ident * prepattern list
  | PLambda of loc * ident * prepattern
  | PJoker of loc * prepattern list
  | PApp of prepattern list

val pp_prepattern : formatter -> prepattern -> unit

val clean_pre_pattern : prepattern -> prepattern

type pdecl = (loc * ident) * preterm option

val pp_pdecl : formatter -> pdecl -> unit

type pcontext = pdecl list

val pp_pcontext : formatter -> pcontext -> unit

type prule =
  loc
  * (mident option * ident) option
  * pdecl list
  * mident option
  * ident
  * prepattern list
  * preterm

val pp_prule : formatter -> prule -> unit
