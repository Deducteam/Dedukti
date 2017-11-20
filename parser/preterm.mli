open Basic
open Format

(** {2 PreTerms} *)
(** This module regroup types before the scoping. That is to say before variables are scoped with De Bruijn indices *)


type preterm =
  | PreType of loc
  | PreId   of loc * ident
  | PreQId  of loc * name
  | PreApp  of preterm * preterm * preterm list
  | PreLam  of loc * ident * preterm option * preterm
  | PrePi   of loc * ident option * preterm * preterm
  | PreMeta of loc * ident option

val pp_preterm : formatter -> preterm -> unit

type prepattern =
  | PCondition  of preterm
  | PPattern    of loc * mident option * ident * prepattern list
  | PLambda     of loc * ident * prepattern
  | PJoker      of loc

val pp_prepattern : formatter -> prepattern -> unit

type pdecl      = loc * ident

val pp_pdecl : formatter -> pdecl -> unit

type pcontext   = pdecl list

val pp_pcontext : formatter -> pcontext -> unit

type prule      = loc * (mident option *ident) option * pdecl list * mident option * ident * prepattern list * preterm

val pp_prule : formatter -> prule -> unit

type pbox_term =
  | PMT of loc * pcontext * preterm

type pmtype =
  | PImpl of loc * pmtype * pmtype
  | PForall of loc * ident * pbox_term * pmtype
  | PBoxTy of pbox_term

type pmterm =
  | PMLamF of loc * ident * pbox_term * pmterm
  | PMLamI of loc * ident * pmtype * pmterm
  | PBoxTe of pbox_term
  | PMDB of loc * ident
  | PMApp of pmterm * pmterm
