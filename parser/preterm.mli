open Basic

(** {2 PreTerms} *)

type preterm =
  | PreType of loc
  | PreId   of loc * ident
  | PreQId  of loc * ident * ident
  | PreApp  of preterm * preterm * preterm list
  | PreLam  of loc * ident * preterm option * preterm
  | PrePi   of loc * ident option * preterm * preterm

type prepattern =
  | PCondition  of preterm
  | PPattern    of loc*ident option*ident*prepattern list
  | PLambda     of loc*ident*prepattern
  | PJoker      of loc

type param =
  | PDecl of loc * ident * preterm
  | PDef of loc * ident * preterm
type pfield = loc * ident * preterm

type pdecl      = loc * ident
type pcontext   = pdecl list
type prule      = loc * pdecl list * ident option * ident * prepattern list * preterm

val pp_pterm    : out_channel -> preterm -> unit
val pp_ppattern : out_channel -> prepattern -> unit
