open Kernel.Basic
open Format

include Preterm

type param =
  | PDecl of loc * ident * preterm
  | PDef of loc * ident * preterm
type pfield = loc * ident * preterm
