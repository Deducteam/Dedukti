(** Pretty printing. *)
open Types

type out = Format.formatter

val pp_pterm    : out -> preterm -> unit

val pp_ppattern : out -> prepattern -> unit

val pp_term     : out -> term -> unit

val pp_pattern  : out -> pattern -> unit

val pp_rule     : out -> rule -> unit

val pp_subst    : sep:string -> out -> term subst -> unit
val pp_subst_l  : sep:string -> out -> term Lazy.t subst -> unit

val pp_context  : out -> context -> unit

val pp_dtree    : int -> out -> dtree -> unit

val pp_rw       : out -> (ident*ident*int*dtree) -> unit

val pp_list     : string -> (out -> 'a -> unit) -> out -> 'a list -> unit
