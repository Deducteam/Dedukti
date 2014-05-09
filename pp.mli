
open Types

val pp_pterm    : out_channel -> preterm -> unit

val pp_ppattern : out_channel -> prepattern -> unit

val pp_term     : out_channel -> term -> unit

val pp_pattern  : out_channel -> pattern -> unit

val pp_rule     : out_channel -> rule -> unit

val pp_context  : out_channel -> context -> unit

val pp_dtree    : int -> out_channel -> dtree -> unit

val pp_rw      : out_channel -> (ident*ident*int*dtree) -> unit

val pp_list     : string -> (out_channel -> 'a -> unit) -> out_channel -> 'a list -> unit
