(** Pretty printing. *)
open Basics
open Preterm
open Term
open Rule

val name                : ident ref
val print_db_enabled    : bool ref

val print_list  : string -> (Format.formatter -> 'a -> unit)
                  -> Format.formatter -> 'a list -> unit

val pp_pterm    : out_channel -> preterm -> unit

val print_pterm : Format.formatter -> preterm -> unit

val pp_ppattern : out_channel -> prepattern -> unit

val print_ppattern : Format.formatter -> prepattern -> unit

val pp_term     : out_channel -> term -> unit

val print_term  : Format.formatter -> term -> unit

val pp_pattern  : out_channel -> pattern -> unit

val print_pattern : Format.formatter -> pattern -> unit

val pp_rule     : out_channel -> rule -> unit

val print_rule  : Format.formatter -> rule -> unit

val pp_frule    : out_channel -> rule_infos -> unit

val print_frule : Format.formatter -> rule_infos -> unit

val pp_context  : out_channel -> context -> unit

val print_context: Format.formatter -> context -> unit

val pp_dtree    : int -> out_channel -> dtree -> unit

val pp_rw       : out_channel -> (ident*ident*int*dtree) -> unit

val pp_list     : string -> (out_channel -> 'a -> unit) -> out_channel -> 'a list -> unit
