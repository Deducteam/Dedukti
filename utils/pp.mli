(** Pretty printing. *)
open Basics
open Preterm
open Term
open Rule

val name                : ident ref
val print_db_enabled    : bool ref
val resugar             : bool ref

val print_ident : Format.formatter -> ident -> unit

val print_list  : string -> (Format.formatter -> 'a -> unit)
                  -> Format.formatter -> 'a list -> unit

val print_pterm : Format.formatter -> preterm -> unit

val print_ppattern : Format.formatter -> prepattern -> unit

val print_term  : Format.formatter -> term -> unit

val print_pattern : Format.formatter -> pattern -> unit

val print_rule  : Format.formatter -> rule -> unit

val print_rule2  : Format.formatter -> rule2 -> unit

val print_frule : Format.formatter -> rule_infos -> unit

val print_context: Format.formatter -> context -> unit
