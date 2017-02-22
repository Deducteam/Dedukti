(** Pretty printing. *)
open Basic
open Preterm
open Term
open Rule

val name                : ident ref
val print_db_enabled    : bool ref

(** {2 Printing functions for debugging} *)

(** pp_ident [chan] [id] prints the identifier [id] on the channel [chan] *)
val pp_ident : out_channel -> ident -> unit

(** pp_list [sep] [pp] [l] print a list [\[l1 ; ... ln\]] by applying [pp] on each element and use se separator [sep] between elements *)
val pp_list : string -> (out_channel -> 'a -> unit) -> out_channel -> 'a list -> unit

val pp_pterm    : out_channel -> preterm -> unit

val pp_ppattern : out_channel -> prepattern -> unit

val pp_term     : out_channel -> term -> unit

val pp_pattern  : out_channel -> pattern -> unit

val pp_typed_context  : out_channel -> typed_context -> unit

val pp_untyped_rule  : out_channel -> untyped_rule -> unit

val pp_typed_rule    : out_channel -> typed_rule -> unit

val pp_rule_infos    : out_channel -> rule_infos -> unit

val pp_dtree    : int -> out_channel -> dtree -> unit

val pp_rw       : out_channel -> (ident*ident*int*dtree) -> unit

(** {2 Printing functions} *)

(** print_ident [fmt] [id] prints the identifier [id] with the formatter [fmt] *)
val print_ident : Format.formatter -> ident -> unit

val print_list  : string -> (Format.formatter -> 'a -> unit)
                  -> Format.formatter -> 'a list -> unit

val print_pterm : Format.formatter -> preterm -> unit

val print_ppattern : Format.formatter -> prepattern -> unit

val print_term  : Format.formatter -> term -> unit

val print_pattern : Format.formatter -> pattern -> unit

val print_untyped_rule  : Format.formatter -> untyped_rule -> unit

val print_typed_rule  : Format.formatter -> typed_rule -> unit

val print_rule_infos : Format.formatter -> rule_infos -> unit

val print_typed_context: Format.formatter -> typed_context -> unit
