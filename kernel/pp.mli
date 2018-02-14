(** Pretty printing. *)
open Basic
open Term
open Rule

val name                : unit -> mident
val print_db_enabled    : bool ref
val print_default       : bool ref
(** {2 Printing functions} *)

(** print_ident [fmt] [id] prints the identifier [id] with the formatter [fmt] *)
val print_ident : Format.formatter -> ident -> unit

val print_mident : Format.formatter -> mident -> unit

val print_name : Format.formatter -> name -> unit

val print_list  : string -> (Format.formatter -> 'a -> unit)
                  -> Format.formatter -> 'a list -> unit

val print_term  : Format.formatter -> term -> unit

val print_pattern : Format.formatter -> pattern -> unit

val print_untyped_rule  : Format.formatter -> untyped_rule -> unit

val print_typed_rule  : Format.formatter -> typed_rule -> unit

val print_rule_infos : Format.formatter -> rule_infos -> unit

val print_typed_context: Format.formatter -> typed_context -> unit

val print_eval_config :  Format.formatter -> Env.eval_config -> unit
