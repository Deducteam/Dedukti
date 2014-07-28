(** Listing certain sets of the rewrite rules in the context. *)
open Types

val print_all : rule list -> unit
val print_non_linear_rules : rule list -> unit
val print_pi_rules : rule list -> unit
val print_type_level_rules : rule list -> unit
