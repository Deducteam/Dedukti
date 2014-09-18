(** Listing certain sets of the rewrite rules in the context. *)
open Rule

val print_all : out_channel -> (string*rule list) list -> unit
val print_non_linear_rules : out_channel -> (string*rule list) list -> unit
val print_type_level_rules : out_channel -> (string*rule list) list -> unit
