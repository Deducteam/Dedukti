(** Pretty printing. *)
open Basic
open Term
open Rule

val name                : unit -> mident
val print_db_enabled    : bool ref
val print_default       : bool ref
(** {2 Printing functions} *)

(** print_ident [fmt] [id] prints the identifier [id] with the formatter [fmt] *)
val print_ident : ident printer

val print_mident : mident printer

val print_name : name printer

val print_list  : string -> 'a printer -> 'a list printer

val print_term  : term printer

val print_pattern : pattern printer

val print_untyped_rule  : untyped_rule printer

val print_typed_rule  : typed_rule printer

val print_rule_infos : rule_infos printer

val print_typed_context: typed_context printer

val print_red_cfg :  Reduction.red_cfg printer

val print_entry   : Entry.entry printer
