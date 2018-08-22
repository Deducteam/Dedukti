(** Pretty printing. *)
open Basic
open Term

val set_module          : mident -> unit
val get_module          : unit -> mident
val print_db_enabled    : bool ref
val print_default       : bool ref

(** {2 Printing functions} *)

val print_ident         : ident              printer
val print_mident        : mident             printer
val print_name          : name               printer
val print_term          : term               printer
val print_typed_context : typed_context      printer
val print_pattern       : Rule.pattern       printer
val print_untyped_rule  : Rule.untyped_rule  printer
val print_typed_rule    : Rule.typed_rule    printer
val print_rule_infos    : Rule.rule_infos    printer
val print_rule_name     : Rule.rule_name     printer
val print_red_cfg       : Reduction.red_cfg  printer
val print_entry         : Entry.entry        printer

(** [print_list sep printer] returns a printer for ['a list] using [printer] as
    element printer and [sep] as separator between elements. *)
val print_list  : string -> 'a printer -> 'a list printer
