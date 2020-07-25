(** Pretty printing. *)
open Kernel
open Basic
open Term

val print_db_enabled    : bool ref
(** This function is here for debug purposes. It prints terms with De Bruijn indices. The terms output this way cannot be parsed again. *)

val print_default_name  : bool ref
(** Names for rules are optional. If this option is set to true, the printing functions will output default rule names given by Dedukti. *)

val print_module_name : bool ref

module type Sig =
sig
  val get_name : unit -> mident
  (** [get_name] get the current module defined for printing functions. *)
end

(** {2 Printing functions} *)
module type Printer =
sig
  val print_list  : string -> 'a printer -> 'a list printer
  (** [print_list sep printer] returns a printer for ['a list] using [printer] as
      element printer and [sep] as separator between elements. *)

  val print_ident         : ident               printer
  val print_mident        : mident              printer
  val print_name          : name                printer
  val print_term          : term                printer
  val print_typed_context : typed_context       printer
  val print_err_ctxt      : typed_context       printer
  val print_pattern       : Rule.pattern        printer
  val print_untyped_rule  : 'a Rule.rule        printer
  val print_typed_rule    : Rule.typed_rule     printer
  val print_rule_infos    : Dtree.rule_infos    printer
  val print_rule_name     : Rule.rule_name      printer
  val print_red_cfg       : Reduction.red_cfg   printer
  val print_entry         : Parsers.Entry.entry printer
  val print_staticity     : Signature.staticity printer
end

module Make(S:Sig) : Printer

module Default : Printer
