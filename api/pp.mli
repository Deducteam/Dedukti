(** Pretty printing. *)
open Basic
open Term

val print_db_enabled    : bool ref
(** This function is here for debug purposes. It prints terms with De Bruijn indices. The terms output this way cannot be parsed again. *)

val print_default_name  : bool ref
(** Names for rules are optional. If this option is set to true, the printing functions will output default rule names given by Dedukti. *)


(** {1 General use printing functions} *)

val print_list  : string -> 'a printer -> 'a list printer
(** [print_list sep printer] returns a printer for ['a list] using [printer] as
    element printer and [sep] as separator between elements. *)

val print_ident  : ident  printer
val print_mident : mident printer
val print_name   : name   printer
val print_staticity : Signature.staticity printer
val print_red_cfg   : Reduction.red_cfg   printer

(** {2 Printing functions} *)

module type Sig =
sig
  type t
  val get_name : t -> mident
end

module type Printer =
sig
  type t
  val print_term          : t -> term                printer
  val print_typed_context : t -> typed_context       printer
  val print_err_ctxt      : t -> typed_context       printer
  val print_pattern       : t -> Rule.pattern        printer
  val print_untyped_rule  : t -> Rule.untyped_rule   printer
  val print_typed_rule    : t -> Rule.typed_rule     printer
  val print_rule_infos    : t -> Rule.rule_infos     printer
  val print_rule_name     : t -> Rule.rule_name      printer
  val print_entry         : t -> Entry.entry         printer
end

module Make (S:Sig) : Printer with type t = S.t
module Default      : Printer with type t = unit
module WithModname  : Printer with type t = mident
