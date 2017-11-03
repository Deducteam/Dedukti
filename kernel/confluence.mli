(** Confluence checker *)

open Basic
open Rule

type confluence_error =
  | NotConfluent of string
  | MaybeConfluent of string
  | CCFailure of string

val set_cmd : string -> unit
val initialize : unit -> unit

val check : unit -> bool
val add_constant : Name.ident -> unit
val add_rules : rule_infos list -> unit
val check : unit -> (unit,confluence_error) error

val finalize : unit -> unit
