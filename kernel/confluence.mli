(** Confluence checker *)

open Basic

val d_confluence : Debug.flag

type confluence_error =
  | NotConfluent   of string
  | MaybeConfluent of string
  | CCFailure      of string

exception Confluence_error of confluence_error

val set_cmd : string -> unit
val initialize : unit -> unit

val add_constant : name -> unit
val add_rules : Dtree.rule_infos list -> unit

(** Runs confluence checker. May raise Confluence Error. *)
val check : unit -> unit

val finalize : unit -> unit
