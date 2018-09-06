(** Confluence checker *)

open Basic
open Rule
    
type Debug.flag += D_confluence

type confluence_error =
  | NotConfluent   of string
  | MaybeConfluent of string
  | CCFailure      of string

exception ConfluenceError of confluence_error

val set_cmd : string -> unit
val initialize : unit -> unit

val add_constant : name -> unit
val add_rules : rule_infos list -> unit

(** Runs confluence checker. May raise Confluence Error. *)
val check : unit -> unit

val finalize : unit -> unit
