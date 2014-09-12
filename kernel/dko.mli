open Types

val marshal : ident -> string list -> rw_infos H.t -> unit

val unmarshal : loc -> string -> (string list*rw_infos H.t)

val get_all_rules : string -> (string*rule list) list
