open Types
(** Export of the rewrites rules into the TPDB format. *)
val export : Format.formatter -> (string*rule list) list -> unit
