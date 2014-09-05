open Types
(** Export of the rewrites rules into the TPDB format. *)
val export : out_channel -> (string*rule list) list -> unit
