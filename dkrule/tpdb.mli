(** Export of the rewrites rules into the TPDB format. *)
val export : out_channel -> (string*Rule.frule list) list -> unit
