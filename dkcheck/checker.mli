(** Dedukti's main functionnalities. *)

val export         : bool ref
val verbose        : bool ref

val mk_entry       : Toplevel.entry -> unit
