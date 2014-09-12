(** Pinting facilities. *)

(** Colored output. *)
val color               : bool ref

(** Print in stderr *)
val debug               : ('a, out_channel, unit) format -> 'a

(** Print an error message and exit. *)
val fail                : Term.loc -> ('a, out_channel, unit, 'b) format4 -> 'a

(** Print a success message. *)
val success             : ('a, out_channel, unit) format -> 'a
