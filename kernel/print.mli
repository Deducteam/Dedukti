(** Pinting facilities. *)

open Types

val color               : bool ref (** Colored output. *)

(** Print in stderr *)
val debug               : ('a, out_channel, unit) format -> 'a

(** Print an error message and exit. *)
val fail                : loc -> ('a, out_channel, unit, 'b) format4 -> 'a

(** Print a success message. *)
val success             : ('a, out_channel, unit) format -> 'a
