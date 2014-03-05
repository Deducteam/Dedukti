
(** This files contains global options and functionalities *)

open Types

(** {2 Global Values} *)

val name                : ident ref
val run_on_stdin        : bool ref
val file                : string ref
val version             : string

(** {2 Program Options} *)

val debug_level         : int ref
val out                 : out_channel ref
val export              : bool ref
val ignore_redecl       : bool ref
val color               : bool ref
val autodep             : bool ref

(** {2 Printing Facilities} *)

(** Print in standard output *)
val print_std           : ('a, out_channel, unit) format -> 'a 
(** Print in channel set by the -o option  *)
val print_out           : ('a, out_channel, unit) format -> 'a
(** Print in stderr depending on debug_level *)
val debug               : int -> loc ->  ('a, out_channel, unit) format -> 'a
val debug_no_loc        : int -> ('a, out_channel, unit) format -> 'a
(** Print an warning message *)
val warning             : loc -> ('a, out_channel, unit) format -> 'a
(** Print an error message and exit *)
val fail                : loc -> ('a, out_channel, unit, 'b) format4 -> 'a
(** Print a success message *)
val success             : ('a, out_channel, unit) format -> 'a

(*FIXME dans le parser*)
val parse_cmd : loc -> string -> preterm list -> cmd
