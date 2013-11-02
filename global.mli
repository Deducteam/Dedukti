
open Types

(* *** Global Options *** *)

val name        : ident ref
val out         : out_channel ref
val quiet       : bool ref
val export      : bool ref
val raphael     : bool ref
val color       : bool ref

val set_name    : string -> unit 
val set_out     : string -> unit 

(* *** Info messages *** *)

val sprint      : string -> unit        (* Print a string on standard output *)
val eprint      : string -> unit        (* Print a string on standard error *)
val vprint      : string Lazy.t -> unit (* Print a string on standard error if in verbose mode *)
val error       : loc -> string -> unit (* Print an error message and exit *)
val print_ok    : unit -> unit          (* Print a success message *)



