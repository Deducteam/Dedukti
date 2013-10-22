
open Types

(* *** String sharing *** *)

val hstring     : string -> StringH.key 

(* *** Global Options *** *)

val name        : string ref
val out         : out_channel ref
val quiet       : bool ref
val export      : bool ref
val raphael     : bool ref

val set_name    : string -> unit 
val set_out     : string -> unit 

(* *** Info messages *** *)

val print       : string -> unit                (* Print a string on standard error *)
val print_v     : string -> unit                (* Print a string on standard error if in verbose mode *)
val error       : string -> string -> unit      (* Print an error message and exit *)


