open Basic

(** Raised on any kind of parse error. *)
exception Parse_error of loc * string

(** Raised on any kind of parse error. *)
exception Scoping_error of loc * string

