(** Errors handling *)
open Basic
open Term

val errors_in_snf : bool ref
(** Flag to enable SNF forms of terms in errors. *)

val color : bool ref
(** Flag to disable colored output. *)

module type ErrorHandler =
sig

val success : ('a, Format.formatter, unit) format -> 'a
(** Print a success message. *)

(*
val fail_exit : int -> string -> mident option -> loc option -> ('a, Format.formatter, unit) format -> 'a
(** [fail_exit c error_id md lc "..."]
    Prints the given error message prefixed with module and location details
    (when provided) as well as the error ID then exits with the given code. *)
*)

val fail_env_error : string option -> exn -> 'a
(** [fail_env_error file err]
    Prints a message explaining the given error raised in the (optionnal) given file
    then exits with code 3. *)

(*
val fail_sys_error : string -> 'a
(** Print a system error message then exits with code 1. *)
*)

end

module Make (E:Env.S) : ErrorHandler
