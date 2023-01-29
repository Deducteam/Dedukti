(** Errors handling *)

open Kernel
open Basic

(** Flag to disable colored output. *)
val color : bool ref

val reduce : (Term.term -> Term.term) ref

(** Print a success message. *)
val success : string -> unit

(** [fail_exit file error_id lc "..."]
    Prints the given error message prefixed with module and location details
    (when provided) as well as the error ID then exits with the given code. *)
val fail_exit :
  file:string ->
  code:string ->
  loc option ->
  ('a, Format.formatter, unit, 'b) format4 ->
  'a

(** Print a system error message then exits with code 1. *)
val fail_sys_error : [< `File of string | `Stdin | `String] -> msg:string -> 'a

val fail_exn : [< `File of string | `Stdin | `String] -> Basic.loc -> exn -> 'a

val fail_cli : msg:string -> 'a

type error_code = int

type error_msg = error_code * Basic.loc option * string

type error_handler = reduce:(Term.term -> Term.term) -> exn -> error_msg option

val register_exception :
  (reduce:(Term.term -> Term.term) -> exn -> error_msg option) -> unit

val string_of_exception :
  reduce:(Term.term -> Term.term) ->
  Basic.loc ->
  exn ->
  error_code * Basic.loc * string
