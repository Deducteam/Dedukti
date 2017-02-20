(** Matching on terms *)

open Basic
open Term

exception NotUnifiable

val solve : int -> int LList.t -> term -> term
