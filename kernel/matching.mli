(** Matching on terms *)

open Basics
open Term

exception NotUnifiable

val solve : int -> int LList.t -> term -> term
