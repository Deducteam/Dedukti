(** Matching on terms *)

open Basic
open Term

exception NotUnifiable
type ho_env = (int*term) LList.t

val resolve : int -> int LList.t -> term -> term

val ho_psubst : ho_env -> term -> term
