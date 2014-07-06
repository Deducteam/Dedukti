
open Types

(* Head Normal Form *)
val hnf         : term -> term

(* Weak Head Normal Form *)
val whnf        : term -> term

(* Strong Normal Form *)
val snf         : term -> term

(* Conversion Test *)
val are_convertible             : term -> term -> bool
(*
(* Bounded Conversion Test *)
val bounded_are_convertible     : int -> term -> term -> yes_no_maybe

(* Weak Normal Form *)
val bounded_whnf                : int -> term -> term option
 *)
(*One Step Reduction*)
val one_step                    : term -> term option
