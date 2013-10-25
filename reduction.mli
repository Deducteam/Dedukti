
open Types

val hnf                 : term -> term          (* Head Normal Form *)

val wnf                 : term -> term          (* Weak Normal Form *)

val are_convertible     : term -> term -> bool  (* Conversion Test *)

val decompose_eq        : term -> term -> ((int*term) list) option 
