
open Types

val hnf                 : term -> term          (* Head Normal Form *)

val wnf                 : term -> term          (* Weak Normal Form *)

val snf                 : term -> term          (* Strong Normal Form *)

val are_convertible     : term -> term -> bool  (* Conversion Test *)

val are_convertible_with_meta : term -> term -> yes_no_maybe  (* Conversion Test *)

val wnf_with_meta       : term -> term option   (* Weak Normal Form *)

val decompose_eq        : (int*term) list -> (term*term) list -> (int*term) list
