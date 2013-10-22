
open Types
          
val hnf                 : term -> term          (* Head Normal Form *)

val wnf                 : term -> term          (* Weak Normal Form *)

val term_eq             : term -> term -> bool  (* Syntactic equality *)

val are_convertible     : term -> term -> bool  (* Conversion Test *)
