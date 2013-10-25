
open Types

val infer               : loc -> term list -> term -> term

val infer_pattern       : term list -> pattern -> term*(term*term) list 
