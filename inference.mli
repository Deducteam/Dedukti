
open Types

val infer               : term list -> term -> term

val infer_pattern       : term list -> pattern -> term*(term*term) list 

