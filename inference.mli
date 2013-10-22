
open Types

val infer                       : term list -> term -> term

val infer_pattern_no_conv_check : term list -> pattern -> term 

