
open Types

val string_of_pterm     : pterm -> string 
  
val string_of_term      : term -> string 
 
val string_of_pattern   : pattern -> string 

val err_conv            : term -> term -> term -> string

val err_conv_type       : term -> term -> string

val err_conv2           : term -> term -> term -> term -> term -> string

val err_sort            : term -> term -> string

val err_topsort         : term -> string

val err_prod            : term -> term -> string

val err_prod2           : term -> string

val err_rule            : top -> string
