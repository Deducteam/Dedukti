
open Types

val string_of_term      : term -> string

val string_of_pterm     : preterm -> string

val string_of_pattern   : (int*term) list -> pattern -> string

val string_of_prepattern : prepattern -> string

val string_of_rule      : rule -> string

val string_of_cpair     : cpair -> string

val string_of_gdt       : ident -> ident -> int -> gdt -> string

