open Types

val linearity                   : bool ref
val weak_confluence             : wcr ref
val type_level_applicative      : bool ref
val modularity                  : bool ref

val init                        : string -> unit
val export                      : in_channel -> unit

