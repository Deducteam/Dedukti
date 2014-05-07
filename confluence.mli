open Types

val linearity                   : bool ref
val weak_confluence             : wcr ref
val subject_reduction_criterion : bool ref
val toyama_criterion            : bool ref

val init                        : string -> unit
val export                      : out_channel -> unit
