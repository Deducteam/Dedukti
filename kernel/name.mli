type mident

type ident

val make_mident : string -> mident

val make : mident -> string -> ident

val make2 : string -> string -> ident

val id : ident -> string

val md : ident -> mident

val string_of_mident : mident -> string

val gensym : unit -> ident

val equal : ident -> ident -> bool

val mequal : mident -> mident -> bool

val pp_ident : Format.formatter -> ident -> unit

val pp_mident : Format.formatter -> mident -> unit

val pp_ident_confluence : Format.formatter -> ident -> unit
