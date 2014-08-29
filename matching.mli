open Types

type pb = term*int list

val resolve_lst : term list -> pb LList.t -> term LList.t option
