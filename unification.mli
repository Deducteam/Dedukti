open Types

exception CannotFindAType
exception CannotType 

val resolve_constraints : term -> (term*term) list -> term 


