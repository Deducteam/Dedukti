open Types

val resolve : loc -> ident -> term -> pattern list -> (term*term) list -> term*pattern list 
