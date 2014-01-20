open Types

val resolve : partial_term -> pattern list -> (partial_term*partial_term) list 
                -> term*pattern list 
