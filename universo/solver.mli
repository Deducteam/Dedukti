open Term

type result
       
val elaboration : term -> term                            
val solve : unit -> result
val reconstruction : result -> term -> term
