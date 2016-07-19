open Semiring

(* TODO : implement a fold over matrices *)
       
module type Vector =
  sig
    type 'a t

    val create : int -> 'a -> 'a t
    val get : int -> 'a t -> 'a
    val set : int -> 'a -> 'a t -> 'a t
    val size : 'a t -> int
    val fold_left : ('a -> 'b -> 'b) -> 'a t -> 'b -> 'b
    val equal : 'a t -> 'a t -> bool
  end

       
module type S =
  sig
    type elt
    type t
	   
    val create : int -> int -> elt -> t
					
    val get : int -> int -> t -> elt
				   
    val set : int -> int -> elt -> t -> t

    val size : t -> int * int
					  
    val identity : int -> t
						  
    val multiply : t -> t -> t

    val pointwise_multiply : t -> t -> t
			       
    val dot_multiply : t -> t -> t -> t
					
    val add : t -> t -> t

    val dot_add : t -> t -> t -> t
			  
    val opposite : t -> t

    val xor : t -> t -> t

    val orb : t -> t -> t
			  
    val dot_add_left : t -> t -> t

    val dot_add_right : t -> t -> t
			  
    val of_list : elt list list -> t

    (* dirty *)				     
    val equal : t -> t -> bool
				     
    val string : t -> string
  end
    
module Make: functor (S:Semiring) ->
	     S with type elt = S.t
	    

