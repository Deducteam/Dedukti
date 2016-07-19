open Graph
open Semiring

module type Queue = sig
    type 'a t
    exception Empty
    val empty : 'a t
    val enqueue : 'a -> 'a t -> 'a t
    val dequeue : 'a t -> ('a * 'a t)
    val mem : 'a -> 'a t -> bool
    val iter : ('a -> unit) -> 'a t -> unit
  end

		      
module PrintGraph :
functor (S:Semiring) ->
	functor(G:Sig.G  with type V.t = int
			   and type V.label = int
			   and type E.t = int * S.t * int
			   and type E.label = S.t) ->
	       sig
		 val print_graph :
		   ?format:(string) ->	G.t -> unit
	       end		      
		 

module Stack:Queue		 

module RandomQueue:Queue
	       
module IntMap:(Map.S with type key = int)
module IntIntSet:(Set.S with type elt = int * int)
		
module DefaultDotAttr : functor (S:Semiring) ->
			functor(G:Sig.G with type V.t = int
					 and type V.label = int
					 and type E.t = int * S.t * int
					 and type E.label = S.t)
			-> (Graphviz.GraphWithDotAttrs with type V.t = int
							and type V.label = int
							and type E.t = int * S.t * int
							and type E.label = S.t)
