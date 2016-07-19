open Graph
open Semiring
open Utils


       
module type SPCS =
  sig
    type t
    type elt
    type graph
    val generalized_ford : ?trace:bool -> graph -> int -> t
    val string : t -> string
  end

module type SPSR =
  sig    
    type t
    type elt
    type graph
    type stepper

    val mohri_stepper : stepper
	   
    val stepper_algorithm : stepper -> graph -> int -> t
	   
    (*val generic_single_source : ?trace:bool -> graph -> int -> t*)
    val left_iteration'' : graph -> int -> t
    val left_iteration' : graph -> int -> t
    val left_iteration : graph -> int -> t
    val result : t -> int -> elt
    val string : t -> string
  end
    
module MakeCS: functor (CS:CostStructure) ->
		       functor (G:Sig.G
				with type V.t = int
				 and type V.label = int
				 and type E.t = int * CS.t * int
				 and type E.label = CS.t) ->
		       (SPCS
			with type elt = CS.t
			 and type graph = G.t
		       ) 
					       

module MakeSR: functor (S:Semiring) ->
		       functor (G:Sig.G
				with type V.t = int
				 and type V.label = int
				 and type E.t = int * S.t * int
				 and type E.label = S.t)  ->
		       functor (Q:Queue) ->
		       (SPSR
			with type elt = S.t
			 and type graph = G.t
		       ) 
					       

    
