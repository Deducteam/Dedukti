open Graph
open Semiring
open Utils

type 'a final_cost = 'a IntMap.t

type path = int list
       
type 'a results =
    {
      final_cost : 'a final_cost ;
      path : path ;
    }

val empty_results : 'a results
	
module type Printable =
  sig
    type t
    val string : t -> string
  end
    
    
val string : (module Printable with type t = 'a) -> 'a results -> string

	
module GSSD:
functor (S:Semiring) ->
	functor	(G:Sig.G  with type V.t = int
			   and type V.label = int
			   and type E.t = int * S.t * int
			   and type E.label = S.t) ->
	  functor (Q:Queue)
	    ->
sig       
  val algorithm :
    ?trace:(bool) -> G.t -> int -> S.t results
end

module GCCFord:
functor (S:CostStructure) ->
	functor	(G:Sig.G  with type V.t = int
			   and type V.label = int
			   and type E.t = int * S.t * int
			   and type E.label = S.t)
		->
		sig
		  val algorithm :
		    ?trace:(bool) -> G.t -> int -> S.t results       	
		end

module Iteration:
functor (S:Semiring) ->
	functor	(G:Sig.G  with type V.t = int
			   and type V.label = int
			   and type E.t = int * S.t * int
			   and type E.label = S.t)
		->
		functor (M:Matrix.S with type elt = S.t)
			  ->
			  sig
			    val to_adjancy_matrix : G.t -> M.t
			    val left_iteration'' :  G.t -> M.t
			    val left_iteration' :  G.t -> M.t
			    val left_iteration : G.t -> M.t
			    val get_row : M.t -> int -> S.t results
			  end

