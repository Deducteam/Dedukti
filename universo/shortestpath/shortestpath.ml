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

    
    
module MakeCS(CS:CostStructure)(G:Sig.G
				with type V.t = int
				 and type V.label = int
				 and type E.t = int * CS.t * int
				 and type E.label = CS.t) =
  struct


    type t = CS.t Algorithms.results
			    
    type elt = CS.t
	    
    type graph = G.t

    module Ford = Algorithms.GCCFord(CS)(G)
				    
    let generalized_ford = Ford.algorithm

    let string = Algorithms.string (module CS)
      						      
  end
	
module MakeSR(S:Semiring)(G:Sig.G
				with type V.t = int
				 and type V.label = int
				 and type E.t = int * S.t * int
				 and type E.label = S.t)
	     (Q:Queue)
  =
  struct

    type t = S.t Algorithms.results

    type elt = S.t

    type graph = G.t

    type stepper = graph ->
		   int Q.t ->  S.t IntMap.t -> S.t IntMap.t ->
		   (int Q.t * S.t IntMap.t * S.t IntMap.t)
					    
    (*module GSSD = Algorithms.GSSD(S)(G)(Q)*)

    module M = Matrix.Make(S)
					 
    module Iter = Algorithms.Iteration(S)(G)(M)


    let mohri_stepper g q d r =
      if q = Q.empty then
	(q,d,r)
      else
	let (current_node,q) = Q.dequeue q in
	let r' = IntMap.find current_node r in	  
	let r = IntMap.add current_node S.neutral_plus r in   
	let update q d r edge =
	  let (_,edge,out_node) = edge in
	  let dout = IntMap.find out_node d in
	  let rout = IntMap.find out_node r in
	  let dout' = S.plus dout (S.times r' edge) in	  
	  if dout = dout' then
	    (q,d,r)
	  else
	    let d = IntMap.add out_node dout' d in	      
	    let rout' = S.plus rout  (S.times r' edge) in	      	  
	    let r = IntMap.add out_node rout' r in
	    if Q.mem out_node q then
	      (q,d,r)
	    else
	      (Q.enqueue out_node q,d,r)
	in
	G.fold_succ_e (fun e (q,d,r) ->
		       update q d r e) g current_node (q,d,r)
      

				      
    let stepper_algorithm stepper g start_node =      
      let initialize node map =
	if node = start_node then
	  IntMap.add node S.neutral_times map
	else
	  IntMap.add node S.neutral_plus map
      in
      let d = G.fold_vertex initialize g IntMap.empty in
      let r = G.fold_vertex initialize g IntMap.empty in
      let q = Q.enqueue start_node (Q.empty) in
      let rec iteration q d r =
      if q = Q.empty then
	d
      else
	let (q,d,r) = stepper g q d r
	in
	iteration q d r
      in
      {Algorithms.empty_results with Algorithms.final_cost = (iteration q d r)}

				      
    (*let generic_single_source = GSSD.algorithm*)

    let string = Algorithms.string (module S)

    let left_iteration'' g node =
      let matrix = Iter.left_iteration'' g in
      Iter.get_row matrix node 


    let left_iteration' g node =
      let matrix = Iter.left_iteration' g in
      Iter.get_row matrix node 

				   
    let left_iteration g node =
      let matrix = Iter.left_iteration g in
      Iter.get_row matrix node 

    let result r i = IntMap.find i r.Algorithms.final_cost
		   
  end
	
			 
