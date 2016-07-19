open Graph
open Utils

       
type 'a final_cost = 'a IntMap.t

type path = int list

type 'a results =
    {
      final_cost : 'a final_cost ;
      path : path ;
    }

let empty_results =
  {
    final_cost = IntMap.empty ;
    path = [] ;
  }

module type Printable =
  sig
    type t
    val string : t -> string
  end
    
let string (type a) (module S:Printable with type t = a) (results) =
  let cost = results.final_cost in
  IntMap.fold (fun k v s ->
	       s^"Node: "^(string_of_int k)^" Value: "^(S.string v)^"\n")
	      cost ""


    
module GSSD(S:Semiring.Semiring)
	   (G:Sig.G  with type V.t = int
		      and type V.label = int
		      and type E.t = int * S.t * int
		      and type E.label = S.t)(Q:Queue)
  =
  struct
    let print_map name map =
      Printf.printf "\nMap : %s\n" name;
      IntMap.iter (fun k v ->
		   Printf.printf "Node : %d Value : %s\n" k
				 (S.string v)
		  ) map

(*
    let stepper_algorithm ?(trace=false) (module Q:Queue)
			  stepper g start_node =      
      let initialize node map =
	if node = start_node then
	  IntMap.add node S.neutral_times map
	else
	  IntMap.add node S.neutral_plus map
      in
      let d = G.fold_vertex initialize g IntMap.empty in
      let r = G.fold_vertex initialize g IntMap.empty in
      let q = Q.enqueue start_node (Q.empty) in
      if q = Q.empty then
	d
      else
	stepper ~trace:trace (module G : Sig.G  with type V.t = int
		      and type V.label = int
		      and type E.t = int * S.t * int
		      and type E.label = S.t) g d r
 *)		
    let algorithm ?(trace=false) g start_node =
      
      let print_queue q =
	Printf.printf "\nQueue : ";
	Q.iter (fun n -> 
		Printf.printf "%d " n) q
      in
      
      let initialize node map =
	if node = start_node then
	  IntMap.add node S.neutral_times map
	else
	  IntMap.add node S.neutral_plus map
      in
      let d = G.fold_vertex initialize g IntMap.empty in
      let r = G.fold_vertex initialize g IntMap.empty in
      let q = Q.enqueue start_node (Q.empty) in            
      let rec aux (d,r,q) =
	if trace then
	  print_queue q;
		
	if q = Q.empty then
	  d
	else
	  
	  let (current_node,q) = Q.dequeue q in
	  let r' = IntMap.find current_node r in
	  if trace then
	    begin
	      print_map "distance" d;
	      print_map "reset" r;
	      Printf.printf "\nNode visited : %d\n" current_node
	    end;
	  
	  let r = IntMap.add current_node S.neutral_plus r in
	  let update d r q edge =
	    let (_,edge,out_node) = edge in
	    let dout = IntMap.find out_node d in
	    let rout = IntMap.find out_node r in
	    let dout' = S.plus dout (S.times r' edge) in
	    
	    if trace then
	      begin
		Printf.printf "\nEdge visited : (%d,%d)\n"
			      current_node out_node;
		Printf.printf "r' is %s\n" (S.string r')
	      end;
	    
	    if dout = dout' then
	      (d,r,q)
	    else
	      let d = IntMap.add out_node dout' d in	      
	      let rout' = S.plus rout  (S.times r' edge) in	      
	      (*let rout' = S.times r' edge in*)

	      let r = IntMap.add out_node rout' r in
	      if Q.mem out_node q then
		(d,r,q)
	      else
		(d,r,Q.enqueue out_node q)
	  in
	  aux (G.fold_succ_e (fun e (d,r,q) ->
			    update d r q e) g current_node (d,r,q))
      in
      {empty_results with final_cost = (aux (d,r,q))}

    let string results =
      let cost = results.final_cost in
      IntMap.fold (fun k v s ->
		   s^"Node: "^(string_of_int k)^" Value: "^(S.string v)^"\n")
		  cost ""
      



end


    
module GCCFord(S:Semiring.CostStructure)
	   (G:Sig.G  with type V.t = int
		      and type V.label = int
		      and type E.t = int * S.t * int
		      and type E.label = S.t)
  =
  struct
    module TripleSet = Set.Make(
			   struct
			     type t = int * S.t * S.t
			     let compare = compare
			   end)

    module DistSet = Set.Make(
			 struct
			   type t = S.t
			   let compare = compare
			 end)

    type t = DistSet.t IntMap.t
(*
    let string_of_distSet set =
      DistSet.fold (fun e str ->
		    str^(S.string e)^" ") set ""
      *)
		  
    let algorithm ?(trace=false) g start_node =      
      let finished g f d =
	let for_all_edges_e f g =
	  G.fold_edges_e (fun e b ->
			  (f e) && b) g true
	in
	let criterion f d edge =
	  let (v,edge,w) = edge in
	  let dset = IntMap.find v d in
	  DistSet.for_all
	    (fun r ->
	     let dset' = IntMap.find w d in
	     DistSet.exists (fun r' ->
			     (S.compare r' (S.times r edge)) < 0
			     || (r' = (S.times r edge))) dset')
	    dset
	in
	for_all_edges_e (fun e ->
			 criterion f d e) g
      in
      
      let d = G.fold_vertex
		(fun x map ->
		 if x = start_node then
		   IntMap.add x (DistSet.singleton S.neutral_times) map
		 else
		   IntMap.add x (DistSet.singleton S.neutral_plus) map
		) g IntMap.empty
      in
      let f = G.fold_vertex
		(fun x map ->
		 if x = start_node then
		   IntMap.add x
			      (TripleSet.singleton
			      (x,S.neutral_times, S.neutral_times))
			      map
		 else
		   IntMap.add x TripleSet.empty map
		) g IntMap.empty
      in
      let rec aux g f d =

	let update f d r r' edge bool  =
	  let (v, edge, w) = edge in
	  let update = (S.times r edge) in
	  if (S.compare r' update) < 0 || update=r' then
	    (true, f,d)
	  else if (S.compare update r') < 0 then
	    let dset = IntMap.find w d in
	    let dset' = DistSet.add update (DistSet.remove r' dset) in
	    let d' = IntMap.add w dset' d in
	    let fset = IntMap.find w f in
	    let fset' = TripleSet.filter (fun (_,truc,_) ->
					   not (truc = r')) fset in
	    let fset' = TripleSet.add (v, update, r) fset' in
	    let f' = IntMap.add w fset' f in 
	    (true, f',d')
	  else
	    (bool, f,d)
	in

	let update_bool f d r edge =
	  let (v,edge,w) = edge in
	  let update = (S.times r edge) in
	  let dset = IntMap.find w d in
	  let dset' = DistSet.add update dset in
	  let fset = IntMap.find w f in
	  let fset' = TripleSet.add (v, update, r) fset in
	  let d' = IntMap.add w dset' d in
	  let f' = IntMap.add w fset' f in
	  (f',d')
	in
	let choose_r' f d r edge =
	  let (_,_,w) = edge in
	  DistSet.fold (fun r' (b,f,d) ->
			update f d r r' edge b)
		       (IntMap.find w d)
		       (false,f,d)
	in
	let choose_r f d edge =
	  let (v,_,_) = edge in
	  DistSet.fold(fun r (f,d) ->
		       let (b,f,d) = choose_r' f d r edge in
		       if b = false then
			 update_bool f d r edge
		       else
			 (f,d)			 
		      )
		      (IntMap.find v d) (f,d)
	in
	if finished g f d then
	  d
	else
	  let (f,d) = G.fold_edges_e (fun edge (f,d) ->
				      choose_r f d edge) g (f,d)
	  in
	  aux g f d
      in
      (* TODO check this method *)
      let format map =
	let results = empty_results in
	let cost = 
	IntMap.fold (
	    fun k v map ->
	    IntMap.add k (DistSet.choose v) map
	  ) map IntMap.empty
	in
	{results with final_cost = cost}
      in
      format (aux g f d)
	  

end


    
module Iteration(S:Semiring.Semiring)
	   (G:Sig.G  with type V.t = int
		      and type V.label = int
		      and type E.t = int * S.t * int
		      and type E.label = S.t)
	   (M:Matrix.S with type elt = S.t)
  =
  struct
    let to_adjancy_matrix g =
      let nb_vertex = G.nb_vertex g in
      let matrix = M.create  nb_vertex nb_vertex S.neutral_plus in     
      
      G.fold_edges_e (fun (v,l,w) matrix ->
		      M.set v w l matrix
		     )  g matrix
    let d = ref 0
    let left_iteration g =
      let m = to_adjancy_matrix g in
      let n = G.nb_vertex g in
      let rec iterate matrix_i =
	incr d;
	let matrix_i' = M.add (M.multiply matrix_i m) (M.identity n) in
	Printf.printf "%s\n" (M.string matrix_i);
	if M.equal matrix_i' matrix_i then
	  matrix_i
	else
	  iterate matrix_i'
      in
      iterate (M.identity n)

    let c = ref 0 
    let left_iteration' g =
      let m = to_adjancy_matrix g in
      (*Printf.printf "Adjancy matrix : \n";
      Printf.printf "%s\n" (M.string m);*)
      let n = G.nb_vertex g in
      let rec iterate x y b =
	incr c;
	let y' = M.dot_multiply b y m in
	(*Printf.printf "Y Matrix : \n";
	Printf.printf "%s\n" (M.string y');*)
	let x' = M.add x y'  in
	(*Printf.printf "X matrix : \n";
	Printf.printf "%s\n" (M.string x');*)
	let b' = M.dot_add_left x y' in
	(*let y'' = M.add y' (M.dot_multiply (M.opposite b) y (M.identity n)) in*)
	(*Printf.printf "B matrix : \n";
	Printf.printf "%s\n" (M.string b');*)
	(*Printf.printf "B matrix : \n";
	Printf.printf "%s\n" (M.string r');*)		
	if M.equal x' x then
	  begin
	    Printf.printf "Steps : %d\n" !c;
	    x
	  end
	else
	  iterate  x' y' b'  
      in
      iterate (M.identity n) (M.identity n) (M.identity n)


    let left_iteration'' g =
      let rec int_fold f n d =
	if n = 0 then
	  d
	else
	  int_fold f (n-1) (f (n-1) d)
      in
      let choice b =
	let (n,m) = M.size b in
	let s = M.create n m S.neutral_plus in
	int_fold (fun i mx ->
		  snd (int_fold (fun j (bool, m) ->
				 if bool then				   				
				   let v = M.get i j b in
				   if v = S.neutral_times then
				     (false, M.set i j (S.neutral_times) m)
				   else
				     (true,m)
				 else
				   (false,m)
			   ) m (true,mx))
		 ) n s
      in
      let m = to_adjancy_matrix g in
      let n = G.nb_vertex g in
      let rec iterate x y b  =
	let s =  choice b in
	Printf.printf "B Matrix : \n";
	Printf.printf "%s\n" (M.string b);
	Printf.printf "S Matrix : \n";
	Printf.printf "%s\n" (M.string s);
	let t = M.dot_multiply s y m in
	let y' = M.dot_add (M.opposite s) y t in
	let x' = M.add x t  in
	Printf.printf "New Matrix : \n";
	Printf.printf "%s\n" (M.string (M.dot_add_left x t));
	Printf.printf "T Matrix : \n";
	Printf.printf "%s\n" (M.string t);
	Printf.printf "Y Matrix : \n";
	Printf.printf "%s\n" (M.string y');
	let b' = M.orb (M.xor s b) (M.dot_add_left x t) in
	(*dirty but i want to be quick *)
	if M.equal b' (M.xor b' b') then
	  begin
	    Printf.printf "Steps : %d\n" !c;
	    x
	  end
	else
	  iterate  x' y' b'
      in
      iterate (M.identity n) (M.identity n) (M.identity n)

      
    let get_row matrix row =
      
      let rec int_fold f n d =
	if n = 0 then
	  d
	else
	  int_fold f (n-1) (f (n-1) d)
      in
      let cost = int_fold (fun n map ->
			   IntMap.add n (M.get row n matrix) map)
			  (fst (M.size matrix))
			  IntMap.empty
      in
      {empty_results with final_cost = cost}
	      
  end
    
