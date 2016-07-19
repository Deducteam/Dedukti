open Semiring

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
				   
    val set : int -> int  -> elt -> t -> t

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

    val equal : t -> t -> bool
				     
    val string : t -> string
						
  end


module Make(S:Semiring) =
  struct
    type elt = S.t
    type t = S.t Parray.t Parray.t

    let rec int_fold f n d =
      if n = 0 then
	d
      else
	int_fold f (n-1) (f (n-1) d)

		 
    let create n m default =
      Parray.create n (Parray.create m default)

    let size matrix =
      let n = Parray.length matrix in
      if n = 0 then
	assert false
      else
	let m = Parray.length (Parray.get matrix 0 ) in
	(n, m)
	  
    let get n m matrix =
      Parray.get (Parray.get matrix n ) m

    let set n m value matrix =
      let line =  (Parray.get matrix n) in
      Parray.set matrix n (Parray.set line m value)

    let add matrix matrix' =
      let (n,m) = size matrix in
      let (n',m') = size matrix' in
      assert (n=n' && m = m');
      let matrix'' = create n m S.neutral_plus in
      int_fold (fun n mx ->
		int_fold (fun m mx ->
			  set n m
			      (S.plus (get n m matrix)
				      (get n m matrix')
			      ) mx
			 ) m mx) n matrix''

    let dot_add_left matrix matrix' =
      let (n,m) = size matrix in
      let (n',m') = size matrix' in
      assert (n=n' && m = m');
      let matrix'' = create n m S.neutral_plus in
      int_fold (fun n mx ->
		int_fold (fun m mx ->
			  let left = get n m matrix in
			  let right = get n m matrix' in
			  let test= S.plus left right  in
			  let bool = 
			    if test = left then
			      S.neutral_plus
			    else
			      S.neutral_times
			  in
			    set n m bool mx
			 ) m mx) n matrix''


    let dot_add_right matrix matrix' =
      let (n,m) = size matrix in
      let (n',m') = size matrix' in
      assert (n=n' && m = m');
      let matrix'' = create n m S.neutral_plus in
      int_fold (fun n mx ->
		int_fold (fun m mx ->
			  let left = get n m matrix in
			  let right = get n m matrix' in
			  let test= S.plus left right  in
			  let bool = 
			    if test = right then
			      S.neutral_plus
			    else
			      S.neutral_times
			  in
			    set n m bool mx
			 ) m mx) n matrix''


	       
    let multiply matrix matrix' =
      let (n,m) = size matrix in
      let (m',o) = size matrix' in
      assert (m=m');
      let matrix'' = create n o S.neutral_plus in
      
      int_fold
	(fun n mx ->
	 int_fold
	   (fun o mx ->
	    set n o 
	    (
	      int_fold
		(fun m v ->
		 S.plus v
			(S.times (get n m matrix)
				 (get m o matrix')
			)
		) m S.neutral_plus
	    ) mx
	   ) o mx
	) n matrix''

    let pointwise_multiply matrix matrix' =
      let (n,m) = size matrix in
      let (n', m') = size matrix' in
      assert (n=n');
      assert (m=m');
      let matrix'' = create n m S.neutral_plus in
      int_fold
	(fun i mx ->
	 int_fold
	   (fun j mx ->
	    set i j (S.times (get i j matrix) (get i j matrix')) mx
	   ) m mx
	) n matrix''

    let dot_add bmatrix matrix matrix' =
      add (pointwise_multiply matrix bmatrix) matrix'

	
    (* TODO add some check *)
    let dot_multiply bmatrix matrix matrix' =
      multiply (pointwise_multiply matrix bmatrix) matrix' 
      (*
      let (n,m) = size matrix in
      let (m',o) = size matrix' in
      let matrix'' = 
      assert (m=m');
      int_fold
	(fun n mx ->
	 int_fold
	   (fun o mx ->
	    set n o 
	    (
	      int_fold
		(fun m v ->

		 S.plus v
			(S.times (S.times (get n m matrix)
					  (get n m bmatrix))
				 (get m o matrix')
			)
		) m S.neutral_plus
	    ) mx
	   ) o mx
	) n matrix''
						   *)

	
	
    let of_list l =
      assert (not (l = [[]]));
      let n = List.length l in
      let m = List.length (List.hd l) in
      assert (List.for_all (fun x -> (List.length x = m)) l);

      let foldi f l d =
	let rec foldi' f l d n =
	  match l with
	  | [] -> d
	  | x::t -> foldi' f l (f n x d) (n-1)
	in
	foldi' f l d 0
      in
      foldi (fun i l matrix ->
	     foldi (fun j v matrix ->
		    set i j v matrix
		  ) l matrix
	    ) l (create n m S.neutral_plus)


    let identity n =
      let m = create n n S.neutral_plus in
      int_fold (
	  fun n matrix ->
	  set n n S.neutral_times matrix
	) n m
      
    let equal m m' =
      let list = Parray.fold_left (fun l x ->
				   (Parray.to_list x)@l) [] m
      in
      let list' = Parray.fold_left (fun l x ->
				   (Parray.to_list x)@l) [] m'
      in
      list = list'

    let opposite matrix =
      let (n,m) = size matrix in
      let result = create n m S.neutral_plus in
      int_fold (fun n mx ->
		int_fold (fun m mx ->
			  let b = get n m matrix in
			  if b = S.neutral_plus then
			    set n m S.neutral_times mx
			  else
			    set n m S.neutral_plus mx
			 ) m mx) n result
      
    let xor matrix matrix' =
      let (n,m) = size matrix in
      let (n', m') = size matrix' in
      assert (n = n');
      assert (m = m');
      let result = create n m S.neutral_plus in
      int_fold(fun i mx ->
	       int_fold (fun j mx ->
			 let l = get i j matrix in
			 let r = get i j matrix' in
			 if l <> r then
			   set i j S.neutral_times mx
			 else
			   mx
			) m mx
	      ) n result

    let orb matrix matrix' =
      let (n,m) = size matrix in
      let (n', m') = size matrix' in
      assert (n = n');
      assert (m = m');
      let result = create n m S.neutral_plus in
      int_fold(fun i mx ->
	       int_fold (fun j mx ->
			 let l = get i j matrix in
			 let r = get i j matrix' in
			 if l <> r then
			   set i j S.neutral_times mx
			 else
			   set i j l mx
			) m mx
	      ) n result

	      
    let string m =
      Parray.fold_left
	(fun s m ->
	 (Parray.fold_left
	   (fun s v ->
	    s^(S.string v)^" ") s m
	 )^"\n"
	) "" m

      
  end
