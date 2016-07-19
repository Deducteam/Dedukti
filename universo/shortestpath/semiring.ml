module type CostStructure =
  sig
    type t
    val times : t -> t -> t
    val neutral_times : t
    val neutral_plus : t
    val compare : t -> t -> int
    val string : t -> string
  end
    

module type Semiring =
  sig
    type t
    val plus : t -> t -> t
    val times : t -> t -> t
    val neutral_plus : t
    val neutral_times : t
    val string : t -> string
  end

    

module type Dioid =
  sig
    include Semiring
    val compare : t -> t -> int
  end


    
module Countable:Semiring =
  struct
    type t = int
    let plus = (+)
    let times = ( * )
    let neutral_plus = 0
    let neutral_times = 1
    let string t = string_of_int t
  end

module Product(S:Semiring)(T:Semiring) : (Semiring with type t = S.t * T.t) =
  struct
    type t = S.t * T.t
    let plus (a,b) (c,d) = (S.plus a c, T.plus b d)
    let times (a,b) (c,d) = (S.times a c, T.times b d)
    let neutral_plus = (S.neutral_plus , T.neutral_plus)
    let neutral_times = (S.neutral_times, T.neutral_times)
    let string (a,b) = "("^(S.string a)^";"^(T.string b)^")"
  end


    
let band t t' =
  match (t,t') with
  | true, true -> true
  | _ -> false

let bor t t' =
  match (t,t') with
  | false, false -> false
  | _ -> true
    
module Boolean:(Semiring with type t = bool) =
  struct
    type t = bool
    let plus = band
    let times = bor
    let neutral_plus = true
    let neutral_times = false
    let string t =
      match t with
      | true -> "true"
      | false -> "false"
  end
  



    
type 'a cloture = Infinity | N of 'a
    
module Tropical:(Dioid with type t = int cloture) =
  struct
    type t = int cloture
    let plus x y =
      match x with
      | Infinity -> y
      | N(x') -> match y with
		   | Infinity -> x
		   | N(y') -> if x'< y' then x else y
				   		     
    let times x y =
      match (x,y) with
      | (Infinity, _)
      | (_,Infinity) -> Infinity
      | (N(x'),N(y')) -> N(x'+y')
    let neutral_plus = Infinity
    let neutral_times = N(0)

    let compare t t' =
      match (t,t') with
      | (Infinity, Infinity) -> 0
      | (N(x), N(y)) when x = y -> 0
      | (Infinity, _) -> 1
      | (_, Infinity) -> -1
      | (N(x), N(y)) -> compare x y


    let string x =
      match x with
      | Infinity -> "+inf"
      | N(x) -> string_of_int x
  end

module Discounting:(Semiring with type t = (int * int) cloture * int cloture) =
  struct
    type t = (int * int) cloture * int cloture

    let frac_smaller (n,d) (n',d') =
      if n' * d - n *d' > 0 then
	true
      else
	false

    let closure_int_smaller x y =
      match (x,y) with
      | (Infinity, Infinity) -> true
      | (Infinity, _) -> false
      | (_, Infinity) -> true
      | (N(i), N(i')) -> i<=i'

    let closure_frac_smaller x y =
      match (x,y) with
      | (Infinity, Infinity) -> false
      | (Infinity, _) -> false
      | (_, Infinity) -> true
      | (N(x),N(y)) -> frac_smaller x y

    let neutral_plus = (Infinity, Infinity) 

    let neutral_times = (N(0,1), N(0))
			 
    let plus (f,c) (f',c') =
      if closure_frac_smaller f f' then
	(f,c)
      else
	if (f=f' && (closure_int_smaller c c')) then
	  (f,c)
	else
	  (f',c')

    let times (f,c) (f',c') =

      let rec pow b =
	if b = 0 then
	  1
	else
	  if b = 1 then
	    2
	  else
	    if b mod 2 = 0 then
	      pow(b/2) * pow(b/2)
	    else
	      2* pow(b-1)
      in
      let rec normalize (n,d) =
	if n mod 2 = 0 && d mod 2 = 0 then
	  normalize (n/2,d/2)
	else
	  (n,d)
      in
      match (f,f') with
      | (Infinity, _)
      | (_, Infinity) ->
	 begin
	   match (c,c') with
	   | (Infinity, _)
	   | (_, Infinity) -> (Infinity, Infinity)
	   | (N(i),N(i')) -> (Infinity, N(i+i'))
	 end
      | (N(f), N(f')) ->
	 begin
	 match c with
	 | Infinity -> (N(f),Infinity)
	 | N(c) ->
	    let (n,d) = f in
	    let (n',d') = f' in
	    let p = pow(c) in	    
	    let nc = (p * d' * n + n' * d, d * d' * p) in
	    let nc = normalize(nc) in	    
	    begin
	      match c' with
	      | Infinity -> (N(nc), Infinity)
	      | N(c') -> (N(nc), N(c+c'))
	    end
	 end
	   
    let string (f,i) =
      let left =
	match f with
	| Infinity -> "+inf"
	| N(n,d) -> (string_of_int n)^"/"^(string_of_int d)
      in      
      let right =
	match i with
	| Infinity -> "+inf"
	| N(i) -> (string_of_int i)
      in
      "( "^left^" , "^right^" )"

      
  end


    
module type Int =
  sig
    val int : int
  end
    
module KShortest(K:Int)(S:Dioid) =
  struct
    type t =  S.t list

    let neutral_plus =
      let rec aux x =
	if x = 0 then
	  []
	else
	  S.neutral_plus::(aux (x-1))
      in
      aux (K.int)
	  
    let neutral_times =
      let rec aux x =
	if x = 0 then
	  []
	else
	  if x= 1 then
	    [S.neutral_times]
	  else
	    S.neutral_plus::(aux (x-1))
      in
      List.rev (aux (K.int))

    let cut' n list =
      let rec aux n l =
	if n = 0 then [] else (List.hd l)::(aux (n-1) (List.tl l))
      in
      aux n list
	  
    let cut list =
      cut' (K.int) list
	   
    let plus t t' =
      cut (List.sort (S.compare) (t@t'))
	  
    let times t t' =
      let rec combine l l' =
	match l with
	| [] -> []
	| x::t -> (List.map (fun x' -> S.times x x') l')@(combine t l')
      in
      cut (List.sort (S.compare) (combine t t'))

    let string x =      
      let rec aux = function	
	  | [] -> ""
	  | [x] -> S.string x
	  | x::t -> (S.string x)^","^(aux t)
      in
      "("^(aux x)^")"
      
  end


module KDiffShortest(K:Int)(S:Dioid)  =
  struct
    type t =  S.t list

    let neutral_plus =
      let rec aux x =
	if x = 0 then
	  []
	else
	  S.neutral_plus::(aux (x-1))
      in
      aux (K.int)
	  
    let neutral_times =
      let rec aux x =
	if x = 0 then
	  []
	else
	  if x= 1 then
	    [S.neutral_times]
	  else
	    S.neutral_plus::(aux (x-1))
      in
      aux (K.int)

    let cut' n list =
      let rec aux n l =
	if n = 0 then
	  []
	else
	  match l with
	  | [] ->  S.neutral_plus::(aux (n-1) [])
	  | x::t -> x::(aux (n-1) t)
      in
      aux n list
	  
    let cut list =
      cut' (K.int) list
	   
    let plus t t' =
      cut (List.sort_uniq (S.compare) (t@t'))

    let times t t' =
      let rec combine l l' =
	match l with
	| [] -> l'
	| x::t -> (List.map (fun x' -> S.plus x x') l')@(combine t l')
      in
      cut (List.sort_uniq (S.compare) (combine t t'))

	  
    let string x =      
      let rec aux = function	
	  | [] -> ""
	  | [x] -> S.string x
	  | x::t -> (S.string x)^","^(aux t)
      in
      "("^(aux x)^")"
      
  end

