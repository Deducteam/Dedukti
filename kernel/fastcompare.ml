open Constsigntype

(*add the number of constant*)
let counter = ref 0;;
let gensym () = let n = !counter in
		counter :=n+1;
		n;;

module Const = struct
  type t = const
  let compare = compare
end;;


module Table_const = Map.Make(Const);;
(*representative contains the canonic or representative of each constant*)
let representative = ref (Table_const.empty : const Table_const.t);;
(*create a key a which contains its representative b for the first time, if it's already existed, it doesn't change*)
let representative_new a b = if not(Table_const.mem a !representative) then
    representative := Table_const.add a b !representative;;
(*modify the value of the representative of a*)
let representative_set a b = representative := Table_const.add a b !representative;;
(*find the representative of a and return it*)
let representative_value a = Table_const.find a !representative;;
(*find the representative of each element of the list and return a list of representatives*)
let rec representative_list_value l = match l with
    [] -> []
  | a::ll -> (Table_const.find a !representative)::(representative_list_value ll);;

module Signature = struct
	type t = signature
	let compare = compare
	end;;

module Table_sign = Map.Make(Signature);;
(*representative_value_sign return a representative of constant of a signature*)
let representative_value_sign sign = match sign with
    App1(a1,a2,l) -> App1(representative_value a1, representative_value a2, representative_list_value l)
  | Lam1(None,a2) ->  Lam1(None,representative_value a2)
  | Lam1(Some a1,a2) -> Lam1(Some (representative_value a1),representative_value a2)
  | Pi1(a1,a2) -> Pi1(representative_value a1, representative_value a2)
  | Clos1(a1,l2) -> Clos1(representative_value a1, representative_list_value l2);;

(*lookup is a table which a canonical representative of a signature returns to a constant*)
let lookup = ref (Table_sign.empty : const Table_sign.t);;

(*lookupinv is the inverse table of the lookup*)
let lookupinv = ref (Table_const.empty : signature Table_const.t);;

(*create a representative index of a signature sign which contain a constant b in the lookup*)
let lookup_new sign b = let repr = representative_value_sign sign in
			    lookup := Table_sign.add repr b !lookup;
			    lookupinv := Table_const.add b repr !lookupinv;;
			    
(*modify the constant of the key a of lookup*)
let lookup_set a b =
  lookup := Table_sign.add a b !lookup;
  lookupinv := Table_const.add b a !lookupinv;;

(*finds the constant of the signature sign in the lookup table and returns it*)
let lookup_value sign = Table_sign.find sign !lookup;;

(*finds the signature of the cosntant const in the lookup table and returns it*)
let lookupinv_value const = Table_const.find const !lookupinv;;

(*uselist contains a list of equations which a is present in one of the elements in the signature*)
let uselist = ref Table_const.empty;;

(*create a empty list in the key a in the uselist table*)
let uselist_empty a = uselist := Table_const.add a [] !uselist;;

(*add elements in the index of a in the uselist*)
let uselist_add a b =
  if Table_const.mem a !uselist then 
    uselist := Table_const.add a (List.append b (Table_const.find a !uselist)) !uselist
  else
    uselist := Table_const.add a b !uselist;;

(**)
let rec uselist_list_add (sign,l,n) = match sign,l with
    _,[] -> ()
  | App1(a,b,l1),c::ll ->
     if Table_const.mem c !uselist then 
       uselist := Table_const.add c (List.append [(App1(a,b,l1),n)] (Table_const.find c !uselist)) !uselist
     else
       (
	 uselist := Table_const.add c [(App1(a,b,l1),n)] !uselist;
	 uselist_list_add (App1(a,b,l1),ll,n)
       )
  | other,ll -> failwith "impossible d'etudier";;

(*add in the uselist the signature in the constant present indexes in the signature*)
let uselist_add_sign sign n = match sign with
    App1(c1,c2,l) ->
      uselist_add c1 [(App1(c1,c2,l),n)];
      uselist_add c2 [(App1(c1,c2,l),n)];
      uselist_list_add (App1(c1,c2,l),l,n)
  | Lam1(None,t2) -> uselist_add t2 [(Lam1(None,t2),n)]
  | Lam1(Some t1,t2) ->
     uselist_add t2 [(Lam1(Some t1,t2),n)]
  | Pi1(t1,t2) ->
    uselist_add t1 [(Pi1(t1,t2),n)];
    uselist_add t2 [(Pi1(t1,t2),n)];
  | Clos1(c1,l) ->
     uselist_add c1 [(Clos1(c1,l),n)];;

(*find the list of index a in uselist and return it*)
let uselist_value a = if Table_const.mem a !uselist then
				Table_const.find a !uselist
			else
				(uselist_empty a;
				 Table_const.find a !uselist);;


module Const_set = Set.Make(Const);;

(*classlist is a table of set of the equivalent representatives for each constant*)
let classlist = ref Table_const.empty;;

(*classlist_set set or add in the index table a with the value b*)
let classlist_set a b = classlist := Table_const.add a b !classlist;;

(*classlist_value return the list of all constants for the representative a*)
let classlist_value a = Table_const.find a !classlist;;

(*initialize classlist based on the representative table*)
let representative_to_classlist key password = if not(Table_const.mem key !classlist) then
    classlist_set key (Const_set.singleton key);;

let classlist_init () = Table_const.iter representative_to_classlist !representative;;
(*create a signature index in the lookup which contain a new constant b*)
(*create a new constant and update the lookup, uselist and representative*)
let constant_new sign = let n = gensym () in
	  (representative_new (E(n)) (E(n));
	   lookup_new sign (E(n));
	   uselist_add_sign sign (E(n));
	   (E(n)));;

(*check if sign exist in uselist, else it create a new representative of E(n), a new element on lookup and uselist*)
let rec belong sign l= match l with
    [] -> constant_new sign
  |(a1,c)::ll -> if a1 = sign then
			c
		    else belong sign ll;;

(*it permits to decompose the term into several different subterms and a equivalent constant and update the uselist, representative and lookup*)
let rec flatten t = match t with
  | Kind -> representative_new (Kind1) (Kind1);
    Kind1
  |Type(loc) -> representative_new (Type1) (Type1);
    Type1
  |DB(loc,x,z) -> representative_new (DB1(z)) (DB1(z));
    DB1(z)
  |Const(loc,str) -> representative_new (Const1(str)) (Const1(str));
    Const1(str)
  |App(t1,t2,list) ->
     let c1 = (flatten t1) in
      let c2 = (flatten t2) in
       let lc = (flattenlist list) in
        let l = uselist_value c1 in
         let b = belong (App1(c1,c2,lc)) l in 
	 b
  |Lam(loc,x,None,t) ->
     let c = flatten t in
     let l = uselist_value c in
     let b = belong (Lam1(None,c)) l in
     b
  |Lam(loc,x,Some t1,t2) ->
     let c1 = flatten t1 in
     let c2 = flatten t2 in
     let l = uselist_value c1 in
     let b = belong (Lam1(Some c1,c2)) l in
     b
  |Pi(loc,x,t1,t2) ->
     let c1 = flatten t1 in
     let c2 = flatten t2 in
     let l = uselist_value c1 in
     let b = belong (Pi1(c1,c2)) l in
     b
and
 (*flattenlist takes a list of terms as argument and return a list of constant flattened*)
 flattenlist l = match l with
  | [] -> []
  | a::ll -> (flatten a)::(flattenlist ll);;
     
let pending = ref [];;

(*pending_lu store all the equation viewed*)
let pending_lu = ref [];;

(*add a equation in the pending list*)
let pending_add (s,t)=
  pending := (s,t)::!pending;
  pending_lu := (s,t)::!pending_lu;;

(* old_repr_a constains the former representative of a*)
let old_repr_a = ref (E(0));;

(*path compression*)
let rec for_path_comp cla a b= match cla with
    [] -> let classlist_value_new = (Const_set.union (classlist_value (!old_repr_a)) (classlist_value (representative_value b))) in
	  classlist_set (representative_value b) classlist_value_new;
	  classlist_set (!old_repr_a) Const_set.empty;
	  ()
  | c::ll -> 
     representative_set c (representative_value b);
    for_path_comp ll a b;;

(*update the uselist, lookup and pending*)
let rec for_update_cong usel a b = match usel with
    []-> uselist_empty (!old_repr_a)
  | (sign,c)::ll -> let repr_sign = representative_value_sign sign in
			  if Table_sign.mem repr_sign !lookup && c != lookup_value repr_sign then
      begin
	(*if (c, lookup_value repr_sign) is already checked, we don't need to see again this equation*)
	if not(List.mem (c, lookup_value repr_sign) !pending_lu) then
            pending_add (c, lookup_value repr_sign)
      end
    else
      begin
	(*add in lookup the representative of the signature sign to c and add in representative of uselist b the sign and its constant*)
	lookup_set repr_sign c;
	uselist_add (representative_value b) [(sign,c)]
      end;
	for_update_cong ll a b;;

(*propagate pick a pending equation then *)
let rec propagate () = match !pending with
    [] -> ()
  | (a,b)::l -> pending := l;
    if representative_value a != representative_value b then
      begin
	if Const_set.cardinal (classlist_value a) <= Const_set.cardinal (classlist_value b) then
	  begin
	    old_repr_a := representative_value a;
	    (*modify the representative of the classlist of old representative to b *)
	    for_path_comp (Const_set.elements (classlist_value (!old_repr_a))) a b;
	    (*update the pending list, lookup and uselist*)
	    for_update_cong (uselist_value (!old_repr_a)) a b
	  end
	else
	  begin
	    old_repr_a := representative_value b;
	    for_path_comp (Const_set.elements (classlist_value (!old_repr_a))) b a;
	    for_update_cong (uselist_value (!old_repr_a)) b a
	  end
      end;
    propagate ()
;;

(*merge a equation in the congruence closure*)
let merge (s,t) =
  pending_add (s, t);
  classlist_init ();
  propagate ()
;;

(*normalize a list of terms*)
let rec normalize_list l = match l with
    [] -> []
  | a::ll -> (normalize a)::(normalize_list ll)

(*normalize a term to find a existant representative and return a constant. If the term doesn't assimilate to a constant, it will create a new constant*)
and normalize t = match t with
    Kind -> representative_value (Kind1)
  |Type(loc) -> representative_value (Type1)
  |DB(loc,x,z) -> representative_value (DB1(z))
  |Const(loc,str) -> representative_value (Const1(str))
  |App(t1,t2,l) -> let u1 = normalize t1 in
		   let u2 = normalize t2 in
		   let lu = normalize_list l in
		 if Table_sign.mem (App1(u1,u2,lu)) !lookup then
		   representative_value (lookup_value (App1(u1,u2,lu)))
		 else
		   begin
		     belong (App1(u1,u2,lu)) [];
		     lookup_value (App1(u1,u2,lu))
		   end
  |Lam(loc,x,Some t1,t2) -> let u1 = normalize t1 in
			    let u2 = normalize t2 in
			if Table_sign.mem (Lam1(Some u1,u2)) !lookup then
		   representative_value (lookup_value (Lam1(Some u1,u2)))
		 else
		   begin
		     belong (Lam1(Some u1,u2)) [];
		     lookup_value (Lam1(Some u1,u2))
		   end
  |Lam(loc,x,None,t) -> let u = normalize t in
			if Table_sign.mem (Lam1(None,u)) !lookup then
		   representative_value (lookup_value (Lam1(None,u)))
		 else
		   begin
		     belong (Lam1(None,u)) [];
		     lookup_value (Lam1(None,u))
		   end
  |Pi(loc,x,t1,t2) ->
     let u1 = normalize t1 in
     let u2 = normalize t2 in
		 if Table_sign.mem (Pi1(u1,u2)) !lookup then
		   representative_value (lookup_value (Pi1(u1,u2)))
		 else
		   begin
		     belong (Pi1(u1,u2)) [];
		     lookup_value (Pi1(u1,u2))
		   end

(*check if s and t are equivalent*)
let areCongruent (s,t) = if normalize s = normalize t then
    true
  else
    false;;

let numiter = ref 0;;

(*create and return a new closure constant if Clos1(ct,e) doesn't exist else return the constant of the signature in the lookup*)
let rec cst_clos ct e =
if Table_sign.mem (Clos1(ct,e)) !lookup then
  lookup_value (Clos1(ct,e))
else
  constant_new (Clos1(ct,e));;
 (*cst_clos_list takes a list of signature as a argument and return a list of closure constants*)
let rec cst_clos_list lc e =
  match lc with
    [] -> []
  | a::ll -> (cst_clos a e)::(cst_clos_list ll e)
;;

(*compare_list compare a list of constant, it is used for App1*)
let rec compare_list l1 l2 = match l1,l2 with
    [],[] -> true
  | [],_ -> false
  | _,[] -> false
  | a1::ll1, a2::ll2 -> compare a1 a2 && compare_list ll1 ll2
and
 (*compare2 compare 2 signatures and call compare again depending the type of signature c1 and c2*)
 compare2 (Clos1(c1,e1)) (Clos1(c2,e2)) =
  match c1,c2 with
    Type1,Type1 -> true
  | Kind1,Kind1 -> true
  | DB1(n),_ -> let e11 = List.nth e1 n in
		  compare e11 (lookup_value (Clos1(c2,e2)))
  | _, DB1(m) -> let e22 = List.nth e2 m in
		  compare (lookup_value (Clos1(c1,e1))) e22
  | E(_),E(_) ->
     let t1 = lookupinv_value c1 in
     let t2 = lookupinv_value c2 in
     (*if the constant c1 and c2 are E(_), it means they are signatures and compares each elements of signature*)
     (match t1,t2 with
       App1(a1,b1,l1),App1(a2,b2,l2) ->
	 let a11 = cst_clos a1 e1 in
	 let b11 = cst_clos b1 e1 in
	 let a22 = cst_clos a2 e2 in
	 let b22 = cst_clos b2 e2 in
	 let l11 = cst_clos_list l1 e1 in
	 let l22 = cst_clos_list l2 e2 in
	 compare a11 a22 && compare b11 b22 && compare_list l11 l22
     | Lam1(None,a1),Lam1(None,a2) -> let c = gensym () in
				      let a11 = cst_clos a1 ((E(c))::e1) in
				      let a22 = cst_clos a2 ((E(c))::e2) in
				      compare a11 a22
     | Lam1(Some a1,b1),Lam1(Some a2,b2) -> let c = gensym () in
					    let a11 = cst_clos a1 e1 in
					    let a22 = cst_clos a2 e2 in
					    let b11 = cst_clos b1 ((E(c))::e1) in
					    let b22 = cst_clos b2 ((E(c))::e2) in
					    compare a11 a22 && compare b11 b22
     | Pi1(a1,b1),Pi1(a2,b2) -> let c = gensym () in
				let a11 = cst_clos a1 e1 in
				let a22 = cst_clos a2 e2 in
				let b11 = cst_clos b1 ((E(c))::e1) in
				let b22 = cst_clos b2 ((E(c))::e2) in
				compare a11 a22 && compare b11 b22
     | _,_ -> false)
  |Const1(a),Const1(b) -> if a = b then
      true
    else
      false
  |_,_ -> false
and
 (*compare check if c1 and c2 are equivalent else it will compare the subterms of c1 and c2 and merge if it is equivalent*)
 compare c1 c2 =
  numiter := !numiter + 1;
  if representative_value c1 = representative_value c2 then
    true
  else
    let Clos1(ct1,e1) = lookupinv_value c1 in
    let Clos1(ct2,e2) = lookupinv_value c2 in
    if (compare2 (Clos1(ct1,e1)) (Clos1(ct2,e2))) then
      (
	merge (c1,c2);
	true
      )
    else
      false
    ;;
