
open Basics;;
open Multi_set;;
open Diophantienne;;
open Acterm;;

  

module A = struct
  type t = acterm
  let compare = acterm_cmpr
end;;

module L =
struct
  include Map.Make (A)
  let update k v m =
    begin
      let r =
        try Some (find k m)
        with Not_found -> None
      in
      match r with
      | None -> add k [v] m
      | Some x -> add k (x @ [v]) m
    end
  let si_of_l l s =
    let tr arg = List.map (fun x -> let (k, v) = x in (k, mk_AC_app2 s (list_of_multiset (Multiset v)))) arg in
    List.fold_left (fun si (x,y) -> Si.add x y si) (Si.empty) (tr (bindings l)) 
end;;



(* resoud l'equation diophantienne associe au deux termes AC *)
let solve_dioph ac1 ac2 =
  let equat_of_purifylist ac1 ac2 = match ac1, ac2 with
  | AC_app2 (s1, Multiset l1), AC_app2 (s2, Multiset l2) ->
    let aux e = let (Elem(m, _)) = e in m in
    let aux2 l = Coefficient (List.map aux l) in
    Equation(aux2 l1, aux2 l2)
  | _ -> failwith "Error : solv_dioph"
  in
  let equ = equat_of_purifylist ac1 ac2 in
  VectSet.elements (procedure equ) ;;



(* - pour chaque solution de la base on creer une nouvelle variable 
     ET on lui associe sa variable purifie corespondante *)
let assocvar ac1 ac2 r = match ac1, ac2 with
  | AC_app2 (s1, Multiset l1), AC_app2 (s2, Multiset l2) ->
    let rec aux l1 l2 r map i = match r with
      | [] -> map
      | h::tl ->
        match h with VectMod.Vect(a, b) ->
          let rec aux2 p q map i = match p, q with
            | [], [] -> map
            | Elem(m1, var)::tl1, m2::tl2 ->
              if m2 = 0 then
                aux2 tl1 tl2 map i
              else
                let map' = L.update var (Elem(m2, mk_AC_varspe (hstring "v") i)) map in
                aux2 tl1 tl2 map' i
            | _ -> failwith "impossible"
          in
          let map' = aux2 l1 a map i in
          let map' = aux2 l2 b map' i in
          aux l1 l2 tl map' (i+1)
    in
    L.si_of_l (aux l1 l2 r (L.empty) 1) s1
  | _ -> failwith "Error : no AC term";;
        

(* - on construit maintenant la substitution *)
let purifyac_to_assocvar ac1 ac2 =
  let r = solve_dioph ac1 ac2 in
  assocvar ac1 ac2 r

let getSymb si1 si2 =
  let aux si e = let (k, v) = e in
    ( (k, v) , Si.find k si )
  in
  List.map (aux si2) (Si.bindings si1);;
let getVar si =
  List.map (fun (k,v) -> ((k,k),v)) (Si.bindings si);;

  
  
(**** Procedure d'unification ****)

exception SymbolClash;;
exception OccursCheck;;
let si = Si.empty;;

(* Renvoi un  unificateur (le premier) de s et t *)

let rec unify s t si =


  let (s, t) = (sub_term si s, sub_term si t) in

  if acterm_eq s t then [ si ]
  else begin
    match s, t with

    (* unification de deux const diff *)
    | AC_const (_,_,i) , AC_const (_,_,i') -> 
      if ident_eq i i' then [ si ]
      else begin [] end

    (* Unification de deux app *)
    | AC_app (s', ls), AC_app (t', lt) ->
      if acterm_eq s' t' then
        let rec aux l1 l2 si' = match l1, l2 with
          | [], [] -> [ si' ]
          | ([], _ | _, []) -> failwith "Error: unify"
          | h1 :: tl1 , h2 :: tl2 ->
            let rec aux' tsi = match tsi with
              | [] -> []
              | h' :: tl' ->
                (aux tl1 tl2 h') @ aux' tl'
            in 
            aux' (unify h1 h2 si')
        in
        match (aux ls lt si) with
        | [] ->  []
        | res -> res 

      else [] (* SymbolClash *)

    (* Unification d'une variable avec un term *)
    | AC_var _, _ ->
      if is_occurs s t then []
      else [ Si.add s t si ]

    (* Unification d'un term avec une variable *)
    | _ , AC_var _ -> unify t s si

    (* Unification de deux symbol AC --> algo unification modulo AC *)
    | AC_app2 (a,_), AC_app2 (b,_) ->

      begin
	(* procedure pour l'unification modulo ac *)
	let rec unifyAC l sigma real_sigma = match l with
          | [] -> 
            [ sub_si sigma real_sigma ]
          | h :: tl ->
	    
            let var = fst (fst h) in
            let sym = snd (fst h) in

            let sum = sub_term sigma (snd h) in
            let sum = sub_term real_sigma sum in
            let sym = sub_term real_sigma sym in
	    

            if acterm_eq s t then unifyAC tl sigma real_sigma
            else

              match sym, sum with

              | AC_var _, _ when is_special_var sym ->
		let sigma' = Si.add sym sum sigma in
		unifyAC tl sigma' real_sigma

              | _, AC_var _ when is_special_var sum ->
		let sigma' = Si.add sum sym sigma in
		unifyAC tl sigma' real_sigma

              | _ , AC_app2 _ when (notonlyVi_in sum) ->
		let new_sigma  = put_voidAC sum (sigma) in
		let new_sum = sub_term new_sigma sum in
		List.fold_left
                  (fun x a -> x @ (unifyAC tl new_sigma a) ) [] (unify sym new_sum real_sigma)

              | _ , AC_app2 _ ->
		let permut = permut_list ((var,sym), sum) sigma in
		let permut = List.filter (fun ((k,v),si') -> acterm_eq sym (sub_term si' sum)) permut in
		List.fold_left
                  (fun some x -> (unifyAC tl (let (_,si) = x in si) real_sigma) @ some)  [] permut

              | (AC_var _, _ | _, AC_var _ | AC_app _, _ | _, AC_app _) ->
		List.fold_left (fun x a -> x @ (unifyAC tl sigma a) ) [] (unify sym sum real_sigma)

	      | _ -> []

	in


	let t = purify s t in
	let ac1 = fst (snd t) in
	let ac2 = snd (snd t) in
	match ac1, ac2 with
	| (AC_app2 (_, Multiset []), _ | _, AC_app2 (_, Multiset []) ) -> []

	| AC_app2 _, AC_app2 _ ->
          begin
            let si1 = fst t in
            let si2 = purifyac_to_assocvar ac1 ac2 in
            let list = getSymb si1 si2 in
            let dif = diff_si si2 si1 in
            let list = list @ (getVar dif) in
            List.map (fun x -> sub_si x x) (unifyAC list (Si.empty) si)
          end
	| _ ->
          let si1 = fst t in
          let ac1 = sub_term si1 ac1 in
          let ac2 = sub_term si1 ac2 in
          unify ac1 ac2 si
      end
	
    | _ -> []

  end
	
;;


(* renvoie un unificateur si s t sont unifiable, une exception sinon *)
exception CantUnify;;
let get_unificateur s t =
  let u = unify s t (Si.empty) in
  match u with
  | [] -> None
  | h :: _ -> Some (Si.bindings (sub_si h h));;
