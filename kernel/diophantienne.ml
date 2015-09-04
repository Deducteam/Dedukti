(**** Definition d'une equation diophantienne ****)

(* les types *)
type coeff = Coefficient of int list;;
type equat = Equation of coeff * coeff;;

(* creer une equation a partir de deux listes de coefficient *)
let mk_Equat a b = Equation( Coefficient(a) , Coefficient(b) );;


  
(**** Definition d'un ensemble de vecteur ****)

(* definiton d'un module pour le representation de vecteur *)
module VectMod =
  struct    
    type t = Vect of int list * int list
    let compare = compare
  end;;

(* definition d'un ensemble de vecteur *)
module VectSet = Set.Make (VectMod);;

  

(**** Les fonction pour les vecteurs ****)

(* creer un vecteur de la base canonique *)
let rec canonique k n =
  if n = 0 then []
  else
    if k = n then ( canonique k (n-1) ) @ [1]
    else ( canonique k (n-1) ) @ [0];;

(* calcul la somme de deux vecteurs *)
let somme v1 v2 = match v1, v2 with
  | VectMod.Vect(l1, l2), VectMod.Vect(l1', l2') ->
     VectMod.Vect( List.map2 (+) l1 l1', List.map2 (+) l2 l2' )

(* calcul le default d'un vecteur a partir d'une equation *)
let d v equ =
  match v, equ with
  | VectMod.Vect(l1, l2), Equation( Coefficient(a), Coefficient(b) ) ->
     (List.fold_left (+) 0 (List.map2 (fun x y -> x * y) l1 a)) -
       (List.fold_left (+) 0 (List.map2 (fun x y -> x * y) l2 b))

(* construit le vecteur ( ei , 0 ) *)
let mk_Vect_g i equ = match equ with
    Equation( Coefficient(a), Coefficient(b) ) ->
    VectMod.Vect( canonique i (List.length a), canonique 0 (List.length b) );;

(* construit le vecteur ( 0 , ei ) *)
let mk_Vect_d i equ = match equ with
    Equation( Coefficient(a), Coefficient(b) ) ->
    VectMod.Vect( canonique 0 (List.length a), canonique i (List.length b) );;

(* test l'egalite de deux vecteurs *)
let rec eq_Vect v1 v2 = match v1, v2 with
  | VectMod.Vect(l1, l2), VectMod.Vect(l1', l2') ->
     (List.for_all2 (fun x y -> x = y) l1 l1') &&  (List.for_all2 (fun x y -> x = y) l2 l2')
  
(* test si un vecteur est plus grand qu'un autre *)
let rec plus_grand' l1 l2 = match l1, l2 with
  | [], [] -> true
  | h1::tl1 , h2::tl2 -> (h1 >= h2) && (plus_grand' tl1 tl2)
  | _ -> failwith "impossible" 
let plus_grand v1 v2 = match v1, v2 with
  | VectMod.Vect(l1, l2), VectMod.Vect(l1', l2') -> (plus_grand' l1 l1') && (plus_grand' l2 l2')
let not_plus_grand v1 v2 = not (plus_grand v1 v2);;
  
(* test si un vecteur est minimal dans un ensemble de vecteur *)
let is_minimal_in p um =
  VectSet.for_all (fun x -> not_plus_grand p x) um;;


  
(**** Calcul preliminaire pour la procedure ****)

(* calcul de l'ensemble Qk *)
let rec q1 p n equ = match n with
  | 0 -> []
  | _ -> (List.map (fun x -> somme x (mk_Vect_d n equ)) (List.filter (fun x -> d x equ > 0) p)) @ (q1 p (n-1) equ);;
let rec q2 p m equ = match m with
  | 0 -> []
  | _ -> (List.map (fun x -> somme x (mk_Vect_g m equ)) (List.filter (fun x -> d x equ < 0) p)) @ (q2 p (m-1) equ);;
let rec fq p equ = match equ with
    Equation( Coefficient(a), Coefficient(b) ) ->
    (q1 p (List.length b) equ) @ (q2 p (List.length a) equ);;
let ensemble_Qk p equ =
  let pl = VectSet.elements p in
  let r = fq pl equ in
  VectSet.of_list r;;

(* Calcul de l'ensemble Mk *)
let fm q equ = VectSet.filter (fun x -> d x equ = 0) q;;
let ensemble_Mk q equ = fm q equ;;  

(* Calcul de l'ensemble Pk *)
let fp q um = VectSet.filter (fun x -> is_minimal_in x um) q;;
let ensemble_Pk q um = fp q um;;


  
(**** Procedure ****)

(* inialise P1 *)
let init_P1 equ =
  let rec aux n equ = match n with
    | 0 -> []
    | _ ->  ( aux (n-1) equ) @ [mk_Vect_g n equ]
  in
  match equ with
    Equation( Coefficient(a), _) ->  VectSet.of_list (aux (List.length a) equ);;

(* one step *)
let step_kp1 pk um equ=
  let qkp1 = ensemble_Qk pk equ in
  let mkp1 = ensemble_Mk pk equ in
  let um = VectSet.union um mkp1 in
  let pkp1 = ensemble_Pk qkp1 um in
  pkp1, um;;

(* k step *)
let rec step p um equ =
  if p = VectSet.empty then um
  else match step_kp1 p um equ with (u, v) -> step u v equ;;

(* PROCEDURE *)
let procedure equ =
  let p1 = init_P1 equ in
  let um = VectSet.empty in
  step p1 um equ ;;


let equ = mk_Equat [1 ; 1 ; 1] [1; 1];;

VectSet.elements (procedure equ);;

