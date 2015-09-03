(* definition des types *)
type 'a elem = Elem of int * 'a;;
type 'a ms = Multiset of 'a elem list;;

(* renvoie la taille d'un multiset *)
let len_Multiset ms = match ms with
    Multiset l -> List.length l;;

(* compare deux multiset *)
let cmpr_Multiset cmpr ms1 ms2 =
  let rec aux cmpr l1 l2 = match l1, l2 with
    | [], [] -> 0
    | _, [] -> 1
    | [], _ -> -1
    | h1::tl1, h2::tl2 ->
      match h1, h2 with
	Elem(m, v), Elem(m', v') ->
	  let c = cmpr v v' in
	  if c <> 0 then c
	  else
	    if m < m' then -1
	    else if m > m' then 1
	    else aux cmpr tl1 tl2
  in
  match ms1, ms2 with
    Multiset l1, Multiset l2 -> aux cmpr l1 l2;;

(* renvoie ms1 sans les element appartenant a ms2 *)
let diff cmpr ms1 ms2 =
  let rec aux cmpr l1 l2 = match l1, l2 with
    | [], [] -> []
    | [], l2' -> []
    | l1', [] -> l1'
    | h1::tl1, h2::tl2 ->
      match h1, h2 with
	Elem( m1, v1), Elem( m2, v2) ->
        if cmpr v1 v2 = -1 then h1 :: aux cmpr tl1 (h2::tl2)
        else if cmpr v1 v2 = 1 then aux cmpr (h1::tl1) tl2
        else if m1 <= m2 then aux cmpr tl1 tl2
        else Elem(m1-m2, v1) :: aux cmpr tl1 tl2
  in
  match ms1, ms2 with
    Multiset l1, Multiset l2 ->
    Multiset (aux cmpr l1 l2);;

(* renvoie deux multiset tel qu'il n'ont aucun element commun *)
let del_same cmpr ms1 ms2 =
  (diff cmpr ms1 ms2, diff cmpr ms2 ms1);;

(* fusionne de multiset *)
let fusion cmpr ms1 ms2 =
  let rec aux cmpr l1 l2 = match l1, l2 with
    | [], [] -> []
    | [], l2' -> l2'
    | l1', [] -> l1'
    | h1::tl1, h2::tl2 ->
      match h1, h2 with
	Elem( m1, v1), Elem( m2, v2) ->
	if cmpr v1 v2 = -1 then h1 :: aux cmpr tl1 (h2::tl2)
	else if cmpr v1 v2 = 1 then h2 :: aux cmpr (h1::tl1) tl2
	else Elem(m1 + m2, v1) :: aux cmpr tl1 tl2
  in
  match ms1, ms2 with
    Multiset l1, Multiset l2 ->
      Multiset (aux cmpr l1 l2);;

(* scinde une liste *)
let scinde l =
  let rec scinde' l r n = match l, n with
    | _, 0 -> r, l
    | h::tl, _ -> scinde' tl (r @ [h]) (n-1)
    | _ -> failwith "error"
  in
  match l with
    Multiset v ->
      let r = (scinde' v [] ((List.length v)/2)) in
      Multiset (fst r), Multiset (snd r)


(* rearrange le multiensemble par multipliciter *)
let rec fusion_rec cmpr l =
  let r = scinde l in
  let e1 = fst r in
  let e2 = snd r in
  let le1 = len_Multiset e1 in
  let le2 = len_Multiset e2 in
  if (le1 > 1) || (le2 > 1) then
    fusion cmpr (fusion_rec cmpr e1) (fusion_rec cmpr e2)
  else fusion cmpr e1 e2;;

(* Construit un multiset a partir d'une liste d'element *)
let mk_Multiset cmpr l =
  let rec aux l = match l with
    | [] -> []
    | h :: tl -> Elem(1,h) :: (aux tl)
  in
  fusion_rec cmpr (Multiset(aux l));;

(* retourne une liste d'element a partir d'un multiset *)
let list_of_multiset ms =
  let (Multiset l) = ms in
  let rec multiply e = match e with
    | Elem(0, _) -> []
    | Elem(m, v) -> v :: multiply (Elem(m-1, v))
  in
  List.flatten (List.map multiply l);;
