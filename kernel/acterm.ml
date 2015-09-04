open Multi_set;;
open Basics;;
open Term;;
open Rule;;


(* definiton d'un terme *)
type acterm =
| AC_var of loc*ident*int*ident
| AC_const of loc*ident*ident
| AC_app of acterm*acterm list
| AC_app2 of acterm*acterm ms
| AC_lam of loc*ident*acterm option*acterm
| AC_pi of loc*ident*acterm*acterm
| AC_kind
| AC_type of loc;;


(* egalite de deux acterm *)
let rec acterm_eq s t = match s, t with
  | AC_kind, AC_kind | AC_type _, AC_type _ -> true
  | (AC_var(_,_,_,i), AC_var(_,_,_,i') | AC_const(_,_,i), AC_const(_,_,i')) -> ident_eq i i'
  | AC_app(s',ls), AC_app(t', lt) -> 
    begin
      try (acterm_eq s' t') && List.for_all2 acterm_eq ls lt with _ -> false
    end
  | AC_app2(s', Multiset ls), AC_app2(t',Multiset lt) ->
    begin
      let aux = 
	fun x y -> 
	  match x, y with 
	  | Elem(m1, e1), Elem(m2, e2) when m1 = m2 ->
	    acterm_eq e1 e2
	  | _ -> false
      in
      try (acterm_eq s' t') && List.for_all2 aux ls lt with _ -> false
    end 
  | AC_lam (_,_,os,s'), AC_lam(_,_,ot,t') -> 
    let v = match os, ot with
      | None, None -> true
      | _, None -> false
      | None, _ -> false
      | _ -> true
    in v && acterm_eq s' t'
  | AC_pi(_,_,s1,s2), AC_pi(_,_,t1,t2) ->
    acterm_eq s1 t1 && acterm_eq s2 t2
  | _ -> false;;


(* comparaison de deux acterm *)
let rec acterm_cmpr s t = match s, t with
  | AC_kind, AC_kind | AC_type _, AC_type _ -> 0
  | AC_var(_,_,_,i), AC_var(_,_,_,i') | AC_const(_,_,i), AC_const(_,_,i') ->
    Pervasives.compare (string_of_ident i) (string_of_ident i')
  | AC_app(s', ls), AC_app(t',lt) ->
    let cmp = acterm_cmpr s' t' in
    if cmp = 0 then lexico_rec ls lt
    else cmp
  | AC_app2(s',ls), AC_app2(t',lt) ->
    let cmp = acterm_cmpr s' t' in
    if cmp = 0 then cmpr_Multiset (acterm_cmpr) ls lt
    else cmp
  | AC_lam (_,_,os,s'), AC_lam(_,_,ot,t') -> 
    let v = match os, ot with
      | None, None -> 0
      | _, None -> 1
      | None, _ -> -1
      | _ -> 0
    in if v = 0 then acterm_cmpr s' t'
      else v
  | AC_pi(_,_,s1,s2), AC_pi(_,_,t1,t2) ->
    let cmp = acterm_cmpr s1 t1 in
    if cmp = 0 then acterm_cmpr s2 t2
    else cmp
  | AC_var _, _ -> -1
  | AC_const _, AC_var _ -> 1
  | AC_const _, _ -> -1
  | AC_app _, AC_var _ | AC_app _, AC_const _ -> 1
  | AC_app _, _ -> -1
  | AC_app2 _, AC_var _ | AC_app2 _, AC_const _ | AC_app2 _, AC_app _ -> 1
  | AC_app2 _, _ -> -1
  | AC_lam _, AC_var _| AC_lam _, AC_const _| AC_lam _, AC_app _ | AC_lam _, AC_app2 _ -> 1
  | AC_lam _, _ -> -1
  | AC_pi _, AC_var _ | AC_pi _, AC_const _ | AC_pi _, AC_app _ | AC_pi _, AC_app2 _ | AC_pi _, AC_lam _ -> 1
  | AC_pi _, _ -> -1
  | AC_type _, AC_var _| AC_type _, AC_const _ | AC_type _, AC_app _| AC_type _, AC_app2 _| AC_type _, AC_lam _ | AC_type _, AC_pi _ -> 1
  | AC_type _, _ -> -1
  | AC_kind , _ -> 1
 
and lexico_rec ls lt = match ls, lt with
  | [], [] -> 0
  | [], _ -> -1
  | _, [] -> 1
  | h1::tl1, h2::tl2 -> 
    let cmp = acterm_cmpr h1 h2 in
    if cmp = 0 then lexico_rec tl1 tl2
    else cmp;;
    

(* affiche un acterm *)
let rec string_of_acterm t =
  let rec string_of_args args = match args with
    | [] -> ""
    | h :: [] -> string_of_acterm h
    | h :: tl -> (string_of_acterm h) ^ ", " ^  string_of_args tl
  in
  let rec string_of_argsAC args ac = match args with
    | [] -> ""
    | Elem(1, h) :: [] -> string_of_acterm h
    | Elem(1, h) :: tl -> 
      (string_of_acterm h) ^ " " ^ (string_of_acterm ac) ^ " " ^ (string_of_argsAC tl ac)
    | Elem(m, h) :: [] -> 
      (string_of_int m) ^ "." ^ string_of_acterm h
    | Elem(m, h) :: tl -> 
      (string_of_int m) ^ "." ^ (string_of_acterm h) ^ " " ^ 
	(string_of_acterm ac) ^ " " ^ (string_of_argsAC tl ac)
  in
  match t with
  | AC_var(_,_,_,i) | AC_const(_,_,i) -> string_of_ident i
  | AC_app (s, ls) -> (string_of_acterm s) ^ "(" ^ (string_of_args ls) ^ ")"
  | AC_app2(s, Multiset ms) -> "(" ^ (string_of_argsAC ms s) ^ ")"
  | AC_lam (_,i,so,s) -> 
    (string_of_ident i) ^ (match so with None -> "" | Some s' -> ":" ^ (string_of_acterm s')) ^
      " => " ^ (string_of_acterm s)
  | AC_pi (_,_,a,b) -> (string_of_acterm a) ^ " -> " ^ (string_of_acterm b)
  | _ -> "";;


(* construit une variable *)
let mk_AC_var l id i = 
  AC_var(l,id,i,hstring ((string_of_ident id)^(string_of_int i)));;

(* construit une variable speciale *)
let mk_AC_varspe s i =
  AC_var(dloc,(hstring ""), -1, hstring ("_" ^ (string_of_ident s) ^ (string_of_int i)));;

(* construit une constante *)
let mk_AC_const l i i' =
  AC_const(l, i, i');;

(* construit une app *)
let mk_AC_app t lt = match lt with
  | [] -> t
  | _ -> AC_app (t, lt);;

(* construit une app acu *)
let mk_AC_app2 t lt =
  let rec flatten_t lt = match lt with
    | [] -> []
    | h::tl -> 
      match h with
      | AC_app2(t', Multiset []) -> flatten_t tl
      | AC_app2(t',tl') when acterm_eq t t' ->
	flatten_t (list_of_multiset tl') @ flatten_t tl
      | _ -> h :: flatten_t tl
  in match flatten_t lt with
  | e::[] -> e
  | l -> AC_app2(t, mk_Multiset acterm_cmpr (flatten_t l));;

(* construit un lambda *)
let mk_AC_lam l i os s =
  AC_lam(l,i,os,s);;

(* construit un pi *)
let mk_AC_pi l i a b =
  AC_pi(l,i,a,b);;

(* test si une constante est acu *)
let is_acu t = match t with
  | Const (_,_,i) when ident_eq i (hstring "add") -> true
  | _ -> false;;

(* transforme un term en acterm *)
let rec acterm_of_term t = match t with
  | DB (l,id,i) -> mk_AC_var l id i
  | Const (l,i,i') -> mk_AC_const l i i'
  | App (s,a,ls) when is_acu s ->
    mk_AC_app2 (acterm_of_term s) (List.map acterm_of_term (a::ls))
  | App (s,a,ls) ->
    mk_AC_app (acterm_of_term s) (List.map acterm_of_term (a::ls))
  | Lam (l,i,os,s) -> 
    let os' = 
      match os with
      | None -> None
      | Some s' -> Some (acterm_of_term s')
    in
    mk_AC_lam l i os' (acterm_of_term s)
  | Pi (l,i,a,b) -> mk_AC_pi l i (acterm_of_term a) (acterm_of_term b)
  | Type l -> AC_type l
  | Kind -> AC_kind;;

(* transforme un pattern en acterm *)
let rec acterm_of_pattern p = match p with
  | Var (l,id,i,_) -> mk_AC_var l id i
  | Pattern (l, i, i', args) when ident_eq i' (hstring "add") ->
    mk_AC_app2 (mk_AC_const l i i') (List.map acterm_of_pattern args)
  | Pattern (l,i,i',args) ->
    mk_AC_app (mk_AC_const l i i') (List.map acterm_of_pattern args)
  | _ -> assert false;;

(* transforme un acterm en term *)
let rec term_of_acterm act = match act with
  | AC_var (l,id,i,i') -> mk_DB l id i
  | AC_const (l,i,i') -> mk_Const l i i'
  | AC_app (t, lt) -> 
    mk_App (term_of_acterm t) (term_of_acterm (List.hd lt)) (List.map term_of_acterm (List.tl lt))
  | AC_app2 (t, mt) -> 
    let l = list_of_multiset mt in
    let lg, ld = cut l [] ((List.length l) / 2) in 
    let new_t = term_of_acterm t in
    mk_App new_t (app_of_acapp2 new_t lg) [ app_of_acapp2 new_t ld ]
  | AC_lam (l,i,ot,t) ->
    let ot' = match ot with
      | Some s -> Some (term_of_acterm s)
      | _ -> None
    in mk_Lam l i ot' (term_of_acterm t)
  | AC_pi (l,i,a,b) -> mk_Pi l i (term_of_acterm a) (term_of_acterm b)
  | AC_kind -> mk_Kind
  | AC_type l -> mk_Type l

and app_of_acapp2 t lt = match lt with
  | [] -> assert false
  | a::[] -> term_of_acterm a
  | a::[ b ] -> mk_App t (term_of_acterm a) [ term_of_acterm b ]
  | _ -> 
    let lg, ld = cut lt [] ((List.length lt) /2) in
    mk_App t (app_of_acapp2 t lg) [ app_of_acapp2 t ld ]

and cut lg ld i = match lg,i with
  | [],_ -> lg, ld
  | _, 0 -> lg, ld
  | h::tl, _ -> cut tl (h::ld) (i-1);;


(* test si une variable v est contenue dans un terme t *)
let rec is_occurs v t = match v, t with
  
  | AC_var _, AC_var _ -> acterm_eq v t
  | AC_var _, AC_app(s, ls) -> List.exists (is_occurs v) ls
  | AC_var _, AC_app2 (s, Multiset ls) -> 
    List.exists (fun x -> let (Elem(m, t')) = x  in (is_occurs v t') ) ls
  | AC_var _, AC_lam (_,_,_,s) -> is_occurs v s
  | _ -> false;;


(**** Definition d'une subsitution ****)

(* module *)
module AssocMap =
  struct
    type t = acterm
    let compare = acterm_cmpr
  end;;
module Si = Map.Make (AssocMap);;

(* effectue une substitution *)
let sub_term si term =
  let sub si var =
    try
      Si.find var si 
    with Not_found -> var
  in
  let rec sub_term' si term = match term with
    | AC_var _ -> sub si term
    | AC_app(s, ls) -> AC_app (s, List.map (sub_term' si) ls)
    | AC_app2(s, ms) ->
      let l = list_of_multiset ms in
      let ltmp2 = List.map (sub_term' si) l  in
      mk_AC_app2 s ltmp2
    | AC_lam(l,i,os,s) -> AC_lam(l,i,os, sub_term' si s) 
    | AC_pi(l,i,a,b) -> AC_pi(l,i,sub_term' si a, sub_term' si b)
    | _ -> term
  in
  sub_term' si term;;


let sub_si si1 si2 =
  List.fold_left (fun si (k,v) -> Si.add k v si) (Si.empty) (List.map (fun (k,v) -> (k, sub_term si1 v)) (Si.bindings si2));;

(* retourne la chaine de caractere d'une substitution *)
let string_of_si si =
  let string_of_e e = let (k, v) = e in (string_of_acterm k) ^ " <- " ^ (string_of_acterm v) in
  let rec string_of_si' l = match l with
    | [] -> ""
    | h::[] -> string_of_e h
    | h :: tl -> (string_of_e h) ^ " ; " ^ (string_of_si' tl)
  in
  "{ " ^ (string_of_si' (Si.bindings si)) ^ " }";;

(* fusionne deux substitutions *)
let fusion_si si1 si2 =
  List.fold_left (fun si (k,v) -> Si.add k v si) si1 (Si.bindings si2);;

(* renvoi la substitution si1 sans les clef appartenant a si2 *) 
let diff_si si1 si2 =
  let l = List.filter (fun (k,v) -> match (try Some(Si.find k si2) with _ -> None) with None -> true | _ -> false) (Si.bindings si1) in
  List.fold_left (fun si (k,v) -> Si.add k v si) (Si.empty) l;;

(* test si t1 et t2 sont egaux a une substitution pres *) 
let eq2 si t1 t2 = acterm_eq (sub_term si t1) (sub_term si t2);;

(* test si une variable est special *)
let is_special_var v = match v with
  | AC_var(_,_,_,i) when (string_of_ident i).[0] = '_' -> true
  | _ -> false;;
    

(* Renvoie une substitution avec toutes les variable special associe a +[] *)
let put_voidAC ac si = match ac with
      | AC_app2 (s, Multiset ls) ->
        let rec put_voidAC' l si s = match l with
          | [] -> si
          | Elem(m, v) :: tl when is_special_var v ->
            put_voidAC' tl (Si.add v (mk_AC_app2 s []) si) s
          | _ :: tl -> put_voidAC' tl si s
        in
        put_voidAC' ls si s
      | _ -> failwith "Error : put_voidAC";;

(* Renvoie une liste avec toutes les subsitutions de permutation *)
let permut_list ac si = match ac with
  | ( (var, cons), AC_app2 (s, Multiset l) ) ->
    let rec algo1 var cons hd0 l si = match l with
      | [] -> []
      | hd :: tl ->
        let aux1 x = let Elem(m, v) = x in (v, mk_AC_app2 s []) in
        let hd2 = let Elem(m, v) = hd in (v, cons) in
        let tl2 = List.map aux1 tl in
        let r = hd0 @ [hd2] @ tl2 in
        let si' = List.fold_right (fun (x,y) -> Si.add x y) r (si) in
        let value = ( (var, cons) , (si') ) in
        value :: algo1 var cons (hd0 @ [(fst hd2, mk_AC_app2 s [])]) tl si
    in
    algo1 var cons [] l si 
  | _ -> failwith "Error : permut_list";;

(* test si ac contient au moin un term qui n'est pas une variable speciale *)
let notonlyVi_in ac = match ac with
      | AC_app2(s, Multiset l) ->
        List.exists (fun x -> match x with | Elem(_, s) when is_special_var s -> false | _ -> true) l
      | _ -> failwith "Error :  notonlyVi_in";;



(**** Purification de deux termes AC *****)

exception CantPurify;;
let purify ac1 ac2 = match ac1, ac2 with
  | AC_app2(s1, ms1), AC_app2(s2, ms2) when acterm_eq s1 s2 ->
    let var_auto s i = mk_AC_varspe s i in
    let rec aux l lres si i = match l with
      | [] -> si, lres
      | h :: tl ->
	let (Elem(m, t)) = h in
        match t with
        | AC_var _  ->
	  aux tl (lres @ [Elem(m, t)]) si i
        | _ ->
          let new_key = var_auto (hstring "_var__") i in
          let si = Si.add new_key t si in
          aux tl (lres @ [Elem(m, new_key)]) si (i+1)
    in
    let rmv = del_same acterm_cmpr ms1 ms2 in
    let l1 = let (Multiset l) = fst rmv in l in
    let l2 = let (Multiset l) = snd rmv in l in
    let r1 = aux l1 [] (Si.empty) 1 in
    let si = fst r1 in
    let lres1 = snd r1 in
    let r2 = aux l2 [] si ( (List.length l1)+1 ) in
    let si = fst r2 in
    let lres2 = snd r2 in
    si, (mk_AC_app2 s1 (list_of_multiset (Multiset lres1)), (mk_AC_app2 s2 (list_of_multiset (Multiset lres2))))
  | _ -> raise CantPurify;;
