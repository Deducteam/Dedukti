open Basics;;
open Multi_set;;
open Diophantienne;;
open Term;;
open Acterm;;

(** UNIF ACU --> travail preliminaire **)
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
let assocvar ac1 ac2 r n = match ac1, ac2 with
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
                let map' = L.update var (Elem(m2, mk_AC_varspe (hstring ((string_of_int n)^"v")) i)) map in
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
let purifyac_to_assocvar ac1 ac2 n =
  let r = solve_dioph ac1 ac2 in
  assocvar ac1 ac2 r n

let getSymb si1 si2 =
  let aux si e = let (k, v) = e in
    ( (k, v) , Si.find k si )
  in
  List.map (aux si2) (Si.bindings si1);;
let getVar si =
  List.map (fun (k,v) -> ((k,k),v)) (Si.bindings si);;
(** **)





type state_ac = {
  list: (acterm*acterm) list;
  subst: Si.key Si.t option;
  sigma: Si.key Si.t;
  next: state_ac list;
};;

let global_a = ref 0;;

let init_state l = 
  {list=l;subst=Some(Si.empty);sigma=Si.empty;next=[]};;

let get_subst st = match st with
  |{list;subst=Some s;sigma;next} -> Some s
  | _ -> None;;

let rec state_of_list l s n l' = match l' with
  | [] -> failwith "error"
  | e::[] -> {list=l;subst=s;sigma=e;next=n}
  | e::re -> {list=l;subst=s;sigma=e;next=(state_of_list l s n re)::[]};;


let rec unif_list s t = match s, t with
  | AC_app(s', ls),AC_app(t', lt) ->
    unif_list s' t' @ aux ls lt
  | AC_app2(s',_), AC_app2(t',_) when acterm_eq s' t' ->
    let pure = purify s t in
    let ac1 = fst (snd pure) in
    let ac2 = snd (snd pure) in
    begin match ac1,ac2  with
    | AC_app2 (s', Multiset []),AC_app2 (t', Multiset []) -> [ (s',t') ]
    | (AC_app2 (_, Multiset []), _ | _, AC_app2 (_, Multiset []) ) -> 
      failwith "err"
    | AC_app2 (s',_), AC_app2 (t',_) ->
      let si1 = fst pure in
      global_a := !global_a + 1;
      let si2 = purifyac_to_assocvar ac1 ac2 (!global_a) in
      let list = getSymb si1 si2 in
      let dif = diff_si si2 si1 in
      (List.map (fun ((s,y),z) -> (y,z)) (list @ (getVar dif)))
    |  _ ->
      let si1 = fst pure in
      let ac1 = sub_term si1 ac1 in
      let ac2 = sub_term si1 ac2 in
      [ (ac1,ac2) ]
    end
  | _,_ as r -> [ r ]

and aux l l' = match l, l' with
  | [],[] -> []
  | (_,[]|[],_) -> failwith "Err"
  | s'::rs,t'::rt ->
    unif_list s' t' @ aux rs rt;;


(* Convention :
     1. Le terme de gauche est soit une constante soit une variable soit un term acu
     2. Le terme de droite ne peut pas etre une variable a part si c'est une variable special 
*)

let rec unify st = match st with
  | {list;subst=None;sigma;next=[]} -> st
  | {list;subst=None;sigma;next=n::_} -> 
    unify n
  | {list=[];subst;sigma;next} -> st
  | {list=(s0,t0)::tl;subst=Some u;sigma=si;next=n} ->

    let s = sub_term u s0 in
    let t1 = sub_term si t0 in
    let t = sub_term u t1 in

    (*
      print_endline ((string_of_acterm s)^" = "^(string_of_acterm t)^" -->"^(string_of_si u));
    *)

    match s, t with
      
    (** CAS I : ACU **)

    | _, AC_var _ when is_special_var t ->
      unify {list=tl;subst=Some u;sigma=(Si.add t s si);next=n}
    | _,AC_app2 (_, Multiset []) ->
      unify {list=(s,t)::tl;subst=None;sigma=si;next=n}
    | _,AC_app2 (_, Multiset (a::_)) when only_special_var t ->
      let permut = permut_list ((mk_AC_var dloc (hstring "") 0,s),t) si in
      let permut' = List.map (fun ((x,y),z) -> z) permut in
      let permut'' = List.filter (fun x -> acterm_eq s (sub_term x t)) permut' in
      if permut'' = [] then 
	unify {list=tl;subst=None;sigma=si;next=n}
      else
	let new_st = state_of_list ((s,t)::tl) (Some u) n permut' in
	unify new_st
    | _,AC_app2 _ when exist_special_var t ->
      let new_si  = put_voidAC t si in
      unify {list=(s,t)::tl;subst=Some u;sigma=new_si;next=n}
    | AC_app2(s',_),AC_app2(t',_) when acterm_eq s' t' ->
      begin
	try
	  let l = unif_list s t in
	  unify {list=l@tl;subst=Some u;sigma=si;next=n}
	with _ -> 
	  unify {list=tl;subst=None;sigma=si;next=n}
      end

    (** **)


    (** CAS II : NON-ACU **)

    | AC_const(_,_,i),AC_const(_,_,i') when ident_eq i i' ->
      unify {list=tl;subst=Some u;sigma=si;next=n}
    | AC_const(_,_,_i),AC_const(_,_,i') ->
      unify {list=tl;subst=None;sigma=si;next=n}
    | AC_var(_,_,_,i),AC_var(_,_,_,i') when ident_eq i i' ->
      unify {list=tl;subst=Some u;sigma=si;next=n}
    | AC_app(s',ls),AC_app(t',lt) ->
      begin
	try
	  let l = unif_list s t in
	  unify {list=l@tl;subst=Some u;sigma=si;next=n}
	with _ -> 
	  unify {list=tl;subst=None;sigma=si;next=n}
      end
    | AC_var _,_ when is_occurs s t ->
      unify {list=tl;subst=None;sigma=si;next=n}
    | AC_var _,_ ->
      let new_u = Si.add s t u in
      unify {list=tl;subst=Some (sub_si new_u new_u);sigma=si;next=n}
    | _, AC_var _ -> 
      unify {list=(t,s)::tl;subst=Some u;sigma=si;next=n}

    (** **)

    (* Err 2 *)
    | _,_ ->       
      unify {list=(s,t)::tl;subst=None;sigma=si;next=n}
;;
