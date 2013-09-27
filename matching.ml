
open Types

type union =
  | PMat of pMat
  | Term of term

let to_pattern2 (env:string list) (pat:pattern) =
  let rec aux = function
    | Pat ((_,m,v),ds,ps)       ->
        let s = Array.length ds + Array.length ps in
          if s = 0 then
            if m= (!Global.name) && List.mem v env then Var v
            else Pattern ((m,v),[||])
          else
            let d = Array.length ds in
            let arr = Array.init s ( fun i -> if i<d then Joker else aux ps.(i-d) ) in
              Pattern ((m,v),arr)
  in aux pat

let pMat_init (lst:rule2 list) = 
  let (head,size) = match lst with
    | (_,((_,v),ds,ps),_)::_    -> ( v , Array.length ds + Array.length ps )
    | _                         -> assert false
  in
  let mk_line (names,((_,c),ds,ps),t) : line = 
    if c != head then 
      raise (PatternError ("Constructor mismatch in rewrite rule:\nExpected: "^head^"\nFound: "^c^"."))
    else if not ( Array.length ds+Array.length ps = size ) then 
      raise (PatternError ("Arity mismatch in rewrite rule for constant '"^c^"'."))
    else
      let d = Array.length ds in
      let arr = Array.init size ( fun i -> if i<d then Joker else to_pattern2 names ps.(i-d) ) in
        { li=arr ; te=t; na=names; }
  in 
    Array.of_list (List.map mk_line lst)  

(*
* On elargie la matrice de (arity-1)
 * On supprime la colonne c
 * On ajoute à la fin les arity colonnes
* *)
let specialize (pm:pMat) (c:int) (arity:int) (lines:int list) : union = 
  assert (Array.length pm > 0);
  let cols  = Array.length pm.(0).li in
  let cols2 = cols + arity - 1 in
    if  cols2 = 0 then (
      match lines with 
        | []      -> assert false 
        | x::_    -> ( assert (x < Array.length pm) ; Term (pm.(x).te) ) 
    ) else
      let p2 = List.map (
        fun li -> 
          assert (li < Array.length pm) ;
          let line = pm.(li).li in
            assert ( c < Array.length line ) ;
            assert ( Array.length line = cols ) ;
            { li = Array.init cols2 (
              fun i -> 
                if i<c then line.(i)
                else if i<(cols-1) then line.(i+1)
                else 
                  match line.(c) with
                    | Pattern (_,pats)  -> ( assert ( i-cols+1 < Array.length pats) ; pats.(i-cols+1) )
                    | _                 -> assert false
            ) ;
              te = pm.(li).te ; na= pm.(li).na ; }
      ) lines in 
        PMat ( Array.of_list p2 ) 

(* on ne garde que les lignes ou la colonne c est une Var/Joker *)                                                                          
let default (pm:pMat) (c:int) : pMat option = 
  let lst =
    Array.fold_left  (
      fun lst t -> 
        assert ( c < Array.length t.li ) ;
        match t.li.(c) with
          | Pattern (_,_)       -> lst
          | _                   -> t::lst
    ) [] pm in
    match lst with
      | [] -> None
      | _   -> Some ( Array.of_list lst )
 
let partition (pm:pMat) (c:int) : ((string*string)*int*int list) list  = 
  let hs  = Hashtbl.create 47 in
    Array.iteri (
      fun i t ->
        assert ( c < Array.length t.li ) ;
        match t.li.(c) with
          | Pattern ((m,v),pats)      -> 
              begin 
                try 
                  let (arity,lst) = Hashtbl.find hs (m,v) in
                    Hashtbl.replace hs (m,v) (arity,i::lst)
                with Not_found -> Hashtbl.add hs (m,v) (Array.length pats,[i])
              end
          | _                             -> () 
    ) pm ;
    Hashtbl.fold (fun id (ar,ll) lst -> (id,ar,List.rev ll)::lst ) hs []

let getColumn (pm:pMat) =
  assert ( Array.length pm > 0 ) ;
  let arr = pm.(0).li in
  let rec aux i = 
    if i < Array.length arr then
      match arr.(i) with
        | Pattern (_,_)   -> Some i
        | _             -> aux (i+1)
          else None
  in aux 0

let rec reorder ord k = function
  | DB n when n<k       -> DB n
  | DB n (* n>=k *)     -> ( assert (n-k < Array.length ord) ; DB (ord.(n-k) + k) )
  | App args            -> App (List.map (reorder ord k) args)
  | Lam (a,f)           -> Lam (reorder ord k a, reorder ord (k+1) f)
  | Pi  (a,b)           -> Pi  (reorder ord k a, reorder ord (k+1) b)
  | t                   -> t 

let get_term (l:line) = 
  let rec get_db v i =
    if (i<Array.length l.li) then
      ( match l.li.(i) with
        | Var v' when v=v'    -> i
        | Pattern (_,_)       -> assert false
        | _                   -> get_db v (i+1) )
    else
      raise (PatternError ("Variable '"^v^"' does not appear on the left side of the rewrite rule."))
  in
  let nam = Array.of_list l.na in
  let ord = Array.init (Array.length nam) (
      fun i -> get_db (nam.(i)) 0
    ) in
    reorder ord 0 l.te

let rec cc (pm:pMat) : gdt =
  match getColumn pm with
    (* La 1ere ligne ne contient que des Var/Jokers *)
    | None      -> Leaf ( get_term pm.(0) )
    (* Colonne c contient un pattern *)
    | Some c    ->
        let cases =
          List.map ( 
            fun (id,arity,lst) ->
              match specialize pm c arity lst with
                | Term te       -> ( id , Leaf te )
                | PMat pm'      -> ( id , cc pm' )
          ) (partition pm c) in
          ( match default pm c with
              | None           -> Switch (c,cases,None)
              | Some pm'       -> Switch (c,cases,Some (cc pm'))
          ) 

let get_rw v (rs:rule2 list) : int*gdt =
  let pm = pMat_init rs in
    assert ( Array.length pm > 0 ) ;
  let gdt = cc pm in
    (*Debug.dump_pMat v pm ;*)
    (*Debug.dump_gdt v gdt ;*)
    ( Array.length pm.(0).li , gdt )
