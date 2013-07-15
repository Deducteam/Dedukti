
open Types

type union =
  | PMat of pMat
  | Term of term

(*
* On elargie la matrice de (arity-1)
 * On supprime la colonne c
 * On ajoute à la fin les arity colonnes
* *)
let specialize (pm:pMat) (c:int) (arity:int) (lines:int list) : union = 
  let cols  = Array.length (fst pm.(0)) in
  let cols2 = cols + arity - 1 in
  if  cols2 = 0 then
    Term (snd pm.(0))
  else
    let p2 = List.map (
      fun li -> 
        ( Array.init cols2 (
          fun i -> 
            let line = fst (pm.(li)) in
              if i<c then line.(i)
              else if i<(cols-1) then line.(i+1)
              else let j = i-cols+1 in
                match line.(c) with
                  | Pattern (_,pats)  -> pats.(j)
                  | _                   -> assert false
        ),
          snd pm.(li) )
      ) lines in 
      PMat ( Array.of_list p2 ) 

(* on ne garde que les lignes ou la colonne c est une Var/Joker *)                                                                          
let default (pm:pMat) (c:int) : pMat option = 
  let lst =
    Array.fold_left  (
      fun lst t -> 
        match (fst t).(c) with
          | Pattern (_,_)     -> lst
          | _                   -> t::lst
    ) [] pm in
    match lst with
      | [] -> None
      | _   -> Some ( Array.of_list lst )
 
let partition (pm:pMat) (c:int) : ((string*string)*int*int list) list  = 
  let hs  = Hashtbl.create 47 in
    Array.iteri (
      fun i (t,_) ->
        match t.(c) with
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
  let (arr,_) = pm.(0) in
  let rec aux i = 
    if i < Array.length arr then
      match arr.(i) with
        | Pattern (_,_)   -> Some i
        | _             -> aux (i+1)
          else None
  in aux 0

let to_list arr =
  let i = ref (-1) in
    List.fold_left ( 
      fun lst p ->
        match p with
          | Pattern (_,_)       -> assert false
          | Joker               -> incr i ; lst
          | Var v               -> incr i ; (v,!i)::lst
  ) [] (Array.to_list arr)

let rec cc (pm:pMat) : gdt =
  (*Global.debug "######### CallCC \n";Debug.print_pMat pm;*)
    match getColumn pm with
      (* La 1ere ligne ne contient que des Var/Jokers *)
      | None      -> 
          let (arr,te) = pm.(0) in
            Leaf ( to_list arr , te )
      (* Colonne c contient un pattern *)
      | Some c    ->
          let cases =
            List.map ( 
              fun (id,arity,lst) ->
                match specialize pm c arity lst with
                  | Term te       -> ( id , Leaf ([],te) )
                  | PMat pm'      -> ( id , cc pm' )
            ) (partition pm c) in
            ( match default pm c with
                | None           -> Switch (c,cases,None)
                | Some pm'       -> Switch (c,cases,Some (cc pm'))
            ) 

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
  let f (env,((_,c),ds,ps),te) : pattern2 array * term = 
    if ( c = head ) && ( Array.length ds+Array.length ps = size ) then
      let d = Array.length ds in
      let arr = Array.init size ( fun i -> if i<d then Joker else to_pattern2 env ps.(i-d) ) in
        ( arr , te )
        else
          failwith "pMat_init"
  in 
    Array.of_list (List.map f lst) 
(*
let interp (vars:(string*int) list) (args:code array) (te:term) = assert false (*TODO*)
 
let mk_new_arg c args1 lst =
  let s1 = Array.length args1 in
  let args2 = Array.of_list lst in
  let s2 = Array.length args2 in
  Array.init (s1+s2-1) (
    fun i ->
      if i<c then args1.(i)
      else if i<(s1-1) then args1.(i+1)
      else args2.(i-(s1-1))
  )

let rec fun_of_gdt = function
  | Leaf (vars,te)              -> ( fun args -> Some (interp vars args te) )
  | Switch (s,cases,def)        -> 
      ( fun arg ->
          match arg.(s) with
            | C_App ((m,v),lst) ->
                begin
                  try fun_of_gdt (List.assoc (m,v) cases) (mk_new_arg s arg lst) 
                  with Not_found -> 
                    ( match def with
                        | None          -> None
                        | Some d        -> fun_of_gdt d arg )
                end
            | _                 -> None
       )
 *)

let get_rw (rs:rule2 list) : int*gdt =
  let pm = pMat_init rs in
  let gdt = cc pm in
    ( Array.length (fst pm.(0)) , gdt )
