open Types

(* A really simple form of Miller Pattern Matching.
* We only try to match patterns of the form F( (DB k_0) ... (DB k_n) )
* against arbitrary terms. *)

type pb = (*te*) term * (* (k_i) *) int list
 (* Matching (modulo beta) problem in F: te ~? F( (DB k_0) ... (DB k_n) ) *)
exception NotUnifiable

let rec lam (te:term) : term list -> term = function
  | [] -> te
  | ty::lst -> lam (mk_Lam dloc qmark ty te) lst

let is_closed te =
    let rec aux k = function
      | Kind | Type _ | Const _ -> true
      | DB (_,_,n) -> (n<k)
      | App (f,a,args) -> (aux k f) && (aux k a) && (List.for_all (aux k) args)
      | Lam (_,_,a,b) | Pi (_,_,a,b) -> (aux k a) && (aux k b)
    in aux 0 te

(* the 'flexrigid' equation 'te ~? F(dbs)' has a solution iff
 * FIXME a revoir/documenter
 *)

let get_pos (n:int) (dbs:int list) : int option =
  let rec aux p = function
    | [] -> None
    | m::_ when n=m -> Some p
    | _::lst -> aux (p+1) lst
  in
    aux 0 dbs

let update_types (ltyp:term list) (dbs:int list) : term list =
    List.mapi (
      fun i db ->
        let ty = List.nth ltyp db in
          assert( is_closed ty ) ; (*FIXME*) 
          ty
    ) dbs

let resolve (ltyp:term list) ((te,dbs):pb) : term =
  let size_minus_one = (List.length dbs) - 1 in
  let rec aux k = function
    | Kind | Type _ | Const _ as t -> t
    | App (f,a,args) -> mk_App (aux k f) (aux k a) (List.map (aux k) args)
    | Lam (l,x,a,b) -> mk_Lam l x (aux k a) (aux (k+1) b)
    | Pi (l,x,a,b) -> mk_Pi l x (aux k a) (aux (k+1) b)
    | DB (l,x,n) as db ->
        if n<k then db else 
          ( match get_pos (n-k) dbs with
              | None -> raise NotUnifiable
              | Some p -> mk_DB l x (size_minus_one - p + k) )
  in
  let tys = update_types ltyp dbs in
  let s = lam (aux 0 te) tys in
    if is_closed s then s else raise NotUnifiable

let resolve_lst (ltyp:term list) (lst:pb LList.t) : term LList.t option =
  try Some (LList.map (resolve ltyp) lst)
  with NotUnifiable -> None
