open Types

type pb = (*te*) term * (* (k_i) *) int list
 (* Matching (modulo beta) problem in F: te ~? F( (DB k_0) ... (DB k_n) ) *)
exception NotUnifiable

let occur (te:term) (x:int) : bool =
  let rec aux k = function
  | Kind | Type _ | Const _ -> false
  | DB (_,_,n) -> (n-k) = x
  | App (f,a,args) -> List.exists (aux k) (f::a::args)
  | Lam (_,_,a,b) | Pi (_,_,a,b) -> ( aux k a || aux (k+1) b )
  in
    aux 0 te

let rec lam (te:term) : term list -> term = function
  | [] -> te
  | ty::lst -> lam (mk_Lam dloc qmark ty te) lst

(* the 'flexrigid' equation 'te ~? F(dbs)' has a solution iff
 * FIXME
 *)

let resolve ltyp ((te,dbs):pb) : term =
  if List.for_all (occur te) dbs then lam te ltyp
  else raise NotUnifiable

let resolve_lst (ltyp:term list) (lst:pb LList.t) : term LList.t option =
  try Some (LList.map (resolve ltyp) lst)
  with NotUnifiable -> None
