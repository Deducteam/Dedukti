open Types

exception NotUnifiable

let permute (dbs:int LList.t) (te:term) : term =
  let size = LList.len dbs in
  let rec find n cpt = function
    | [] -> raise NotUnifiable
    | q::lst -> if q=n then size-1-cpt else find n (cpt+1) lst
  in
  let rec aux k = function
    | Type _ | Kind | Const _ as t -> t
    | DB (_,x,n) as t ->
        if n < k then t
        else
          let n' = find (n-k) 0 (LList.lst dbs) in
            mk_DB dloc x (n'+k)
    | Lam (l,x,a,b) -> mk_Lam dloc x (aux k a) (aux (k+1) b)
    | Pi  (_,x,a,b) -> mk_Pi  dloc x (aux k a) (aux (k+1) b)
    | App (f,a,lst) -> mk_App (aux k f) (aux k a) (List.map (aux k) lst)
  in aux 0 te


(* Find F such that F (DB [k_0]) ... (DB [k_n]) =~ [te]
 * when the k_i are distinct *)
let resolve (k_lst:int LList.t) (te:term) : term =
  let ty = mk_Type dloc in (*FIXME*)
  let rec add_lam te = function
    | [] -> te
    | _::lst -> add_lam (mk_Lam dloc qmark ty te) lst
  in
    add_lam (permute k_lst te) (LList.lst k_lst)

                    (*
type pb = (*te*) term * (* (k_i) *) int list
 (* Matching (modulo beta) problem in F: te ~? F( (DB k_0) ... (DB k_n) ) *)

let rec lam (te:term) : term list -> term = function
  | [] -> te
  | ty::lst -> lam (mk_Lam dloc qmark ty te) lst

let is_closed te =
    let rec aux k = function
      | Kind | Type _ | Const _ -> true
      | DB (_,_,n) -> (n<k)
      | App (f,a,args) -> (aux k f) && (aux k a) && (List.for_all (aux k) args)
      | Lam (_,_,a,b) | Pi (_,_,a,b) -> (aux k a) && (aux (k+1) b)
    in aux 0 te

let get_pos (n:int) (dbs:int list) : int option =
  let rec aux p = function
    | [] -> None
    | m::_ when n=m -> Some p
    | _::lst -> aux (p+1) lst
  in
    aux 0 dbs

(* Re-arranges the context [ctx] with respect to [dbs].
 * Not necessary a permutation: the context may decrease in size
 * This is a DeBruijn hell *)
let permute_ctx (ctx:term list) (dbs:int list) : term list =
  let rec aux0 i db t =
    let rec aux k = function
      | Kind | Type _ | Const _ as te -> te
      | App (f,a,args) -> mk_App (aux k f) (aux k a) (List.map (aux k) args)
      | Lam (l,x,a,b) -> mk_Lam l x (aux k a) (aux (k+1) b)
      | Pi (l,x,a,b) -> mk_Pi l x (aux k a) (aux (k+1) b)
      | DB (l,x,n) as te ->
          if n < k then te
          else if (* k =< *) n < k+db then
            ( match get_pos (n-k) dbs with
                | None -> assert false (*TODO should not happen?*)
                | Some z -> ( assert ( z<i ) ; mk_DB l x (i-z) ) )
          else (* n >= k+db *)
            mk_DB l x (n-db+i)
    in aux 0 t
  in
  List.mapi ( fun i db -> aux0 i db (List.nth ctx db) ) dbs

(* Computes the solution F of the equation: [te] ~beta F([dbs])
 * in the context [ctx] *)
let resolve (ctx:term list) ((te,dbs):pb) : term =
  let size_minus_one = (List.length dbs) - 1 in
  let rec permute k = function
    | Kind | Type _ | Const _ as t -> t
    | App (f,a,args) ->
        mk_App (permute k f) (permute k a) (List.map (permute k) args)
    | Lam (l,x,a,b) -> mk_Lam l x (permute k a) (permute (k+1) b)
    | Pi (l,x,a,b) -> mk_Pi l x (permute k a) (permute (k+1) b)
    | DB (l,x,n) as db ->
        if n<k then db else
          ( match get_pos (n-k) dbs with
              | None -> raise NotUnifiable
              | Some p -> mk_DB l x (size_minus_one - p + k) )
  in
  let s = lam (permute 0 te) (permute_ctx ctx dbs) in
        Global.debug_no_loc 2 "Equation (in F): %a ~beta F(%a)" Pp.pp_term te
          (Pp.pp_list "," (fun out i -> Printf.fprintf out "%i" i))  dbs ;
        Global.debug_no_loc 1 "Solution: F = %a " Pp.pp_term s;
    if is_closed s then
      ( Global.debug_no_loc 1 "Solution: F = %a " Pp.pp_term s ; s )
    else ( Global.debug_no_loc 1 "No Solution."; raise NotUnifiable )

let resolve_lst (ctx:term list) (lst:pb LList.t) : term LList.t option =
  try Some (LList.map (resolve ctx) lst)
  with NotUnifiable -> None
                     *)
