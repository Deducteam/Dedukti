
open Types

(* Compilation of rewrite rules to decision tree as in:
 * Compiling Pattern Matching to Good Decision Trees (Maranget, 2008)
 * *)

module H = Hashtbl.Make(
struct
  type t        = ident*ident
  let hash      = Hashtbl.hash
  let equal (m,v) (m',v') =
    ident_eq v v' && ident_eq m m'
end )

(*let foldi (type a) (type b) (f:int -> 'a -> 'b -> 'a)  (z:'a) (arr:'b array) : 'a =*)
let foldi f z arr =
  let rec aux i =
    if i < Array.length arr then f i (aux (i+1)) arr.(i)
    else z
  in
    aux 0

(* *** Types *** *)

type line = { loc:loc ; pats:pattern array ; right:term ; env_size:int ; }
type pMat = line list
type union = PMat of pMat | Term of term
type partition = { cases:( (ident*ident) * union ) list ; default: pMat option ; }

(* *** Debug *** *)

(*
let dump_line (id:string) (l:line) =
    Global.eprint ( " [ " ^ string_of_int l.env_size ^ " ] " ^ id ^ " " ) ;
    Array.iter (fun p -> Global.eprint (Pp.string_of_pattern p^"\t")) l.pats ;
    Global.eprint ( " --> " ^ Pp.string_of_term l.right ^ "\n" )

let dump_pMat (id:string) (pm:pMat) =
  Global.eprint (" --------> PMAT FOR "^id^"\n");
  List.iter (dump_line id) pm ;
  Global.eprint " <-------- \n"
 *)

(* *** Internal functions *** *)

let pMat_from_rules (rs:rule list) : pMat =
  match rs with
    | (l,ctx,id,pats,ri)::rs'  ->
        let arity = Array.length pats in
          { loc=l; pats=pats ; right=ri ; env_size=List.length ctx } :: (
            List.map (
              fun (l,ctx',id',pats',ri') ->
                if Array.length pats' != arity then
                  raise ( PatternError ( l , "All the rules must have the same arity." ) )
                else if (not (ident_eq id id')) then
                  raise ( PatternError ( l , "All the rules must have the same head symbol." ) )
                else { loc=l ; pats=pats' ; right=ri' ; env_size=List.length ctx' }
            ) rs'
          )
    | _ -> assert false

let union_from_lines (lst:line list) : union =
  match lst with
    | []        -> assert false
    | x::lst'   ->
        let arity = Array.length x.pats in
          if arity = 0 then Term x.right
          else
            begin
              assert ( List.for_all (fun y -> Array.length y.pats = arity ) lst' ) ;
              PMat lst
            end

let qm = hstring "?"
let mk_var_lst inf sup =
  let rec aux i =
    if i<sup then
      (mk_DB qm i) :: (aux (i+1))
    else []
  in aux inf

let specialize (c:int) (pm:pMat) (nargs,m,v:int*ident*ident) : (ident*ident)*union =
  let lines = List.fold_right (
    fun l lst ->
      let n = Array.length l.pats - 1 in
      match l.pats.(c) with
        | Var (_,q)                   ->
            let te =
              if nargs=0 then mk_Const m v
              else mk_App ( (mk_Const m v)::(mk_var_lst l.env_size (l.env_size+nargs) ) ) in
            { loc       = l.loc ;
              env_size  = l.env_size + nargs ;
              right     = Subst.subst_q (q,te) 0 l.right ;
              pats      =
                Array.init (n + nargs) (
                  fun i ->
                    if i < c      then l.pats.(i)               (* [ 0 - c [ *)
                    else if i < n then l.pats.(i+1)             (* [ c - n [ *)
                    else               Var (qm,l.env_size+i-n)  (* [ n - n+nargs [ *)
                )
            }::lst
        | Joker _                        ->
              { l with pats =
                  Array.init (n + nargs) (
                    fun i ->
                      if i < c      then l.pats.(i)       (* [ 0 - c [ *)
                      else if i < n then l.pats.(i+1)     (* [ c - n [ *)
                      else               Joker 0    (* [ n - n+nargs [ *)
                  )
              }::lst
        | Pattern (m',v',args)      ->
            if not (ident_eq v v' && ident_eq m m') then lst
            else
                { l with pats =
                    Array.init (n + Array.length args) (
                      fun i ->
                        if i < c      then l.pats.(i)       (* [ 0 - c [ *)
                        else if i < n then l.pats.(i+1)     (* [ c - n [ *)
                        else               args.(i-n)       (* [ n - n+nargs [ *)
                    )
                }::lst
  ) pm [] in
    ( (m,v) , union_from_lines lines )

let partition (pm:pMat) (c:int) : partition =
  let hs = H.create 17 in
  let (def,consts) = List.fold_right (
    fun l (def,csts) -> match l.pats.(c) with
      | Pattern (m,v,args)      -> 
          if H.mem hs (m,v) then (def,csts) 
          else ( 
            H.add hs (m,v) () ;
            (def,(Array.length args,m,v)::csts)
          )
      | _                       -> (l::def,csts)
  ) pm ([],[]) in
  let cases = List.map (specialize c pm) consts in
    match def with
      | []    -> { cases=cases ; default=None ; }
      | _     -> { cases=cases ; default=Some def ; }

let getColumn (l:pattern array) : int option =
  let rec aux i =
    if i < Array.length l then
      ( match l.(i) with
        | Pattern _     -> Some i
        | _             -> aux (i+1) )
    else None
  in aux 0

let rec reorder l (ord:int array) (k:int) : term -> term = function
  | App args                    -> mk_App (List.map (reorder l ord k) args)
  | Lam (x,a,f)                 -> mk_Lam x (reorder l ord k a) (reorder l ord (k+1) f)
  | Pi  (x,a,b)                 -> mk_Pi  x (reorder l ord k a) (reorder l ord (k+1) b)
  | DB (x,n) when (n>=k)        ->
      begin
        (*assert (n-k < Array.length ord);*)
        let n_db = ord.(n-k) in
          if n_db = (-1) then
            raise ( PatternError ( l ,
              "Free variables on the right-hand side of a rule should also appear in the left-hand side." ) )
          else mk_DB x (n_db + k)
      end
  | t                   -> t

let get_term (l:line) : (int*int)list*term =
  let ord = Array.make l.env_size (-1) in
  let lst =
    foldi (
      fun i lst p ->
        match p with
          | Var (_,n)   ->
              if ord.(n) = (-1) then ( ord.(n) <- i ; lst )
              else (
                Global.warning l.loc "Non linear rules are experimental." ;
                (ord.(n),i)::lst
              )
          | Joker _     -> lst
          | Pattern _   -> assert false
    ) [] l.pats in
    ( lst , reorder l.loc ord 0 l.right )

let rec cc (pm:pMat) : gdt =
  let (first,pm2) = match pm with | [] -> assert false | x::y -> (x,y) in
    match getColumn first.pats with
      (* La 1ere ligne ne contient que des Var *)
      | None      ->
          begin
            match get_term first , pm2 with
              | ( [] , te ) , _         -> Test ([] ,te,None)
              | ( lst, te ) , []        -> Test (lst,te,None)
              | ( lst, te ) , _         -> Test (lst,te,Some (cc pm2))
          end
      (* Colonne c contient un pattern *)
      | Some c    ->
          let par = partition pm c in
          let cases =
            List.rev_map ( (*rev ?*)
              function
                | ( id , Term te  )     -> ( id , Test ([],te,None) )   
                | ( id , PMat pm' )     -> ( id , cc pm' )
            ) par.cases in
            ( match par.default with
                | None           -> Switch (c,cases,None)
                | Some pm'       -> Switch (c,cases,Some (cc pm'))
            )

let safe_find m v cases =
  try Some ( snd ( List.find (fun ((m',v'),_) -> ident_eq v v' && ident_eq m m') cases ) )
  with Not_found -> None

let rec replace m v g = function
  | []                          -> assert false
  | (((m',v'),g') as c)::lst    ->
      if ident_eq v v' && ident_eq m m' then ((m,v),g)::lst
      else c::(replace m v g lst)



let rec add_lines pm = function
  | Test ([],_,_) as g          ->
      ( Global.warning (match pm with [] -> assert false | x::_ -> x.loc ) "Useless rule." ; g )
  | Test (lst,te,None)          -> Test (lst,te,Some (cc pm))
  | Test (lst,te,Some g)        -> Test (lst,te,Some (add_lines pm g))
  | Switch (i,cases,def)        ->
      begin
        let p = partition pm i in
        let cases2 = List.fold_left (
          fun ca ((m,v),u) ->
            match safe_find m v cases , u with
              | None , PMat pm'       -> ( (m,v) , cc pm' )::ca
              | None , Term te        -> ( (m,v) , Test ([],te,None) )::ca
              | Some g , PMat pm'     -> replace m v (add_lines pm' g) ca
              | Some g , Term te      ->
                  ( match g with
                      | Test ([],_,_)   -> ca
                      | _               -> assert false (*FIXME*)
                  )
        ) cases p.cases in
        let def2 = match ( def , p.default ) with
          | _ , None          -> def
          | None , Some pm'   -> Some (cc pm')
          | Some d , Some pm' -> Some (add_lines pm' d)
        in
          Switch (i,cases2,def2)
      end

let add_rw (n,g) rs =
  match pMat_from_rules rs with
    | []                -> assert false
    | l::_ as pm        ->
        if Array.length l.pats != n then
          raise ( PatternError ( l.loc , "Arity mismatch: all the rules must have the same arity." ) )
        else
          ( n , add_lines pm g )

let get_rw (rs:rule list) : int*gdt =
  let pm  = pMat_from_rules rs in
     (* dump_pMat "???" pm *)  
  let gdt = cc pm in
     (*dump_gdt "???" gdt ; *)
  let le = match pm with | x::_ -> Array.length x.pats | _ -> assert false in
    ( le , gdt )
