
open Types

let ls (*Local Scope*) : int StringH.t = StringH.create 47

let rec of_pterm0 (n:int) = function 
  | PType                       -> Type
  | PId (_,v)                   -> 
      ( try DB (n-(StringH.find ls v)-1) 
        with Not_found -> GVar (!Global.name,v) )
  | PQid (_,m,v)                -> GVar (m,v)
  | PApp (f,u) as t             -> App ( get_app_lst n t [] )
  | PLam (v,None,t)             -> raise (ParserError "Not implemented (untyped lambda)")
  | PPi (None,a,b)              -> Pi  ( of_pterm0 n a , of_pterm0 (n+1) b )
  | PPi (Some (l,v),a,b)        -> 
      begin
        StringH.add ls v n ;
        let bb = of_pterm0 (n+1) b in
          StringH.remove ls v ;
          Pi  ( of_pterm0 n a , bb )
      end
  | PLam ((_,v),Some a,t)     -> 
      begin
        StringH.add ls v n ;
        let tt = of_pterm0 (n+1) t in
          StringH.remove ls v ;
          Lam ( of_pterm0 n a , tt )
      end
and get_app_lst n t args =
  match t with
    | PApp (f,u)        -> get_app_lst n f ((of_pterm0 n u)::args)
    | _                 -> (of_pterm0 n t)::args

let of_pterm te = of_pterm0 0 te 

(* Substitution *)

let rec shift2 (r:int) (k:int) = function
  | DB n        -> if n<k then DB n else DB (n+r)
  | App args    -> App ( List.map (shift2 r k) args )
  | Lam (a,f)   -> Lam (shift2 r k a,shift2 r (k+1) f)
  | Pi  (a,b)   -> Pi  (shift2 r k a,shift2 r (k+1) b)
  | t           -> t 

let shift k t = shift2 1 k t

let rec subst0 (k:int) (u:term) = function
  | DB n when k=n       -> u
  | DB n                -> if n>k then DB (n-1) else DB n
  | App args            -> App ( List.map (subst0 k u) args )
  | Lam (a,f)           -> Lam (subst0 k u a,subst0 (k+1) (shift 0 u) f)
  | Pi (a,b)            -> Pi  (subst0 k u a,subst0 (k+1) (shift 0 u) b)
  | t                   -> t

let subst t u = subst0 0 u t 

let rec psubst (nargs,args) k t = assert false 

let rec pop n = function (* n < size of the list *)
  | []                  -> assert false
  | a::_ when n=0       -> a
  | _::lst              -> pop (n-1) lst

let rec psubst_l (nargs,args) k t =  (*FIXME avoid beta redexes*)
  match t with
    | Type | Kind | GVar _ | Var _     -> t
    | DB n              ->
        ( match n with
            | n when n >= (k+nargs)     -> DB (n-nargs)
            | n when n < k              -> t 
            | n (* k <= n < k+nargs *)  -> shift2 0 k (Lazy.force (pop (n-k) args))
        )
    | App (he::tl)      ->
        let tl2 = List.map (psubst_l (nargs,args) k) tl in
          ( match psubst_l (nargs,args) k he with
              | App tl3 -> App (tl3@tl2)
              | he'     -> App (he'::tl2) )
    | App []            -> assert false
    | Lam (a,f)         -> Lam (psubst_l (nargs,args) k a,psubst_l (nargs,args) (k+1) f)
    | Pi  (a,b)         -> Pi  (psubst_l (nargs,args) k a,psubst_l (nargs,args) (k+1) b) 

(*
type config = int (*taille du contexte*) * cbv_cbn list (*contexte*) * term (*terme à réduire*) * cbv_cbn list (*stack*)
and cbv_cbn = config Lazy.t * term Lazy.t     

let rec unwind (k,e,t,s) =
  let t = 
    if k = 0 then t 
    else psubst (k,e) 0 t
  in
    if s = [] then t 
    else App ( t::(List.map (fun (_,c) -> Lazy.force c) s) )

let rec reduce (delta:int) (config:config) : config*bool = 
  match config with
    | ( _ , _ , Type , _ )              -> config, true
    | ( _ , _ , Kind , _ )              -> config, true
    | ( _ , _ , Pi _ , _ )              -> config, true
    | ( _ , _ , Lam _ , [] )            -> config, true
    | ( k , e , Lam (_,t) , p::s )      -> reduce delta ( k+1 , p::e, t , s )
    | ( _ , _ , GVar v , s )            -> 
        ( match Env.get_global_def v with
            | None      -> config, true
            | Some te   ->
                if delta >= 0 then (*FIXME*) config, false
                else reduce delta (0,[],te,s)
        )
    | ( _ , _ , EVar (m,v) , _ )        -> assert false (*TODO*)
    | ( k , e , DB n , s ) when k<n     ->
        let k',e',t',s' = Lazy.force (fst (List.nth e n)) in
          reduce delta (k',e',t',s'@s)
    | ( k, _ , DB n , s ) (* n >= k *)  -> config, true
    | ( _ , _ , App ([]|[_]) , _ )      -> assert false
    | ( k , e , App (he::tl) , s )      ->
        let tl' = List.map ( 
          fun t -> 
            ( lazy (fst (reduce delta (k,e,t,[]))) , lazy (unwind (k,e,t,[])) )                       
        ) tl in
          reduce delta (k, e, he, tl' @ s)
*)

(* Call-by-Value *)
(*
type cbv_state = int (*taille du contexte*) * term list (*contexte*) * term (*terme à réduire*) * cbv_state list (*stack*)

let rec cbv_term_of_state (k,e,t,s:cbv_state) : term =
  let t = 
    if k = 0 then t 
    else psubst (k,e) 0 t
  in
    if s = [] then t 
    else App ( t::(List.map cbv_term_of_state s) )

let rec cbv_reduce (delta:int) (config:cbv_state) : ( cbv_state * bool ) = 
  match config with
  | ( _ , _ , Type , _ )              -> config, true
  | ( _ , _ , Kind , _ )              -> config, true
  | ( _ , _ , Pi _ , _ )              -> config, true
  | ( _ , _ , Lam _ , [] )            -> config, true
  | ( k , e , Lam (_,t) , p::s )      -> cbv_reduce delta ( k+1 , (cbv_term_of_state p)::e, t , s )
  | ( _ , _ , GVar v , s )            -> 
      ( match Env.get_global_def v with
          | None      -> config, true
          | Some te   ->
              if delta >= 0 then (*FIXME*) config, false
              else cbv_reduce delta (0,[],te,s)
      )
  | ( _ , _ , EVar (m,v) , _ )        -> assert false (*TODO*)
  | ( k , e , DB n , s ) when n<k     -> cbv_reduce delta (0,[],List.nth e n,s)
  | ( k, _ , DB n , s ) (* n >= k *)  -> config, true
  | ( _ , _ , App ([]|[_]) , _ )      -> assert false
  | ( k , e , App (he::tl) , s )      ->
      let tl' = List.map ( fun t -> (k,e,t,[]) ) tl in
        cbv_reduce delta (k, e, he, tl' @ s)
 *)

(* Call-by-Need *)

type cbn_state = int (*taille du contexte*) * (term Lazy.t) list (*contexte*) * term (*terme à réduire*) * cbn_state list (*stack*)

let rec cbn_term_of_state (k,e,t,s:cbn_state) : term =
  let t = 
    if k = 0 then t 
    else psubst_l (k,e) 0 t
  in
    if s = [] then t 
    else App ( t::(List.map cbn_term_of_state s) )

let rec cbn_reduce (delta:int) (config:cbn_state) : ( cbn_state * bool ) = 
  match config with
    | ( _ , _ , Type , _ )              -> config, true
    | ( _ , _ , Kind , _ )              -> config, true
    | ( _ , _ , Pi _ , _ )              -> config, true
    | ( _ , _ , Lam _ , [] )            -> config, true
    | ( k , e , Lam (_,t) , p::s )      -> cbn_reduce delta ( k+1 , (lazy (cbn_term_of_state p))::e, t , s )
    | ( _ , _ , GVar (m,v) , s )        -> 
        ( match Env.get_global_def m v with
            | None      -> config, true
            | Some te   ->
        (* FIXME if delta >= 0 then config, false
         else *) cbn_reduce delta (0,[],te,s)
        )
    | ( _ , _ , Var _ , _ )             -> config,true
    | ( k , e , DB n , s ) when n<k     -> cbn_reduce delta (0,[],Lazy.force (pop n e),s)
    | ( k, _ , DB n , s ) (* n >= k *)  -> config, true
    | ( _ , _ , App ([]|[_]) , _ )      -> assert false
    | ( k , e , App (he::tl) , s )      ->
        let tl' = List.map ( fun t -> (k,e,t,[]) ) tl in
          cbn_reduce delta (k, e, he, tl' @ s)

(* ... *)

let dump_state (k,e,t,s) =
  Global.msg ("k = "^string_of_int k^"\n");
  Global.msg ("t = "^Debug.string_of_term t^"\n");
  Global.msg "e = [";
  List.iter (fun u -> Global.msg (" ("^Debug.string_of_term (Lazy.force u)^")")) e ;
  Global.msg " ]\ns = [";
  List.iter (fun (_,_,u,_) -> Global.msg (" {{ "^Debug.string_of_term u^" }}")) s ;
  Global.msg " ]\n"

let hnf (t:term) : term = cbn_term_of_state (fst (cbn_reduce 0 (*FIXME*) (0,[],t,[])))

let term_eq (t1:term) (t2:term) : bool = t1 == t2 || t1=t2 
 
let nnn = ref 0
let mk_fvar () =
  let n = !nnn in
  incr nnn ; lazy (Var n)

let rec add_to_list lst s s' =
  match s,s' with
    | [] , []           -> lst
    | x::s , y::s'      -> add_to_list ((x,y)::lst) s s'
    | _ ,_              -> assert false

let rec state_conv (delta:int) : (cbn_state*cbn_state) list -> bool = function
  | []                  -> true
  | (s1,s2)::lst       ->
      begin
        let rec aux = function (*states are beta-delta head normal*)
          | ( _ , _ , Kind ) , ( _ , _ , Kind )                       -> ( true , None )
          | ( _ , _ , Type ) , ( _ , _ , Type )                       -> ( true , None )
          | ( _ , _ , Var n ) , ( _ , _ , Var n' )                    -> ( n=n' , None )
          | ( _ , _ , GVar (m,v) ) , ( _ , _ , GVar (m',v') )         -> ( m=m' && v=v' , None )
          | ( k , _ , DB n ) , ( k' , _ , DB n' )                     -> ( n=n' , None )
          | ( k , e , Lam (a,f) ) , ( k' , e' , Lam (a',f') )          
          | ( k , e , Pi  (a,f) ) , ( k' , e' , Pi  (a',f') )         -> 
              let x = mk_fvar () in
              ( true , Some ( 
                ( (k,e,a,[]) , (k',e',a',[]) ) ,
                ( (k+1,x::e,f,[]) , (k'+1,x::e',f',[]) )
              ) )
          | ( _ , _ , _ ) , ( _ , _ , _ )                               -> ( false , None )
        in

          term_eq (cbn_term_of_state s1) (cbn_term_of_state s2) ||
          let ( (k,e,t,s)     , norm1 ) = cbn_reduce delta s1 in
          let ( (k',e',t',s') , norm2 ) = cbn_reduce delta s2 in
    
      (*Global.msg " --- Dump s1\n";
      dump_state s1 ; 
      Global.msg " --- Dump s2\n";
      dump_state s2 ;
      Global.msg " --- Dump N(s1)\n";
      dump_state (k,e,t,s) ;
      Global.msg " --- Dump N(s2)\n";
      dump_state (k',e',t',s') ;*)

            match norm1 , norm2 with
              | true , true   -> (* Both t and t' are beta-delta head normal*)
                  ( match aux ( (k,e,t) , (k',e',t') ) with
                      | true , None             -> state_conv delta (add_to_list lst s s')
                      | true , Some (x,y)       -> state_conv delta (add_to_list (x::y::lst) s s')
                      | false , _               -> false 
                  ) 
              | true , false  -> (* TODO Only the first term is delta head normal *) assert false
              | false , true  -> (* TODO Only the second term is delta head normal *) assert false
              | false , false -> (* TODO *) assert false
      end

let are_convertible t1 t2 =
  state_conv 0 [ ((0,[],t1,[]),(0,[],t2,[])) ]


