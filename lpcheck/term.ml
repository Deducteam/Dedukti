
open Types

let dump_state (k,e,t,s) =
  Global.print ("k = "^string_of_int k^"\n");
  Global.print ("t = "^Debug.string_of_term t^"\n");
  Global.print "e = [";
  List.iter (fun u -> Global.print (" ("^Debug.string_of_term (Lazy.force u)^")")) e ;
  Global.print " ]\ns = [";
  List.iter (fun (_,_,u,_) -> Global.print (" {{ "^Debug.string_of_term u^" }}")) s ;
  Global.print " ]\n"

let pos x lst = 
  let rec aux n = function
    | []                  -> None
    | y::lst when x=y     -> Some n
    | y::lst              -> aux (n+1) lst
  in
    aux 0 lst

let rec pop n = function (* n < size of the list *)
  | []                  -> assert false
  | a::_ when n=0       -> a
  | _::lst              -> pop (n-1) lst

(* ... *)

(* Invariant: k == List.length ctx *)      
let rec of_pterm (k:int) (ctx:string list) : pterm -> term = function 
  | PType                       -> Type
  | PId (_,v)                   -> 
      ( match pos v ctx with
          | None        -> GVar (!Global.name,v)
          | Some n      -> DB n )
  | PQid (_,m,v)                -> GVar (m,v)
  | PApp (f,u) as t             -> App ( get_app_lst k ctx t [] )
  | PLam (v,None,t)             -> raise (ParserError "Not implemented (untyped lambda)")
  | PPi (None,a,b)              -> Pi  ( of_pterm k ctx a , of_pterm (k+1) (""::ctx) b )
  | PPi (Some (l,v),a,b)        -> Pi  ( of_pterm k ctx a , of_pterm (k+1) ( v::ctx) b )
  | PLam ((_,v),Some a,t)       -> Lam ( of_pterm k ctx a , of_pterm (k+1) ( v::ctx) t )
and get_app_lst k ctx t args =
  match t with
    | PApp (f,u)        -> get_app_lst k ctx f ((of_pterm k ctx u)::args)
    | _                 -> (of_pterm k ctx t)::args

(* Invariant: k == List.length ctx *)      
let rec term_of_pat (k:int) (ctx:string list) : pattern -> term = function
  | Pat ((_,m,v),ds,ps) ->
      begin
          if m = !Global.name && Array.length ps=0 && Array.length ds=0 then
            ( match pos v ctx with
                | None        -> GVar (!Global.name,v)
                | Some n      -> DB n )
          else 
            let l1 = Array.fold_right (fun p lst -> (term_of_pat k ctx p)::lst) ps [] in
            let l2 = Array.fold_right (fun t lst -> (of_pterm k ctx t)::lst) ds l1 in
              App ( (GVar (m,v))::l2)
      end

let term_of_tpat k ctx ((l,v),ds,ps:top_pattern) : term = term_of_pat k ctx (Pat ((l,!Global.name,v),ds,ps))

(* Substitution *)

let rec shift (r:int) (k:int) = function
  | DB n        -> if n<k then DB n else DB (n+r)
  | App args    -> App ( List.map (shift r k) args )
  | Lam (a,f)   -> Lam (shift r k a,shift r (k+1) f)
  | Pi  (a,b)   -> Pi  (shift r k a,shift r (k+1) b)
  | t           -> t 

(* nargs == List.length args *)
let rec psubst_l (nargs,args) k t =  
  match t with
    | Type | Kind | GVar _ | LVar _     -> t
    | DB n when (n >= (k+nargs))        -> DB (n-nargs)
    | DB n when (n < k)                 -> DB n
    | DB n (* when (k<=n<(k+nargs)) *)  -> shift k 0 (Lazy.force (pop (n-k) args))
    | Lam (a,b)                         -> Lam ( psubst_l (nargs,args) k a , psubst_l (nargs,args) (k+1) b )
    | Pi  (a,b)                         -> Pi  ( psubst_l (nargs,args) k a , psubst_l (nargs,args) (k+1) b ) 
    | App []                            -> assert false
    | App (he::tl)                      ->
        let tl' = List.map (psubst_l (nargs,args) k) tl in
          ( match psubst_l (nargs,args) k he with
              | App tl0 -> App (tl0@tl')
              | he'     -> App (he'::tl') )

(* nargs == List.length args *)
let rec psubst (nargs,args) k t =  
  match t with
    | Type | Kind | GVar _ | LVar _     -> t
    | DB n when (n >= (k+nargs))        -> DB (n-nargs)
    | DB n when (n < k)                 -> DB n
    | DB n (* when (k<=n<(k+nargs)) *)  -> shift k 0 (pop (n-k) args)
    | Lam (a,b)                         -> Lam ( psubst (nargs,args) k a , psubst (nargs,args) (k+1) b )
    | Pi  (a,b)                         -> Pi  ( psubst (nargs,args) k a , psubst (nargs,args) (k+1) b ) 
    | App []                            -> assert false
    | App (he::tl)                      ->
        let tl' = List.map (psubst (nargs,args) k) tl in
          ( match psubst (nargs,args) k he with
              | App tl0 -> App (tl0@tl')
              | he'     -> App (he'::tl') )

let subst t u = psubst (1,[u]) 0 t

(* Call-by-Need *)

type cbn_state = int (*taille du contexte*) * (term Lazy.t) list (*contexte*) * term (*terme à réduire*) * cbn_state list (*stack*)

let rec cbn_term_of_state (k,e,t,s:cbn_state) : term =
  let t = ( if k = 0 then t else psubst_l (k,e) 0 t ) in
    if s = [] then t 
    else App ( t::(List.map cbn_term_of_state s) )
           
let rec split_stack i = function (*FIXME*)
  | l  when i=0 -> Some ([],l)
  | []          -> None
  | x::l        -> (
    match split_stack (i-1) l with
      | None            -> None
      | Some (s1,s2)    -> Some (x::s1,s2)
    )
            
let get_gdt m v cases def =
  try Some (snd (List.find (fun ((m',v'),_) -> m=m' && v=v') cases))
  with Not_found -> None

let mk_new_args c args1 lst = (*FIXME*)
  let s1 = Array.length args1 in
  let args2 = Array.of_list lst in
  let s2 = Array.length args2 in
  Array.init (s1+s2-1) (
    fun i ->
      if i<c then args1.(i)
      else if i<(s1-1) then args1.(i+1)
      else args2.(i-(s1-1))
  )

let mk_args a = Array.of_list a (*FIXME*)

let rec cbn_reduce (config:cbn_state) : cbn_state = 
  match config with

    | ( _ , _ , Type , _ )              -> config
    | ( _ , _ , Kind , _ )              -> config
    | ( _ , _ , Pi _ , _ )              -> config
    | ( _ , _ , LVar _ , _ )            -> config
    | ( _ , _ , Lam _ , [] )            -> config
    | ( k , _ , DB n , _ ) when (n>=k)  -> config
    | ( _ , _ , App ([]|[_]) , _ )      -> assert false

    | ( k , e , DB n , s ) (*when n<k*) -> cbn_reduce ( 0 , [] , Lazy.force (pop n e) , s )       (*FIXME shift?*)
    | ( k , e , Lam (_,t) , p::s )      -> cbn_reduce ( k+1 , (lazy (cbn_term_of_state p))::e , t , s )
    | ( k , e , App (he::tl) , s )      ->
        let tl' = List.map ( fun t -> (k,e,t,[]) ) tl in
          cbn_reduce ( k , e , he , tl' @ s )

    | ( _ , _ , GVar (m,v) , s )        -> 
        begin
          match Env.get_global_symbol m v with
            | Env.Def (te,_)            -> cbn_reduce ( 0 , [] , te , s )
            | Env.Decl (_,None)         -> config
            | Env.Decl (_,Some (i,g))   -> 
                ( match split_stack i s with
                    | None                -> config
                    | Some (s1,s2)        ->
                        ( match rewrite (mk_args s1) g with
                            | None              -> config
                            | Some (k,e,t)      -> cbn_reduce ( k , e , t , s2 ) 
                        )
                ) 
        end

and rewrite (args:cbn_state array) (g:gdt)  = 
  match g with
    | Leaf te                     -> Some ( Array.length args , List.map (fun a -> lazy (cbn_term_of_state a)) (Array.to_list args) , te ) 
    | Switch (i,cases,def)        -> 
        begin
          match cbn_reduce (args.(i)) with
            | ( _ , _ , GVar (m,v) , s )  -> 
                ( match get_gdt m v cases def with
                    | None        -> None
                    | Some g      -> rewrite (mk_new_args i args s) g
                )
            | _                           -> None
        end
(*
and cbn_reduce0 s =
  let s' = cbn_reduce s in
    Global.print_v ("\n------- REDUCE\n");
    dump_state s;
    Global.print_v ("------->\n");
    dump_state s';
    Global.print_v ("------- END \n");
    s'
 *)

(* ... *)

let hnf (t:term) : term = cbn_term_of_state (cbn_reduce (0,[],t,[]))

let term_eq (t1:term) (t2:term) : bool = t1 == t2 || t1=t2 
 
let nnn = ref 0
let mk_fvar () =
  let n = !nnn in
  incr nnn ; lazy (LVar n)

let rec add_to_list lst s s' =
  match s,s' with
    | [] , []           -> lst
    | x::s , y::s'      -> add_to_list ((x,y)::lst) s s'
    | _ ,_              -> assert false

let rec state_conv : (cbn_state*cbn_state) list -> bool = function
  | []                  -> true
  | (s1,s2)::lst        ->
      begin
        let rec aux = function (*states are beta-delta head normal*)
          | ( _ , _ , Kind ) , ( _ , _ , Kind )                       -> ( true , None )
          | ( _ , _ , Type ) , ( _ , _ , Type )                       -> ( true , None )
          | ( _ , _ , LVar n ) , ( _ , _ , LVar n' )                  -> ( n=n' , None )
          | ( _ , _ , GVar (m,v) ) , ( _ , _ , GVar (m',v') )         -> ( m=m' && v=v' , None )
          | ( k , _ , DB n ) , ( k' , _ , DB n' )                     -> ( assert (k<=n && k'<=n') ; (n-k)=(n'-k') , None )
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
          let (k,e,t,s)         = cbn_reduce s1 in
          let (k',e',t',s')     = cbn_reduce s2 in
    
      (*Global.print "\n -------- CONV -------- \n";
      Global.print " ---> S1\n";
      dump_state s1 ; 
      Global.print " ---> HNF(S1)\n";
      dump_state (k,e,t,s) ;
      Global.print " --- S2\n";
      dump_state s2 ;
      Global.print " --- HNF(S2)\n";
      dump_state (k',e',t',s') ;*)

            match aux ( (k,e,t) , (k',e',t') ) with
              | true , None             -> state_conv (add_to_list lst s s')
              | true , Some (x,y)       -> state_conv (add_to_list (x::y::lst) s s')
              | false , _               -> false 
      end

let are_convertible t1 t2 =
  state_conv [ ((0,[],t1,[]),(0,[],t2,[])) ]


