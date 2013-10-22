
open Types

let estring = Global.hstring ""

type cbn_state = int (*size of context *) * (term Lazy.t) list (*context*) * term (*term to reduce*) * cbn_state list (*stack*)

let rec cbn_term_of_state (k,e,t,s:cbn_state) : term =
  let t = ( if k = 0 then t else Subst.psubst_l (k,e) 0 t ) in
    if s = [] then t 
    else App ( t::(List.map cbn_term_of_state s) )
           
let rec split_stack (i:int) : cbn_state list -> (cbn_state list*cbn_state list) option = function 
  | l  when i=0 -> Some ([],l)
  | []          -> None
  | x::l        -> (
    match split_stack (i-1) l with
      | None            -> None
      | Some (s1,s2)    -> Some (x::s1,s2)
    )

let safe_find (m:string) (v:string) (cases:((string*string)*gdt) list) : gdt option =
  try Some (snd (List.find (fun ((m',v'),_) -> m=m' && v=v') cases))
  with Not_found -> None

let rec remove c lst =
  match lst with
    | []        -> assert false
    | x::lst'   -> 
        if c==0 then lst'
        else x::(remove (c-1) lst')

let mk_new_args c args1 lst = 
  (remove c args1)@lst (*FIXME*)
       
let rec cbn_reduce (config:cbn_state) : cbn_state = 
  match config with
    (* Weak normal terms *)
    | ( _ , _ , Type , _ )              -> config
    | ( _ , _ , Kind , _ )              -> config
    | ( _ , _ , Pi _ , _ )              -> config
    | ( _ , _ , Lam _ , [] )            -> config
    | ( k , _ , DB n , _ ) when (n>=k)  -> config
    (* Bound variable (to be substitute) *)
    | ( k , e , DB n , s ) (*when n<k*) -> cbn_reduce ( 0 , [] , Lazy.force (List.nth e n) , s )
    (* Beta redex *)
    | ( k , e , Lam (_,t) , p::s )      -> cbn_reduce ( k+1 , (lazy (cbn_term_of_state p))::e , t , s )
    (* Application *)
    | ( _ , _ , App ([]|[_]) , _ )      -> assert false
    | ( k , e , App (he::tl) , s )      ->
        let tl' = List.map ( fun t -> (k,e,t,[]) ) tl in
          cbn_reduce ( k , e , he , tl' @ s ) (*FIXME*)
    (* Global variable*)
    | ( _ , _ , GVar (m,_) , _ ) when m==estring        -> config (*FIXME*)
    | ( _ , _ , GVar (m,v) , s )        -> 
        begin
          match Env.get_global_symbol m v with
            | Env.Def (te,_)            -> cbn_reduce ( 0 , [] , te , s )
            | Env.Decl (_,None)         -> config
            | Env.Decl (_,Some (i,g))   -> 
                ( match split_stack i s with
                    | None                -> config
                    | Some (s1,s2)        ->
                        ( match rewrite s1 g with
                            | None              -> config
                            | Some (k,e,t)      -> cbn_reduce ( k , e , t , s2 ) 
                        )
                ) 
        end

and rewrite (args:cbn_state list) (g:gdt) : (int*(term Lazy.t) list*term) option = 
  match g with
    | Leaf te                     -> Some ( List.length args (*FIXME*) , List.map (fun a -> lazy (cbn_term_of_state a)) args , te ) 
    | Switch (i,cases,def)        -> 
        begin
          (* assert (i<Array.length args); *)
          match cbn_reduce (List.nth args i) with
            | ( _ , _ , GVar (m,v) , s )  -> 
                ( match safe_find m v cases , def with
                    | Some g , _        -> rewrite (mk_new_args i args s) g
                    | None , Some g     -> rewrite args g
                    | _ , _             -> None )
            | ( _ , _ , _ , s ) -> 
                (match def with
                   | Some g     -> rewrite args g
                   | None       -> None )
        end

(* Head Normal Form *)          
let hnf (t:term) : term = cbn_term_of_state ( cbn_reduce ( 0 , [] , t , [] ) ) 

let rec cbn_term_of_state2 (k,e,t,s:cbn_state) : term =
  let t = ( if k = 0 then t else Subst.psubst_l (k,e) 0 t ) in
    if s = [] then t 
    else App ( t::(List.map (fun st -> cbn_term_of_state2 (cbn_reduce st)) s ))

(* Weak Normal Form *)          
let wnf (t:term) : term = cbn_term_of_state2 (cbn_reduce (0,[],t,[])) 

(* *** Conversion Test *** *)

(* Syntactic equality *)                            
let term_eq (t1:term) (t2:term) : bool = t1 == t2 || t1=t2 
 
let rec add_to_list lst s s' =
  match s,s' with
    | [] , []           -> lst
    | x::s1 , y::s2      -> add_to_list ((x,y)::lst) s1 s2
    | _ ,_              -> assert false

let nnn = ref 0
let get_arg _ =
  let n = !nnn in
    incr nnn ;
    lazy (GVar (estring,Global.hstring (string_of_int n)))

      (*
let print_debug s1 s2 =
  Global.print "\n -------- CONV -------- \n";
  Global.print " ---> S1\n";
  Error.dump_state s1 ; 
  Global.print " ---> HNF(S1)\n";
  Error.dump_state (k,e,t,s) ;
  Global.print " --- S2\n";
  Error.dump_state s2 ;
  Global.print " --- HNF(S2)\n";
  Error.dump_state (k',e',t',s')
        *)

let rec state_conv : (cbn_state*cbn_state) list -> bool = function
  | []                  -> true
  | (s1,s2)::lst        ->
      begin
        let t1 = cbn_term_of_state s1 in
        let t2 = cbn_term_of_state s2 in
          if term_eq t1 t2 then 
            state_conv lst
          else
            let s1' = cbn_reduce s1 in
            let s2' = cbn_reduce s2 in
              match s1',s2' with (*states are beta-delta head normal*)
                | ( _ , _ , Kind , s ) , ( _ , _ , Kind , s' )                    -> state_conv (add_to_list lst s s')
                | ( _ , _ , Type , s ) , ( _ , _ , Type , s' )                    -> state_conv (add_to_list lst s s')
                | ( k , _ , DB n , s ) , ( k' , _ , DB n' , s' )                  -> ( (*assert (k<=n && k'<=n') ;*) (n-k)=(n'-k') && state_conv (add_to_list lst s s') )
                | ( _ , _ , GVar (m,v) , s ) , ( _ , _ , GVar (m',v') , s' )      -> ( m=m' && v=v' && state_conv (add_to_list lst s s') )
                | ( k , e , Lam (a,f) , s ) , ( k' , e' , Lam (a',f') , s' )          
                | ( k , e , Pi  (a,f) , s ) , ( k' , e' , Pi  (a',f') , s' )      -> 
                    let arg = get_arg () in
                    let x = ( (k,e,a,[]) , (k',e',a',[]) ) in
                    let y = ( (k+1,arg::e,f,[]) , (k'+1,arg::e',f',[]) ) in
                      state_conv (add_to_list (x::y::lst) s s')
                | ( _ , _ , _ , _ ) , ( _ , _ , _ , _ )                            -> false
      end

let are_convertible t1 t2 =
  state_conv [ ( (0,[],t1,[]) , (0,[],t2,[]) ) ]

(* ----------------- *)
(*
let rec state_unif (sub:(int*term) list) : (cbn_state*cbn_state) list -> ((int*term) list) option = function
  | []                  -> Some sub
  | (s1,s2)::lst        ->
      begin
        let t1 = cbn_term_of_state s1 in
        let t2 = cbn_term_of_state s2 in
          if term_eq t1 t2 then 
            state_unif sub lst
          else
            let s1' = cbn_reduce s1 in
            let s2' = cbn_reduce s2 in
              match s1',s2' with 
                (* Base Cases*)
                | ( _ , _ , Kind , s ) , ( _ , _ , Kind , s' )                          -> state_unif sub (add_to_list lst s s')
                | ( _ , _ , Type , s ) , ( _ , _ , Type , s' )                          -> state_unif sub (add_to_list lst s s')
                | ( _ , _ , GVar (m,v) , s ) , ( _ , _ , GVar (m',v') , s' )            -> if m==m' && v==v' then state_unif sub (add_to_list lst s s') else None
                | ( k , _ , DB n , s ) , ( k' , _ , DB n' , s' ) when (n<k && n'<k')    -> if (n-k)=(n'-k')  then state_unif sub (add_to_list lst s s') else None
                (*Composed Cases*)
                | ( k , e , Lam (a,f) , s ) , ( k' , e' , Lam (a',f') , s' )          
                | ( k , e , Pi  (a,f) , s ) , ( k' , e' , Pi  (a',f') , s' )            -> 
                    let arg = get_arg () in
                    let x = ( (k,e,a,[]) , (k',e',a',[]) ) in
                    let y = ( (k+1,arg::e,f,[]) , (k'+1,arg::e',f',[]) ) in
                      state_unif sub (add_to_list (x::y::lst) s s')
                (* Unification *)
                | ( ( k , _ , DB n , [] ) , st )
                | ( st , ( k , _ , DB n , [] ) )  when ( n>=k )                         -> state_unif  ((n-k,cbn_term_of_state st)::sub) lst
                (* Ignored Cases *)
                | ( k , _ , DB n , _ ) , _ 
                | _ , ( k , _ , DB n , _ )   when ( n>=k )                              -> state_unif sub lst
                (*Not Unifiable*)
                | ( _ , _ , _ , _ ) , ( _ , _ , _ , _ )                                 -> None
      end
 *)
