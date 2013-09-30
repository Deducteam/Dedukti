
open Types

(* *** Weak Reduction *** *)

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
            
let get_gdt (m:string) (v:string) (cases:((string*string)*gdt) list) (def:gdt option) : gdt option =
  try Some (snd (List.find (fun ((m',v'),_) -> m=m' && v=v') cases))
  with Not_found -> def

let mk_new_args c args1 lst = 
  let s1 = Array.length args1 in
  let args2 = Array.of_list lst in
  let s2 = Array.length args2 in
  Array.init (s1+s2-1) (
    fun i ->
      if i<c then args1.(i)
      else if i<(s1-1) then args1.(i+1)
      else args2.(i-(s1-1))
  )

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
    | ( _ , _ , GVar (m,v) , s )        -> 
        begin
          match Env.get_global_symbol m v with
            | Env.Def (te,_)            -> cbn_reduce ( 0 , [] , te , s )
            | Env.Decl (_,None)         -> config
            | Env.Decl (_,Some (i,g))   -> 
                ( match split_stack i s with
                    | None                -> config
                    | Some (s1,s2)        ->
                        ( match rewrite (Array.of_list s1) g with
                            | None              -> config
                            | Some (k,e,t)      -> cbn_reduce ( k , e , t , s2 ) 
                        )
                ) 
        end

and rewrite (args:cbn_state array) (g:gdt) : (int*(term Lazy.t) list*term) option = 
  match g with
    | Leaf te                     -> Some ( Array.length args , List.map (fun a -> lazy (cbn_term_of_state a)) (Array.to_list args) , te ) 
    | Switch (i,cases,def)        -> 
        begin
          assert (i<Array.length args);
          match cbn_reduce (args.(i)) with
            | ( _ , _ , GVar (m,v) , s )  -> 
                ( match get_gdt m v cases def with
                    | None        -> None
                    | Some g      -> rewrite (mk_new_args i args s) g
                )
            | s                           -> None
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

(* Weak Normal Form *)          
let wnf (t:term) : term = cbn_term_of_state (cbn_reduce (0,[],t,[]))

(* *** Conversion Test *** *)

(* Syntactic equality *)                            
let term_eq (t1:term) (t2:term) : bool = t1 == t2 || t1=t2 
 
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
          | ( _ , _ , GVar (m,v) ) , ( _ , _ , GVar (m',v') )         -> ( m=m' && v=v' , None )
          | ( k , _ , DB n ) , ( k' , _ , DB n' )                     -> ( (*assert (k<=n && k'<=n') ;*) (n-k)=(n'-k') , None )
          | ( k , e , Lam (a,f) ) , ( k' , e' , Lam (a',f') )          
          | ( k , e , Pi  (a,f) ) , ( k' , e' , Pi  (a',f') )         -> 
              let x = lazy (DB (k+2)) in
              ( true , Some ( 
                ( (k,e,a,[]) , (k',e',a',[]) ) ,
                ( (k+1,x::e,f,[]) , (k'+1,x::e',f',[]) )
              ) )
          | ( _ , _ , _ ) , ( _ , _ , _ )                               -> ( false , None )
        in

          term_eq (cbn_term_of_state s1) (cbn_term_of_state s2) ||
          let (k,e,t,s)         = cbn_reduce s1 in
          let (k',e',t',s')     = cbn_reduce s2 in
    
(*      Global.print "\n -------- CONV -------- \n";
      Global.print " ---> S1\n";
      Error.dump_state s1 ; 
      Global.print " ---> HNF(S1)\n";
      Error.dump_state (k,e,t,s) ;
      Global.print " --- S2\n";
      Error.dump_state s2 ;
      Global.print " --- HNF(S2)\n";
      Error.dump_state (k',e',t',s') ; *)

            match aux ( (k,e,t) , (k',e',t') ) with
              | true , None             -> state_conv (add_to_list lst s s')
              | true , Some (x,y)       -> state_conv (add_to_list (x::y::lst) s s')
              | false , _               -> false 
      end

let are_convertible t1 t2 =
  state_conv [ ((0,[],t1,[]),(0,[],t2,[])) ]

(* ----------------- *)

let rec decompose_state (rw:int) (lst:(int*term) list) : (cbn_state*cbn_state) list -> (int*term) list = function
  | []                  -> lst
  | (s1,s2)::states     ->
      begin
        if term_eq (cbn_term_of_state s1) (cbn_term_of_state s2) then decompose_state rw lst states
        else
          match ( cbn_reduce s1 , cbn_reduce s2 ) with
          (* Base Cases*)
            | ( _ , _ , Type , s ) , ( _ , _ , Type , s' )
            | ( _ , _ , Kind , s ) , ( _ , _ , Kind , s')                               -> decompose_state rw lst (add_to_list states s s')
            | ( _ , _ , GVar (m,v) , s ) , ( _ , _ , GVar (m',v') , s' )                -> if (m=m' && v=v') then decompose_state rw lst (add_to_list states s s') else assert false (*FIXME*)
            | ( k , _ , DB n , s ) , ( k' , _ , DB n' , s' ) when ((n-k)=(n'-k'))       -> decompose_state rw lst (add_to_list states s s')
            (*Composed Cases*)
            | ( k , e , Lam (a,f) , s ) , ( k' , e' , Lam (a',f') , s')          
            | ( k , e , Pi  (a,f) , s ) , ( k' , e' , Pi  (a',f') , s') -> 
                let x = lazy (DB (k+rw+2)) in (*FIXME*)
                let c1 = ( (k,e,a,[]) , (k',e',a',[]) ) in
                let c2 = ( (k+1,x::e,f,[]) , (k'+1,x::e',f',[]) ) in
                  decompose_state rw lst (add_to_list (c1::c2::states) s s')
            (* Unification *)
            | ( k , _ , DB n , [] ) , st when ( n-k <= rw )                             -> decompose_state rw ((n-k,cbn_term_of_state st)::lst) states
            | st , ( k , _ , DB n , [] ) when ( n-k <= rw )                             -> decompose_state rw ((n-k,cbn_term_of_state st)::lst) states
            (* Ignored Cases *)
            | ( k , _ , DB n , _ ) , _   when ( n-k <= rw )                             -> decompose_state rw lst states
            | _ , ( k , _ , DB n , _ )   when ( n-k <= rw )                             -> decompose_state rw lst states
 
            (*Not Unifiable*)
            | ( _ , _ , _ , _ ) , ( _ , _ , _ , _ )                     -> assert false (*FIXME*)
      end

let decompose (rw:int) (lst:(int*term) list) (t1:term) (t2:term) : (int*term) list = 
  decompose_state rw lst [((0,[],t1,[]),(0,[],t2,[]))]
