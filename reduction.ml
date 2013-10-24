
open Types

let estring = Global.hstring ""
let gvar_eq (m,v) (m',v') = ( m==m' || m=m' ) && ( v==v' || v=v' )

(* *** REDUCTION *** *)

type cbn_state = int (*size of context *) * (term Lazy.t) list (*context*) * term (*term to reduce*) * cbn_state list (*stack*)

let dump_state (k,e,t,s) =
  Global.eprint ("k = "^string_of_int k^"\n");
  Global.eprint ("t = "^Error.string_of_term t^"\n");
  Global.eprint "e = [";
  List.iter (fun u -> Global.eprint (" ("^Error.string_of_term (Lazy.force u)^")")) e ;
  Global.eprint " ]\ns = [";
  List.iter (fun (_,_,u,_) -> Global.eprint (" {{ "^Error.string_of_term u^" }}")) s ;
  Global.eprint " ]\n"

let rec cbn_term_of_state (k,e,t,s:cbn_state) : term =
  let t = ( if k = 0 then t else Subst.psubst_l (k,e) 0 t ) in
    if s = [] then t 
    else App ( t::(List.map cbn_term_of_state s) )
           
let rec split_stack (i:int) : cbn_state list -> (cbn_state list*cbn_state list) option = function 
  | l  when i=0 -> Some ([],l)
  | []          -> None
  | x::l        -> 
    ( match split_stack (i-1) l with
      | None            -> None
      | Some (s1,s2)    -> Some (x::s1,s2) )

let safe_find (m:string) (v:string) (cases:((string*string)*gdt) list) : gdt option =
  try Some (snd (List.find (fun ((m',v'),_) -> gvar_eq (m,v) (m',v')) cases))
  with Not_found -> None

let rec remove c lst =
  match lst with
    | []        -> assert false
    | x::lst'   -> 
        if c==0 then lst'
        else x::(remove (c-1) lst')

let rec cbn_reduce (config:cbn_state) : cbn_state = 
  match config with
    (* Weak normal terms *)
    | ( _ , _ , Type , _ )              -> config
    | ( _ , _ , Kind , _ )              -> config
    | ( _ , _ , Meta _ , _ )            -> config
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
          cbn_reduce ( k , e , he , tl' @ s ) (*FIXME @*)
    (* Global variable*)
    | ( _ , _ , GVar (m,_) , _ ) when m==estring        -> config (*FIXME*)
    | ( _ , _ , GVar (m,v) , s )        -> 
        begin
          match Env.get_global_symbol dloc m v with
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
    | Leaf te                     -> Some ( List.length args (*FIXME length*) , List.map (fun a -> lazy (cbn_term_of_state a)) args , te ) 
    | Switch (i,cases,def)        -> 
        begin
          (* assert (i<Array.length args); *)
          match cbn_reduce (List.nth args i) with
            | ( _ , _ , GVar (m,v) , s )  -> 
                ( match safe_find m v cases , def with
                    | Some g , _        -> rewrite ((remove i args)@s) g (*FIXME @*)
                    | None , Some g     -> rewrite args g
                    | _ , _             -> None )
            | ( _ , _ , _ , s ) -> 
                (match def with
                   | Some g     -> rewrite args g
                   | None       -> None )
        end

(* Weak Normal Form *)          
let wnf (t:term) : term = cbn_term_of_state ( cbn_reduce ( 0 , [] , t , [] ) ) 

let rec cbn_term_of_state2 (k,e,t,s:cbn_state) : term =
  let t = ( if k = 0 then t else Subst.psubst_l (k,e) 0 t ) in
    if s = [] then t 
    else App ( t::(List.map (fun st -> cbn_term_of_state2 (cbn_reduce st)) s ))

(* Head Normal Form *)          
let hnf (t:term) : term = cbn_term_of_state2 (cbn_reduce (0,[],t,[])) 

(* *** CONVERSION *** *)

(* Syntactic equality *)                            
let term_eq (t1:term) (t2:term) : bool = t1 == t2 || t1=t2 
 
let rec add_to_list lst s s' =
  match s,s' with
    | [] , []           -> lst
    | x::s1 , y::s2      -> add_to_list ((x,y)::lst) s1 s2
    | _ ,_              -> assert false

let cpt = ref 0
let fvar _ =
  let n = !cpt in
    incr cpt ;
    lazy (GVar (estring,Global.hstring (string_of_int n)))

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
                | ( _ , _ , GVar (m,v) , s ) , ( _ , _ , GVar (m',v') , s' )      -> ( gvar_eq (m,v) (m',v') && state_conv (add_to_list lst s s') ) 
                | ( k , e , Lam (a,f) , s ) , ( k' , e' , Lam (a',f') , s' )          
                | ( k , e , Pi  (a,f) , s ) , ( k' , e' , Pi  (a',f') , s' )      -> 
                    let arg = fvar () in
                    let x = ( (k,e,a,[]) , (k',e',a',[]) ) in
                    let y = ( (k+1,arg::e,f,[]) , (k'+1,arg::e',f',[]) ) in
                      state_conv (add_to_list (x::y::lst) s s')
                | ( _ , _ , Meta _ , _ ) , _ | _ , ( _ , _ , Meta _ , _ )          -> assert false
                | ( _ , _ , _ , _ ) , ( _ , _ , _ , _ )                            -> false
      end

let are_convertible t1 t2 =
  state_conv [ ( (0,[],t1,[]) , (0,[],t2,[]) ) ]

(* *** UNIFICATION *** *)

let rec decompose (sub:(int*term) list) : (cbn_state*cbn_state) list -> ((int*term) list) option = function
  | []                  -> Some sub
  | (s1,s2)::lst        ->
      begin
        let t1 = cbn_term_of_state s1 in
        let t2 = cbn_term_of_state s2 in
          if term_eq t1 t2 then 
            decompose sub lst
          else
            let s1' = cbn_reduce s1 in
            let s2' = cbn_reduce s2 in
              match s1',s2' with 
                (* Base Cases*)
                | ( _ , _ , Kind , s ) , ( _ , _ , Kind , s' )                          -> decompose sub (add_to_list lst s s')
                | ( _ , _ , Type , s ) , ( _ , _ , Type , s' )                          -> decompose sub (add_to_list lst s s')
                | ( _ , _ , GVar (m,v) , s ) , ( _ , _ , GVar (m',v') , s' )            -> if gvar_eq (m,v) (m',v') then decompose sub (add_to_list lst s s') else None 
                | ( k , _ , DB n , s ) , ( k' , _ , DB n' , s' ) (* (n<k && n'<k') *)   -> if (n-k)=(n'-k')  then decompose sub (add_to_list lst s s') else None
                (*Composed Cases*)
                | ( k , e , Lam (a,f) , s ) , ( k' , e' , Lam (a',f') , s' )          
                | ( k , e , Pi  (a,f) , s ) , ( k' , e' , Pi  (a',f') , s' )            -> 
                    let arg = fvar () in
                    let x = ( (k,e,a,[]) , (k',e',a',[]) ) in
                    let y = ( (k+1,arg::e,f,[]) , (k'+1,arg::e',f',[]) ) in
                      decompose sub (add_to_list (x::y::lst) s s')
                (* Unification *)
                | ( ( _ , _ , Meta n , [] ) , st )
                | ( st , ( _ , _ , Meta n , [] ) )                                      -> decompose  ((n,cbn_term_of_state st)::sub) lst
                (* Ignored Cases *)
                | ( _ , _ , Meta n , _ ) , _ 
                | _ , ( _ , _ , Meta n , _ )                                            -> decompose sub lst
                (*Not Unifiable*)
                | ( _ , _ , _ , _ ) , ( _ , _ , _ , _ )                                 -> None
      end

let decompose_eq t1 t2 = decompose [] [ (0,[],t1,[]),(0,[],t2,[]) ]      
