
open Types

(* *** REDUCTION *** *)

type cbn_state =
    int                         (*size of context *)
    * (term Lazy.t) list        (*context*)
    * term                      (*term to reduce*)
    * cbn_state list            (*stack*)

let dump_state (k,e,t,s) =
  Global.eprint ("k = "^string_of_int k^"\n");
  Global.eprint ("t = "^ Pp.string_of_term t^"\n");
  Global.eprint "e = [";
  List.iter (fun u -> Global.eprint (" ("^ Pp.string_of_term (Lazy.force u)^")")) e ;
  Global.eprint " ]\ns = [";
  List.iter (fun (_,_,u,_) -> Global.eprint (" {{ "^ Pp.string_of_term u^" }}")) s ;
  Global.eprint " ]\n"

let rec cbn_term_of_state (k,e,t,s:cbn_state) : term =
  let t = ( if k = 0 then t else Subst.psubst_l (k,e) 0 t ) in
    if s = [] then t
    else mk_App ( t::(List.map cbn_term_of_state s) )

let rec split_stack (i:int) : cbn_state list -> (cbn_state list*cbn_state list) option = function
  | l  when i=0 -> Some ([],l)
  | []          -> None
  | x::l        ->
    ( match split_stack (i-1) l with
      | None            -> None
      | Some (s1,s2)    -> Some (x::s1,s2) )

let safe_find m v cases =
  try Some ( snd ( List.find (fun ((m',v'),_) -> ident_eq v v' && ident_eq m m') cases ) )
  with Not_found -> None

let rec remove c lst =
  match lst with
    | []        -> assert false
    | x::lst'   ->
        if c==0 then lst'
        else x::(remove (c-1) lst')

let rec add_to_list lst s s' =
  match s,s' with
    | [] , []           -> Some lst
    | x::s1 , y::s2     -> add_to_list ((x,y)::lst) s1 s2
    | _ ,_              -> None

let rec cbn_reduce (config:cbn_state) : cbn_state =
  match config with
    (* Weak normal terms *)
    | ( _ , _ , Type , _ )
    | ( _ , _ , Kind , _ )
    | ( _ , _ , Pi _ , _ )
    | ( _ , _ , Meta _ , _ )
    | ( _ , _ , Lam _ , [] )                  -> config
    | ( k , _ , DB (_,n) , _ ) when (n>=k)    -> config
    (* Bound variable (to be substitute) *)
    | ( k , e , DB (_,n) , s ) (*when n<k*)   -> cbn_reduce ( 0 , [] , Lazy.force (List.nth e n) , s )
    (* Beta redex *)
    | ( k , e , Lam (_,_,t) , p::s )          -> cbn_reduce ( k+1 , (lazy (cbn_term_of_state p))::e , t , s )
    (* Application *)
    | ( _ , _ , App ([]|[_]) , _ )            -> assert false
    | ( k , e , App (he::tl) , s )      ->
        let tl' = List.map ( fun t -> (k,e,t,[]) ) tl in
          cbn_reduce ( k , e , he , tl' @ s )
    (* Global variable*)
    | ( _ , _ , Const (m,_), _ ) when m==empty -> config
    | ( _ , _ , Const (m,v) , s )              ->
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
    | Switch (i,cases,def)      ->
        begin
          (* assert (i<Array.length args); *)
          match cbn_reduce (List.nth args i) with
            | ( _ , _ , Const (m,v) , s )  ->
                ( match safe_find m v cases , def with
                    | Some g , _        -> rewrite ((remove i args)@s) g
                    | None , Some g     -> rewrite args g
                    | _ , _             -> None )
            | ( _ , _ , _ , s ) ->
                (match def with
                   | Some g     -> rewrite args g
                   | None       -> None )
        end
    | Test ([],te,def)          ->
        Some ( List.length args (*FIXME*) , 
               List.map (fun a -> lazy (cbn_term_of_state a)) args , te )
    | Test (lst,te,def)         ->
        begin
          let ctx = List.map (fun st -> lazy (cbn_term_of_state st)) args in 
          let k = List.length ctx in (*FIXME*)
          let conv_tests = List.map ( fun (t1,t2) -> ( (k,ctx,t1,[]) , (k,ctx,t2,[]) ) ) lst in
          if state_conv conv_tests then
            Some ( k , List.map (fun a -> lazy (cbn_term_of_state a)) args , te )
          else
            match def with
              | None    -> None
              | Some g  -> rewrite args g
        end

and state_conv : (cbn_state*cbn_state) list -> bool = function
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
                | ( _ , _ , Kind , s ) , ( _ , _ , Kind , s' )
                | ( _ , _ , Type _ , s ) , ( _ , _ , Type _ , s' )              ->
                (* assert ( List.length s == 0 && List.length s' == 0 ) *) state_conv lst
                | ( k , _ , DB (_,n) , s ) , ( k' , _ , DB (_,n') , s' )        ->
                    ( (*assert (k<=n && k'<=n') ;*) (n-k)=(n'-k') &&
                      match (add_to_list lst s s') with
                        | None          -> false
                        | Some lst'     -> state_conv lst'
                    )
                | ( _ , _ , Const (m,v) , s ) , ( _ , _ , Const (m',v') ,s' )           ->
                    ( ident_eq v v' && ident_eq m m' &&
                      match (add_to_list lst s s') with
                        | None          -> false
                        | Some lst'     -> state_conv lst'
                    )
                | ( k , e , Lam (_,a,f) , s ) , ( k' , e' , Lam (_,a',f') , s' )
                | ( k , e , Pi  (_,a,f) , s ) , ( k' , e' , Pi  (_,a',f') , s' )        ->
                    let arg = Lazy.lazy_from_val (mk_Unique ()) in
                    let x = ( (k,e,a,[]) , (k',e',a',[]) ) in
                    let y = ( (k+1,arg::e,f,[]) , (k'+1,arg::e',f',[]) ) in
                      ( match add_to_list (x::y::lst) s s' with
                          | None        -> false
                          | Some lst'   -> state_conv lst' )
                | ( _ , _ , Meta _ , _ ) , _ | _ , ( _ , _ , Meta _ , _ )               -> assert false
                | ( _ , _ , _ , _ ) , ( _ , _ , _ , _ )                                 -> false
      end


(* Weak Normal Form *)
let wnf (t:term) : term = cbn_term_of_state ( cbn_reduce ( 0 , [] , t , [] ) )

let rec cbn_term_of_state2 (k,e,t,s:cbn_state) : term =
  let t = ( if k = 0 then t else Subst.psubst_l (k,e) 0 t ) in
    if s = [] then t
    else mk_App ( t::(List.map (fun st -> cbn_term_of_state2 (cbn_reduce st)) s ))

(* Head Normal Form *)
let hnf (t:term) : term = cbn_term_of_state2 (cbn_reduce (0,[],t,[]))

(* Strong Normal Form *)
let snf te = failwith "Not implemented (strong normalization)." (*TODO*)


let are_convertible t1 t2 =
  state_conv [ ( (0,[],t1,[]) , (0,[],t2,[]) ) ]

(* *** UNIFICATION *** *)

let rec decompose (sub:(int*term) list) : (cbn_state*cbn_state) list -> ((int*term) list) option =
  function
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
                | ( _ , _ , Kind , s ) , ( _ , _ , Kind , s' )
                | ( _ , _ , Type , s ) , ( _ , _ , Type , s' )                  ->
                (* assert ( List.length s == 0 && List.length s' == 0 ) *) decompose sub lst
                | ( _ , _ , Const (m,v) , s ) , ( _ , _ , Const (m',v') , s' )  ->
                    if ident_eq v v' && ident_eq m m' then
                      ( match (add_to_list lst s s') with
                        | None          ->      None
                        | Some lst'     -> decompose sub lst' )
                    else None
                | ( k , _ , DB (_,n) , s ) , ( k' , _ , DB (_,n') , s' ) (* (n<k && n'<k') *)   ->
                    if (n-k)=(n'-k') then
                      ( match (add_to_list lst s s') with
                          | None        -> None
                          | Some lst'   -> decompose sub lst' )
                    else None
                (*Composed Cases*)
                | ( k , e , Lam (_,a,f) , s ) , ( k' , e' , Lam (_,a',f') , s' )
                | ( k , e , Pi  (_,a,f) , s ) , ( k' , e' , Pi  (_,a',f') , s' )        ->
                    let arg = Lazy.lazy_from_val (mk_Unique ()) in
                    let x = ( (k,e,a,[]) , (k',e',a',[]) ) in
                    let y = ( (k+1,arg::e,f,[]) , (k'+1,arg::e',f',[]) ) in
                     ( match (add_to_list (x::y::lst) s s') with
                         | None         -> None
                         | Some lst'    -> decompose sub lst'
                     )
                | ( k , e , App l , s ) , ( k' , e' , App l' , s' )             ->
                    let ll1 = ( List.map (fun t -> (k,e,t,[]) ) l ) @ s in
                    let ll2 = ( List.map (fun t -> (k',e',t,[]) ) l' ) @ s' in
                      ( match (add_to_list lst ll1 ll2) with
                          | None         -> None
                          | Some lst'    -> decompose sub lst'
                      )

                (* Unification *)
                | ( ( _ , _ , Meta n , [] ) , st )
                | ( st , ( _ , _ , Meta n , [] ) )                              ->
                    decompose  ((n,cbn_term_of_state st)::sub) lst
                (* Ignored Cases *)
                | ( _ , _ , Meta n , _ ) , _
                | _ , ( _ , _ , Meta n , _ )                                        -> decompose sub lst
                (*Not Unifiable*)
                | ( _ , _ , _ , _ ) , ( _ , _ , _ , _ )                                 -> None
      end

let decompose_eq t1 t2 = decompose [] [ (0,[],t1,[]),(0,[],t2,[]) ]
