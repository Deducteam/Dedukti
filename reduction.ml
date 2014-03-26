open Types

(* *** REDUCTION *** *)

type cbn_state =
    int                         (*size of context *)
    * (term Lazy.t) list        (*context*)
    * term                      (*term to reduce*)
    * cbn_state list            (*stack*)

(*
 let dump_state (k,e,t,s) =
 Global.eprint ("k = "^string_of_int k^"\n");
 Global.eprint ("t = "^ Pp.string_of_term t^"\n");
 Global.eprint "e = [";
 List.iter (fun u -> Global.eprint (" ("^ Pp.string_of_term (Lazy.force u)^")")) e ;
 Global.eprint " ]\ns = [";
 List.iter (fun (_,_,u,_) -> Global.eprint (" {{ "^ Pp.string_of_term u^" }}")) s ;
 Global.eprint " ]\n"
 *)

let rec cbn_term_of_state (k,e,t,s:cbn_state) : term =
  let t = ( if k = 0 then t else Subst.psubst_l (k,e) 0 t ) in
    if s = [] then t
    else mk_App ( t::(List.map cbn_term_of_state s) )

let rec split_stack i = function
  | l  when i=0 -> Some ([],l)
  | []          -> None
  | x::l        -> ( match split_stack (i-1) l with
                       | None            -> None
                       | Some (s1,s2)    -> Some (x::s1,s2) )

let rec safe_find m v = function
  | []                  -> None
  | (_,m',v',tr)::tl       ->
      if ident_eq v v' && ident_eq m m' then Some tr
      else safe_find m v tl

let rec remove c lst =
  match lst with
    | []        -> assert false
    | x::lst'   -> if c==0 then lst' else x::(remove (c-1) lst')

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
    | ( _ , _ , Lam _ , [] )                  -> config
    | ( k , _ , DB (_,n) , _ ) when (n>=k)    -> config
    (* Bound variable (to be substitute) *)
    | ( k , e , DB (_,n) , s ) (*when n<k*)   ->
        cbn_reduce ( 0 , [] , Lazy.force (List.nth e n) , s )
    (* Beta redex *)
    | ( k , e , Lam (_,_,t) , p::s )          ->
        cbn_reduce ( k+1 , (lazy (cbn_term_of_state p))::e , t , s )
    (* Application *)
    | ( _ , _ , App ([]|[_]) , _ )            -> assert false
    | ( k , e , App (he::tl) , s )      ->
        let tl' = List.map ( fun t -> (k,e,t,[]) ) tl in
          cbn_reduce ( k , e , he , tl' @ s )
    (* Global variable*)
    | ( _ , _ , Const (m,_), _ ) when m==empty -> config
    | ( _ , _ , Const (m,v) , s )              ->
        begin
          match Env.get_infos dloc m v with
            | Def (te,_)        -> cbn_reduce ( 0 , [] , te , s )
            | Decl _            -> config
            | Decl_rw (_,_,i,g) ->
                ( match split_stack i s with
                    | None                -> config
                    | Some (s1,s2)        ->
                        ( match rewrite i s1 g with
                            | None              -> config
                            | Some (k,e,t)      -> cbn_reduce ( k , e , t , s2 )
                        )
                )
        end
    | ( _ , _ , Meta _ , _ )                    -> assert false

and rewrite (nargs:int) (args:cbn_state list) (g:dtree) =
  (* assert ( nargs = List.lenght args ); *)
  match g with
    | Switch (i,cases,def)      ->
        begin
          (* assert (i<Array.length args); *)
          match cbn_reduce (List.nth args i) with
            | ( _ , _ , Const (m,v) , s )  ->
                ( match safe_find m v cases , def with
                    | Some g , _        ->
                        rewrite (nargs-1+(List.length s)) ((remove i args)@s) g
                    | None , Some g     -> rewrite nargs args g
                    | _ , _             -> None )
            | ( _ , _ , _ , s ) ->
                (match def with
                   | Some g     -> rewrite nargs args g
                   | None       -> None )
        end
    | Test ([],te,def)          ->
        Some ( nargs  ,
               List.map (fun a -> lazy (cbn_term_of_state a)) args , te )
    | Test (lst,te,def)         ->
        begin
          let ctx = List.map (fun st -> lazy (cbn_term_of_state st)) args in
          let conv_tests =
            List.map (fun (t1,t2) -> ( (nargs,ctx,t1,[]) , (nargs,ctx,t2,[]) )) lst in
            if state_conv conv_tests then
              Some (nargs, List.map (fun a -> lazy (cbn_term_of_state a)) args, te)
            else
              match def with
                | None    -> None
                | Some g  -> rewrite nargs args g
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
                | ( _ , _ , Type , s ) , ( _ , _ , Type , s' ) ->
                    (* assert ( List.length s == 0 && List.length s' == 0 ) *)
                    state_conv lst
                | ( k , _ , DB (_,n) , s ) , ( k' , _ , DB (_,n') , s' ) ->
                    ( (*assert (k<=n && k'<=n') ;*) (n-k)=(n'-k') &&
                      match (add_to_list lst s s') with
                        | None          -> false
                        | Some lst'     -> state_conv lst'
                    )
                | ( _ , _ , Const (m,v) , s ) , ( _ , _ , Const (m',v') ,s' ) ->
                    ( ident_eq v v' && ident_eq m m' &&
                      match (add_to_list lst s s') with
                        | None          -> false
                        | Some lst'     -> state_conv lst'
                    )
                | ( k , e , Lam (_,a,f) , s ) , ( k' , e' , Lam (_,a',f') , s' )
                | ( k , e , Pi  (_,a,f) , s ) , ( k' , e' , Pi  (_,a',f') , s' ) ->
                    let arg = Lazy.lazy_from_val (mk_Unique ()) in
                    let x = ( (k,e,a,[]) , (k',e',a',[]) ) in
                    let y = ( (k+1,arg::e,f,[]) , (k'+1,arg::e',f',[]) ) in
                      ( match add_to_list (x::y::lst) s s' with
                          | None        -> false
                          | Some lst'   -> state_conv lst' )
                | ( _ , _ , Meta _ , _ ) , _
                | _ , ( _ , _ , Meta _ , _ )                    -> assert false
                | ( _ , _ , _ , _ ) , ( _ , _ , _ , _ )         -> false
      end

(* Weak Normal Form *)
let whnf t = cbn_term_of_state ( cbn_reduce ( 0 , [] , t , [] ) )

(* Head Normal Form *)
let rec hnf t =
  match whnf t with
    | Kind | Const _ | DB _ | Type | Pi (_,_,_) | Lam (_,_,_) as t' -> t'
    | App lst -> mk_App (List.map hnf lst)
    | Meta _  -> assert false

(* Convertibility Test *)
let are_convertible t1 t2 = state_conv [ ( (0,[],t1,[]) , (0,[],t2,[]) ) ]

(* Strong Normal Form *)
let rec snf (t:term) : term =
  match whnf t with
    | Kind | Const _
    | DB _ | Type as t' -> t'
    | App lst           -> mk_App (List.map snf lst)
    | Pi (x,a,b)        -> mk_Pi x (snf a) (snf b)
    | Lam (x,a,b)       -> mk_Lam x (snf a) (snf b)
    | Meta _            -> assert false

(* ************** Bounded reduction for (untyped) terms with meta *************** *)

let rec bounded_cbn_reduce cpt (config:cbn_state) : cbn_state option =
  if cpt < 1 then None else
    match config with
      (* Weak normal terms *)
      | ( _ , _ , Meta _ , _ )
      | ( _ , _ , Type , _ )
      | ( _ , _ , Kind , _ )
      | ( _ , _ , Pi _ , _ )
      | ( _ , _ , Lam _ , [] )                  -> Some config
      | ( k , _ , DB (_,n) , _ ) when (n>=k)    -> Some config
      (* Bound variable (to be substitute) *)
      | ( k , e , DB (_,n) , s ) (*when n<k*)   ->
          bounded_cbn_reduce (cpt-1) ( 0 , [] , Lazy.force (List.nth e n) , s )
      (* Beta redex *)
      | ( k , e , Lam (_,_,t) , p::s )          ->
          bounded_cbn_reduce (cpt-1) (k+1, (lazy (cbn_term_of_state p))::e, t, s )
      (* Application *)
      | ( _ , _ , App ([]|[_]) , _ )            -> assert false
      | ( k , e , App (he::tl) , s )      ->
          let tl' = List.map ( fun t -> (k,e,t,[]) ) tl in
            bounded_cbn_reduce (cpt-1) ( k , e , he , tl' @ s )
      (* Global variable*)
      | ( _ , _ , Const (m,_), _ ) when m==empty -> Some config
      | ( _ , _ , Const (m,v) , s )              ->
          begin
            match Env.get_infos dloc m v with
              | Def (te,_)     -> bounded_cbn_reduce (cpt-1) ( 0 , [] , te , s )
              | Decl _          -> Some config
              | Decl_rw (_,_,i,g) ->
                  ( match split_stack i s with
                      | None                -> Some config
                      | Some (s1,s2)        ->
                          ( match bounded_rewrite (cpt-1) i s1 g with
                              | None2             -> Some config
                              | DontKnow          -> None
                              | Some2 (k,e,t)     ->
                                  bounded_cbn_reduce (cpt-1) ( k , e , t , s2 )
                          )
                  )
          end

and bounded_rewrite cpt (nargs:int) (args:cbn_state list) (g:dtree) =
  (* assert ( nargs = List.lenght args ); *)
  if cpt < 1 then DontKnow else
    match g with
      | Switch (i,cases,def)      ->
          begin
            (* assert (i<Array.length args); *)
            match bounded_cbn_reduce (cpt-1) (List.nth args i) with
              | Some ( _ , _ , Const (m,v) , s )  ->
                  ( match safe_find m v cases , def with
                      | Some g , _        ->
                          bounded_rewrite (cpt-1) (nargs-1+(List.length s))
                            ((remove i args)@s) g
                      | None , Some g     ->
                          bounded_rewrite (cpt-1) nargs args g
                      | _ , _             -> None2 )
              | Some ( _ , _ , _ , s ) ->
                  (match def with
                     | Some g     -> bounded_rewrite (cpt-1) nargs args g
                     | None       -> None2 )
              | None -> DontKnow
          end
      | Test ([],te,def)          ->
          Some2 ( List.length args  ,
                  List.map (fun a -> lazy (cbn_term_of_state a)) args , te )
      | Test (lst,te,def)         ->
          begin
            let ctx = List.map (fun st -> lazy (cbn_term_of_state st)) args in
            let k = List.length ctx in
            let conv_tests =
              List.map ( fun (t1,t2) -> ( (k,ctx,t1,[]) , (k,ctx,t2,[]) ) ) lst in
              match bounded_state_conv (cpt-1) conv_tests with
                | Yes -> Some2 (k ,List.map (fun a -> lazy (cbn_term_of_state a)) args, te )
                | No  ->
                    ( match def with
                        | None    -> None2
                        | Some g  -> bounded_rewrite (cpt-1) nargs args g )
                | Maybe -> DontKnow
          end

and bounded_state_conv cpt = function
  | []                  -> Yes
  | (s1,s2)::lst        ->
      begin
        if cpt < 1 then Maybe else
          let t1 = cbn_term_of_state s1 in
          let t2 = cbn_term_of_state s2 in
            if term_eq t1 t2 then
              bounded_state_conv (cpt-1) lst
            else
              let s1' = bounded_cbn_reduce (cpt-1) s1 in
              let s2' = bounded_cbn_reduce (cpt-1) s2 in
                match s1', s2' with
                  | None, _ | _, None     -> Maybe
                  | Some s1', Some s2'    ->
                      begin
                        match s1',s2' with (*states are beta-delta head normal*)
                          | ( _ , _ , Kind , s ) , ( _ , _ , Kind , s' )
                          | ( _ , _ , Type , s ) , ( _ , _ , Type , s' )      ->
                              (* assert ( List.length s == 0 && List.length s' == 0 ) *)
                              bounded_state_conv (cpt-1) lst
                          | ( k , _ , DB (_,n) , s ) , ( k' , _ , DB (_,n') , s' )
                              (*assert (k<=n && k'<=n') ;*) when (n-k)=(n'-k')   ->
                              ( match (add_to_list lst s s') with
                                  | None          -> No
                                  | Some lst'     -> bounded_state_conv (cpt-1) lst' )
                          | ( _ , _ , Const (m,v) , s ) , ( _ , _ , Const (m',v') ,s' )
                                when ( ident_eq v v' && ident_eq m m' ) ->
                              ( match (add_to_list lst s s') with
                                  | None          -> No
                                  | Some lst'     -> bounded_state_conv (cpt-1) lst' )
                          | ( k , e , Lam (_,a,f) , s ) , ( k' , e' , Lam (_,a',f') , s' )
                          | ( k , e , Pi  (_,a,f) , s ) , ( k' , e' , Pi  (_,a',f') , s' ) ->
                              let arg = Lazy.lazy_from_val (mk_Unique ()) in
                              let x = ( (k,e,a,[]) , (k',e',a',[]) ) in
                              let y = ( (k+1,arg::e,f,[]) , (k'+1,arg::e',f',[]) ) in
                                ( match add_to_list (x::y::lst) s s' with
                                    | None        -> No
                                    | Some lst'   -> bounded_state_conv (cpt-1) lst' )
                          | ( _ , _ , Meta n , s ) , ( _ , _ , Meta n' , s' ) when n=n' ->
                              (  match (add_to_list lst s s') with
                                   | None          -> No
                                   | Some lst'     -> bounded_state_conv (cpt-1) lst'
                              )
                          | ( _ , _ , _ , _ ) , ( _ , _ , _ , _ ) -> No
                      end
      end

let bounded_are_convertible k t1 t2 : yes_no_maybe =
  bounded_state_conv k [ ( (0,[],t1,[]) , (0,[],t2,[]) ) ]

let bounded_whnf k t =
  match bounded_cbn_reduce k ( 0 , [] , t , [] ) with
    | None      -> None
    | Some s    -> Some ( cbn_term_of_state s )

(* One-Step Reduction *)

let rec state_one_step = function
  (* Weak normal terms *)
  | ( _ , _ , Type , s )
  | ( _ , _ , Kind , s )
  | ( _ , _ , Pi _ , s )                      -> None
  | ( _ , _ , Lam _ , [] )                    -> None
  | ( k , _ , DB (_,n) , _ ) when (n>=k)      -> None
  (* Bound variable (to be substitute) *)
  | ( k , e , DB (_,n) , s ) (*when n<k*)     ->
      Some ( 0 , [] , Lazy.force (List.nth e n) , s )
  (* Beta redex *)
  | ( k , e , Lam (_,_,t) , p::s )            ->
      Some ( k+1 , (lazy (cbn_term_of_state p))::e , t , s )
  (* Application *)
  | ( _ , _ , App ([]|[_]) , _ )              -> assert false
  | ( k , e , App (he::tl) , s )              ->
      let tl' = List.map ( fun t -> (k,e,t,[]) ) tl in
        state_one_step ( k , e , he , tl' @ s )
  (* Global variable*)
  | ( _ , _ , Const (m,_), _ ) when m==empty  -> None
  | ( _ , _ , Const (m,v) , s )               ->
      begin
        match Env.get_infos dloc m v with
          | Def (te,_)          -> Some ( 0 , [] , te , s )
          | Decl _              -> None
          | Decl_rw (_,_,i,g)   ->
              ( match split_stack i s with
                  | None                -> None
                  | Some (s1,s2)        ->
                      ( match rewrite i s1 g with
                          | None              -> None
                          | Some (k,e,t)      -> Some ( k , e , t , s2 )
                      )
              )
      end
  | ( _ , _ , Meta _ , _ )                    -> assert false

let one_step t =
  match state_one_step (0,[],t,[]) with
    | None      -> None
    | Some st   -> Some ( cbn_term_of_state st )
