open Types

type cbn_state =
    term Lazy.t LList.t         (*context*)
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

let rec cbn_term_of_state (ctx,t,s:cbn_state) : term =
  let t = ( if LList.is_empty ctx then t else Subst.psubst_l ctx 0 t ) in
    match s with
      | [] -> t | a::lst ->
          mk_App t (cbn_term_of_state a) (List.map cbn_term_of_state lst)

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

let rec add_to_list lst s s' =
  match s,s' with
    | [] , []           -> Some lst
    | x::s1 , y::s2     -> add_to_list ((x,y)::lst) s1 s2
    | _ ,_              -> None

let rec cbn_reduce (config:cbn_state) : cbn_state =
  match config with
    (* Weak normal terms *)
    | ( _ ,  Type _ , _ )
    | ( _ ,  Kind , _ )
    | ( _ ,  Pi _ , _ )
    | ( _ ,  Lam _ , [] ) -> config
    | ({LList.len=k} , DB (_,_,n) , _ ) when (n>=k) -> config
    (* Bound variable (to be substitute) *)
    | ( ctx , DB (_,_,n) , s ) (*when n<k*) ->
        cbn_reduce ( LList.nil, Lazy.force (LList.nth ctx n) , s )
    (* Beta redex *)
    | ( ctx , Lam (_,_,_,t) , p::s ) ->
        cbn_reduce ( LList.cons (lazy (cbn_term_of_state p)) ctx , t , s )
    (* Application *)
    | ( ctx , App (f,a,lst) , s ) ->
        (* rev_map + rev_append to avoid map + append*)
        let tl' = List.rev_map ( fun t -> (ctx,t,[]) ) (a::lst) in
          cbn_reduce ( ctx , f , List.rev_append tl' s )
    (* Global variable*)
    | ( _ , Const (_,m,_), _ ) when m==empty -> config
    | ( _ , Const (_,m,v) , s )              ->
        begin
          match Env.get_infos dloc m v with
            | Def (te,_)        -> cbn_reduce ( LList.nil, te , s )
            | Decl _            -> config
            | Decl_rw (_,_,i,g) ->
                ( match split_stack i s with
                    | None                -> config
                    | Some (s1,s2)        ->
                        ( match rewrite (LList.make ~len:i s1) g with
                            | None              -> config
                            | Some (ctx,t)      -> cbn_reduce ( ctx , t , s2 )
                        )
                )
        end
    | ( _ , Meta _ , _ )                    -> assert false

and rewrite (args:cbn_state LList.t) (g:dtree) =
  (* assert ( nargs = List.lenght args ); *)
  match g with
    | Switch (i,cases,def)      ->
        begin
          (* assert (i<Array.length args); *)
          match cbn_reduce (LList.nth args i) with
            | ( _  , Const (_,m,v) , s )  ->
                ( match safe_find m v cases , def with
                    | Some g , _        ->
                        rewrite (LList.append_l (LList.remove i args) s) g
                    | None , Some g     -> rewrite args g
                    | _ , _             -> None )
            | ( _ , _ , s ) ->
                (match def with
                   | Some g     -> rewrite args g
                   | None       -> None )
        end
    | Test ([],te,def)          ->
        let ctx = LList.map (fun a -> lazy (cbn_term_of_state a)) args in
        Some ( ctx, te )
    | Test (lst,te,def)         ->
        begin
          let ctx = LList.map (fun st -> lazy (cbn_term_of_state st)) args in
          let conv_tests =
            List.map (fun (t1,t2) -> ( (ctx,t1,[]) , (ctx,t2,[]) )) lst in
            if state_conv conv_tests then
              let ctx = LList.map (fun a -> lazy (cbn_term_of_state a)) args in
              Some (ctx, te)
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
                | ( _ , Kind , s ) , ( _ , Kind , s' )
                | ( _ , Type _ , s ) , ( _ , Type _ , s' ) ->
                    (* assert ( List.length s == 0 && List.length s' == 0 ) *)
                    state_conv lst
                | ( {LList.len=k} , DB (_,_,n) , s ) , ({LList.len=k'} , DB (_,_,n') , s' ) ->
                    ( (*assert (k<=n && k'<=n') ;*) (n-k)=(n'-k') &&
                      match (add_to_list lst s s') with
                        | None          -> false
                        | Some lst'     -> state_conv lst'
                    )
                | ( _ , Const (_,m,v) , s ) , ( _ , Const (_,m',v') ,s' ) ->
                    ( ident_eq v v' && ident_eq m m' &&
                      match (add_to_list lst s s') with
                        | None          -> false
                        | Some lst'     -> state_conv lst'
                    )
                | ( ctx , Lam (_,_,a,f) , s ) , ( ctx' , Lam (_,_,a',f') , s' )
                | ( ctx , Pi  (_,_,a,f) , s ) , ( ctx' , Pi  (_,_,a',f') , s' ) ->
                    let arg = Lazy.lazy_from_val (mk_Unique ()) in
                    let x = ( (ctx,a,[]) , (ctx',a',[]) ) in
                    let y = ( (LList.cons arg ctx,f,[]) , (LList.cons arg ctx',f',[]) ) in
                      ( match add_to_list (x::y::lst) s s' with
                          | None        -> false
                          | Some lst'   -> state_conv lst' )
                | ( _ , Meta _ , _ ) , _
                | _ , ( _ , Meta _ , _ )                    -> assert false
                | ( _ , _ , _ ) , ( _ , _ , _ )         -> false
      end

(* Weak Normal Form *)
let whnf t = cbn_term_of_state ( cbn_reduce ( LList.nil , t , [] ) )

(* Head Normal Form *)
let rec hnf t =
  match whnf t with
    | Kind | Const _ | DB _ | Type _ | Pi (_,_,_,_) | Lam (_,_,_,_) as t' -> t'
    | App (f,a,lst) -> mk_App (hnf f) (hnf a) (List.map hnf lst)
    | Meta _  -> assert false

(* Convertibility Test *)
let are_convertible t1 t2 = state_conv [ ( (LList.nil,t1,[]) , (LList.nil,t2,[]) ) ]

(* Strong Normal Form *)
let rec snf (t:term) : term =
  match whnf t with
    | Kind | Const _
    | DB _ | Type _ as t' -> t'
    | App (f,a,lst)     -> mk_App (snf f) (snf a) (List.map snf lst)
    | Pi (_,x,a,b)        -> mk_Pi dloc x (snf a) (snf b)
    | Lam (_,x,a,b)       -> mk_Lam dloc x (snf a) (snf b)
    | Meta _            -> assert false

(* One-Step Reduction *)
let rec state_one_step = function
  (* Weak normal terms *)
  | ( _ , Type _ , s )
  | ( _ , Kind , s )
  | ( _ , Pi _ , s )                      -> None
  | ( _ , Lam _ , [] )                    -> None
  | ( {LList.len=k} , DB (_,_,n) , _ ) when (n>=k)      -> None
  (* Bound variable (to be substitute) *)
  | ( ctx, DB (_,_,n) , s ) (*when n<k*)     ->
      Some ( LList.nil , Lazy.force (LList.nth ctx n) , s )
  (* Beta redex *)
  | ( ctx , Lam (_,_,_,t) , p::s )            ->
      Some ( LList.cons (lazy (cbn_term_of_state p)) ctx , t , s )
  (* Application *)
  | ( ctx , App (f,a,args) , s )              ->
      let tl' = List.map ( fun t -> (ctx,t,[]) ) (a::args) in
        state_one_step ( ctx, f , tl' @ s )
  (* Global variable*)
  | ( _ , Const (_,m,_), _ ) when m==empty  -> None
  | ( _ , Const (_,m,v) , s )               ->
      begin
        match Env.get_infos dloc m v with
          | Def (te,_)          -> Some ( LList.nil , te , s )
          | Decl _              -> None
          | Decl_rw (_,_,i,g)   ->
              ( match split_stack i s with
                  | None                -> None
                  | Some (s1,s2)        ->
                      ( match rewrite (LList.make ~len:i s1) g with
                          | None              -> None
                          | Some (ctx,t)      -> Some ( ctx , t , s2 )
                      )
              )
      end
  | ( _ , Meta _ , _ )                    -> assert false

let one_step t =
  match state_one_step (LList.nil,t,[]) with
    | None      -> None
    | Some st   -> Some ( cbn_term_of_state st )
