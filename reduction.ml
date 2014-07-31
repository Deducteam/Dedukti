open Types

(* FIXME about tail recursion *)

type env = (term Lazy.t) list
type state = int (*size of env*) * env * term * stack and stack = state list

let rec split_stack i = function
  | l  when i=0 -> Some ([],l)
  | []          -> None
  | x::l        -> map_opt (fun (s1,s2) -> (x::s1,s2) ) (split_stack (i-1) l)

let rec term_of_state (k,e,t,s:state) : term =
  let t = ( if k = 0 then t else Subst.psubst_l (k,e) 0 t ) in
    match s with
      | [] -> t | a::lst ->
          mk_App t (term_of_state a) (List.map term_of_state lst)

let rec add_to_list lst (s:stack) (s':stack) =
  match s,s' with
    | [] , []           -> Some lst
    | x::s1 , y::s2     -> add_to_list ((x,y)::lst) s1 s2
    | _ ,_              -> None

 let dump_state (k,e,t,s) =
   Global.debug_no_loc 1 "[ k=%i | e=[...] | %a | [...] ]" k Pp.pp_term t

let dump_stack stk =
  Global.debug_no_loc 1 "DUMP STATE --->";
  List.iter dump_state stk ;
  Global.debug_no_loc 1 " <------ DUMP STATE"

let itg = hstring "?"

(* ********************* *)

exception NotUnifiable

let occur (k:int) (te:term) (x:int) : bool = true (*TODO*)
let rec lam (te:term) : term list -> term = function
  | [] -> te
  | ty::lst -> lam (mk_Lam dloc itg ty te) lst

let flexrigid (k:int) (ltyp:term list) (dbs:int list) (te:term) : term Lazy.t =
  if List.for_all (occur k te) dbs then Lazy.from_val (lam te ltyp)
  else raise NotUnifiable

let get_new_context (ltyp:term list) (lst:mtch_pb list) (stck:state list) : term Lazy.t list option =
  let k = List.length ltyp in
  try Some ( List.map ( fun (i,dbs) -> flexrigid k ltyp dbs (term_of_state (List.nth stck i)) ) lst )
  with NotUnifiable -> None

(* ********************* *)

let rec on_stack k e (s:stack) (args:term list) : stack (* args@s *) =
  match args with
    | [] -> s
    | t::args0 -> (k,e,t,[])::(on_stack k e s args0)

let rec beta_reduce (config:state) : state =
  match config with
    (* Weak heah beta normal terms *)
    | ( _ , _ , Type _ , _ ) | ( _ , _ , Kind , _ ) | ( _ , _ , Pi _ , _ )
    | ( _ , _ , Const _ , _ ) | ( _ , _ , Lam _ , [] ) -> config
    | ( k , _ , DB (_,_,n) , _ ) when (n>=k) -> config
    (* DeBruijn index: environement lookup *)
    | ( k , e , DB (_,_,n) , s ) (*when n<k*) ->
        beta_reduce ( 0 , [] , Lazy.force (List.nth e n) , s )
    (* Beta redex *)
    | ( k , e , Lam (_,_,_,t) , p::s ) ->
        beta_reduce ( k+1 , (lazy (term_of_state p))::e , t , s )
    (* Application: arguments go on the stack *)
    | ( k , e , App (f,a,args) , s ) ->
        beta_reduce ( k , e , f , on_stack k e s (a::args) )

(* ********************* *)

type find_case_ty =
  | FC_Lam of dtree*term*state
  | FC_Const of dtree*state list
  | FC_DB of dtree*state list
  | FC_None

let rec find_case (st:state) (cases:(case*dtree) list) : find_case_ty =
  match st, cases with
    | _, [] -> FC_None
    | ( k , e , Lam (_,_,ty,te) , _ ) , ( CLam , tr )::tl ->
        let ty2 = term_of_state (k,e,ty,[]) in
        let st2 = (k+1 ,(Lazy.lazy_from_val (mk_DB dloc empty 0))::e ,te ,[] ) in
        FC_Lam ( tr , ty2 , st2 )
    | ( _ , _ , Const (_,m,v) , s) , (CConst (nargs,m',v'),tr)::tl ->
        if ident_eq v v' && ident_eq m m' then
          ( assert (List.length s == nargs) ; FC_Const (tr,s) )
        else find_case st tl
    | ( _ , _ , DB (_,_,n) , s ) , (CDB (nargs,n'),tr)::tl ->
        if n==n' then ( assert (List.length s == nargs) ; FC_DB (tr,s) )
        else find_case st tl
    | _, _::tl -> find_case st tl

(* Resolve the matching v(args) = te:
* [v] is the variable to be matched
* [args] are 'bound' variable
* te is a term without free variable *)

let rec reduce (config0:state) : state = (*FIXME*)
  match beta_reduce config0 with
    | ( _ , _ , Const (_,m,v) , s ) as config ->
        begin
          match Env.get_infos dloc m v with
            | Decl _            -> config
            | Def (te,_)        -> reduce ( 0 , [] , te , s )
            | Decl_rw (_,_,i,g) ->
                begin
                 (* Global.debug_no_loc 1 "Trying to rewrite '%a'\n" Pp.pp_term (term_of_state config) ; *)
                ( match split_stack i s with
                    | None -> config
                    | Some (s1,s2) ->
                        ( match rewrite [] s1 g with
                            | None         -> config
                            | Some (k,e,t) -> reduce (k,e,t,s2)
                        )
                )
                end
        end
    | config -> config

and rewrite (ltyp:term list) (stck:stack) (g:dtree) : (int*env*term) option =
  (*dump_stack stck ;*)
  match g with
    | Switch (i,cases,def) ->
        begin
          assert (i<List.length stck);
          let arg_i = reduce (List.nth stck i) in
            match find_case arg_i cases with
              | FC_DB (g,s) | FC_Const (g,s) -> rewrite ltyp (stck@s) g
              | FC_Lam (g,ty,te) -> rewrite (ty::ltyp) (stck@[te]) g
              | FC_None -> bind_opt (rewrite ltyp stck) def
        end
    | Test (Syntactic _,tests,right,def) -> failwith "Not implemented" (*TODO*)
    | Test (MillerPattern mtch_pbs,tests,right,def) ->
        begin
          match get_new_context ltyp mtch_pbs stck with
            | None -> bind_opt (rewrite ltyp stck) def
            | Some ctx ->
                let n = List.length ctx in (*FIXME this is known in advance*)
                let conv_tests = List.rev_map (
                  fun (t1,t2) -> ( (n,ctx,t1,[]) , (n,ctx,t2,[]) )) tests
                in
                  if state_conv conv_tests then Some (n, ctx, right)
                  else bind_opt (rewrite ltyp stck) def
        end

and state_conv : (state*state) list -> bool = function
  | [] -> true
  | (s1,s2)::lst ->
      if term_eq (term_of_state s1) (term_of_state s2) then
        state_conv lst
      else
        match reduce s1, reduce s2 with (*states are beta-delta head normal*)
          | ( _ , _ , Kind , s ) , ( _ , _ , Kind , s' )
          | ( _ , _ , Type _ , s ) , ( _ , _ , Type _ , s' ) ->
              begin
                assert ( s = [] && s' = [] ) ;
                state_conv lst
              end
          | ( k , _ , DB (_,_,n) , s ) , ( k' , _ , DB (_,_,n') , s' )
                                           when (n-k)==(n'-k') ->
              begin
                match add_to_list lst s s' with
                  | None          -> false
                  | Some lst'     -> state_conv lst'
              end
          | ( _ , _ , Const (_,m,v) , s ) , ( _ , _ , Const (_,m',v') ,s' )
                                              when ident_eq v v' && ident_eq m m' ->
              begin
                match (add_to_list lst s s') with
                  | None          -> false
                  | Some lst'     -> state_conv lst'
              end
          | ( k , e , Lam (_,_,a,f) , s ) , ( k' , e' , Lam (_,_,a',f') , s' )
          | ( k , e , Pi  (_,_,a,f) , s ) , ( k' , e' , Pi  (_,_,a',f') , s' ) ->
              begin
                assert ( s = [] && s' = [] ) ;
                let arg = Lazy.lazy_from_val (mk_DB dloc itg 0) in
                let lst' = ( (k,e,a,[]) , (k',e',a',[]) ) ::
                           ( (k+1,arg::e,f,[]) , (k'+1,arg::e',f',[]) ) :: lst in
                  state_conv lst'
              end
          | ( _ , _ , _ , _ ) , ( _ , _ , _ , _ )         -> false

(* ********************* *)

(* Weak Normal Form *)
let whnf t = term_of_state ( reduce ( 0 , [] , t , [] ) )

(* Head Normal Form *)
let rec hnf t =
  match whnf t with
    | Kind | Const _ | DB _ | Type _ | Pi (_,_,_,_) | Lam (_,_,_,_) as t' -> t'
    | App (f,a,lst) -> mk_App (hnf f) (hnf a) (List.map hnf lst)

(* Convertibility Test *)
let are_convertible t1 t2 = state_conv [ ( (0,[],t1,[]) , (0,[],t2,[]) ) ]

(* Strong Normal Form *)
let rec snf (t:term) : term =
  match whnf t with
    | Kind | Const _
    | DB _ | Type _ as t' -> t'
    | App (f,a,lst)     -> mk_App (snf f) (snf a) (List.map snf lst)
    | Pi (_,x,a,b)        -> mk_Pi dloc x (snf a) (snf b)
    | Lam (_,x,a,b)       -> mk_Lam dloc x (snf a) (snf b)

(* One-Step Reduction *)
let rec state_one_step = function
  (* Weak normal terms *)
  | ( _ , _ , Type _ , s )
  | ( _ , _ , Kind , s )
  | ( _ , _ , Pi _ , s )                      -> None
  | ( _ , _ , Lam _ , [] )                    -> None
  | ( k , _ , DB (_,_,n) , _ ) when (n>=k)      -> None
  (* Bound variable (to be substitute) *)
  | ( k , e , DB (_,_,n) , s ) (*when n<k*)     ->
      Some ( 0 , [] , Lazy.force (List.nth e n) , s )
  (* Beta redex *)
  | ( k , e , Lam (_,_,_,t) , p::s )            ->
      Some ( k+1 , (lazy (term_of_state p))::e , t , s )
  (* Application *)
  | ( k , e , App (f,a,args) , s )              ->
      let tl' = List.map ( fun t -> (k,e,t,[]) ) (a::args) in
        state_one_step ( k , e , f , tl' @ s )
  (* Global variable*)
  | ( _ , _ , Const (_,m,_), _ ) when m==empty  -> None
  | ( _ , _ , Const (_,m,v) , s )               ->
      begin
        match Env.get_infos dloc m v with
          | Def (te,_)          -> Some ( 0 , [] , te , s )
          | Decl _              -> None
          | Decl_rw (_,_,i,g)   ->
              ( match split_stack i s with
                  | None                -> None
                  | Some (s1,s2)        ->
                      ( match rewrite [] s1 g with
                          | None              -> None
                          | Some (k,e,t)      -> Some ( k , e , t , s2 )
                      )
              )
      end

let one_step t =
  match state_one_step (0,[],t,[]) with
    | None      -> None
    | Some st   -> Some ( term_of_state st )
