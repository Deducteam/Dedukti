open Types

type env = term Lazy.t LList.t

type state = {
  ctx:env;              (*context*)
  term : term;          (*term to reduce*)
  stack : stack;        (*stack*)
}
and stack = state list

let rec term_of_state {ctx;term;stack} : term =
  let t = ( if LList.is_empty ctx then term else Subst.psubst_l ctx 0 term ) in
    match stack with
      | [] -> t
      | a::lst -> mk_App t (term_of_state a) (List.map term_of_state lst)

let rec split_stack (i:int) : stack -> (stack*stack) option = function
  | l  when i=0 -> Some ([],l)
  | []          -> None
  | x::l        -> map_opt (fun (s1,s2) -> (x::s1,s2) ) (split_stack (i-1) l)

let rec safe_find m v = function
  | []                  -> None
  | (_,m',v',tr)::tl       ->
      if ident_eq v v' && ident_eq m m' then Some tr
      else safe_find m v tl

let rec add_to_list lst (s:stack) (s':stack) =
  match s,s' with
    | [] , []           -> Some lst
    | x::s1 , y::s2     -> add_to_list ((x,y)::lst) s1 s2
    | _ ,_              -> None

 let dump_state { ctx; term; stack } =
   Global.debug_no_loc 1 "[ e=[...] | %a | [...] ]" Pp.pp_term term

let dump_stack stk =
  Global.debug_no_loc 1 " ================ >";
  List.iter dump_state stk ;
  Global.debug_no_loc 1 " < ================"

(* ********************* *)

let get_context1 (lst:int LList.t) (stck:stack) : env =
  LList.map (fun i -> lazy (term_of_state (List.nth stck i) )) lst

exception NotUnifiable

let occur (te:term) (x:int) : bool =
  let rec aux k = function
  | Kind | Type _ | Const _ -> false
  | DB (_,_,n) -> (n-k) = x
  | App (f,a,args) -> List.exists (aux k) (f::a::args)
  | Lam (_,_,a,b) | Pi (_,_,a,b) -> ( aux k a || aux (k+1) b )
  in
    aux 0 te

let rec lam (te:term) : term list -> term = function
  | [] -> te
  | ty::lst -> lam (mk_Lam dloc qmark ty te) lst

let flexrigid (ltyp:term list) (dbs:int list) (te:term) : term Lazy.t =
  if List.for_all (occur te) dbs then Lazy.from_val (lam te ltyp)
  else raise NotUnifiable

let get_context2 (ltyp:term list) (pre_ctx:ctx_loc) (stck:stack) : env option =
  match pre_ctx with
    | Syntactic var_lst -> Some (get_context1 var_lst stck)
    | MillerPattern lst ->
        let aux (i,dbs) = flexrigid ltyp dbs (term_of_state (List.nth stck i)) in
          try Some (LList.map aux lst)
          with NotUnifiable -> None

(* ********************* *)

let rec beta_reduce : state -> state = function
    (* Weak heah beta normal terms *)
    | { term=Type _ }
    | { term=Kind }
    | { term=Const _ }
    | { term=Pi _ }
    | { term=Lam _; stack=[] } as config -> config
    | { ctx={ LList.len=k }; term=DB (_,_,n) } as config when (n>=k) -> config
    (* DeBruijn index: environment lookup *)
    | { ctx; term=DB (_,_,n); stack } (*when n<k*) ->
        beta_reduce { ctx=LList.nil; term=Lazy.force (LList.nth ctx n); stack }
    (* Beta redex *)
    | { ctx; term=Lam (_,_,_,t); stack=p::s } ->
        beta_reduce { ctx=LList.cons (lazy (term_of_state p)) ctx; term=t; stack=s }
    (* Application: arguments go on the stack *)
    | { ctx; term=App (f,a,lst); stack=s } ->
        (* rev_map + rev_append to avoid map + append*)
        let tl' = List.rev_map ( fun t -> {ctx;term=t;stack=[]} ) (a::lst) in
          beta_reduce { ctx; term=f; stack=List.rev_append tl' s }

(* ********************* *)

type find_case_ty =
  | FC_Lam of dtree*term*state
  | FC_Const of dtree*state list
  | FC_DB of dtree*state list
  | FC_None

let rec find_case (st:state) (cases:(case*dtree) list) : find_case_ty =
  match st, cases with
    | _, [] -> FC_None
    | { ctx; term=Lam (_,_,ty,te) } , ( CLam , tr )::tl ->
        let ty2 = term_of_state { ctx; term=ty; stack=[] } in
        let ctx2 = LList.cons (Lazy.lazy_from_val (mk_DB dloc qmark 0)) ctx in
          FC_Lam ( tr , ty2 , { ctx=ctx2; term=ty2; stack=[] } )
    | { term=Const (_,m,v); stack } , (CConst (nargs,m',v'),tr)::tl ->
        if ident_eq v v' && ident_eq m m' then
          ( assert (List.length stack == nargs);
            FC_Const (tr,stack) ) (*TODO delete assert *)
        else find_case st tl
    | { term=DB (_,_,n); stack } , (CDB (nargs,n'),tr)::tl ->
        if n==n' then (
          assert (List.length stack == nargs) ;
          FC_DB (tr,stack) ) (*TODO delete assert *)
        else find_case st tl
    | _, _::tl -> find_case st tl

let rec reduce (st:state) : state =
  match beta_reduce st with
    | { ctx; term=Const (_,m,v); stack } as config ->
        begin
          match Env.get_infos dloc m v with
            | Decl _            -> config
            | Def (term,_)        -> reduce { ctx=LList.nil; term; stack }
            | Decl_rw (_,_,i,g) ->
                begin
                  match split_stack i stack with
                    | None -> config
                    | Some (s1,s2) ->
                        ( match rewrite [] s1 g with
                            | None -> config
                            | Some (ctx,term) -> reduce { ctx; term; stack=s2 }
                        )
                end
        end
    | config -> config

(*TODO remplacer la stack par un array ? (on peut connaitre la taille max à l'avance).*)
and rewrite (ltyp:term list) (stack:stack) (g:dtree) : (env*term) option =
  let test ctx eqs =
    state_conv (List.rev_map (
      fun (t1,t2) -> ( { ctx; term=t1; stack=[] } , { ctx; term=t2; stack=[] } )
    ) eqs)
  in
    (*dump_stack stck ;*)
    match g with
      | Switch (i,cases,def) ->
          begin
            let arg_i = reduce (List.nth stack i) in
              match find_case arg_i cases with
                | FC_DB (g,s) | FC_Const (g,s) -> rewrite ltyp (stack@s) g
                | FC_Lam (g,ty,te) -> rewrite (ty::ltyp) (stack@[te]) g
                | FC_None -> bind_opt (rewrite ltyp stack) def
          end
      | Test (Syntactic var_lst,[],right,def) ->
          Some (get_context1 var_lst stack, right)
      | Test (Syntactic var_lst, eqs, right, def) ->
          let ctx = get_context1 var_lst stack in
            if test ctx eqs then Some (ctx, right)
            else bind_opt (rewrite ltyp stack) def
      | Test (pre_ctx, eqs, right, def) ->
          begin
            match get_context2 ltyp pre_ctx stack with
              | None -> bind_opt (rewrite ltyp stack) def
              | Some ctx ->
                  if test ctx eqs then Some (ctx, right)
                  else bind_opt (rewrite ltyp stack) def
          end

and state_conv : (state*state) list -> bool = function
  | [] -> true
  | (s1,s2)::lst ->
      if term_eq (term_of_state s1) (term_of_state s2) then
        state_conv lst
      else
        match reduce s1, reduce s2 with
          | { term=Kind; stack=s } , { term=Kind; stack=s' }
          | { term=Type _; stack=s } , { term=Type _; stack=s' } ->
              begin
                assert ( s = [] && s' = [] ) ;
                state_conv lst
              end
          | { ctx=e;  term=DB (_,_,n);  stack=s },
            { ctx=e'; term=DB (_,_,n'); stack=s' }
              when (n-e.LList.len)==(n'-e'.LList.len) ->
              begin
                match add_to_list lst s s' with
                  | None          -> false
                  | Some lst'     -> state_conv lst'
              end
          | { term=Const (_,m,v);   stack=s },
            { term=Const (_,m',v'); stack=s' } when ident_eq v v' && ident_eq m m' ->
              begin
                match (add_to_list lst s s') with
                  | None          -> false
                  | Some lst'     -> state_conv lst'
              end
          | { ctx=e;  term=Lam (_,_,a,b);   stack=s },
            { ctx=e'; term=Lam (_,_,a',b'); stack=s'}
          | { ctx=e;  term=Pi  (_,_,a,b);   stack=s },
            { ctx=e'; term=Pi  (_,_,a',b'); stack=s'} ->
              begin
                assert ( s = [] && s' = [] ) ;
                let arg = Lazy.lazy_from_val (mk_DB dloc qmark 0) in
                let lst' =
                  ( {ctx=e;term=a;stack=[]}, {ctx=e';term=a';stack=[]} ) ::
                  ( {ctx=LList.cons arg e;term=b;stack=[]},
                    {ctx=LList.cons arg e';term=b';stack=[]} ) :: lst in
                  state_conv lst'
              end
          | _, _ -> false

(* ********************* *)

(* Weak Normal Form *)
let whnf term = term_of_state ( reduce { ctx=LList.nil; term; stack=[] } )

(* Head Normal Form *)
let rec hnf t =
  match whnf t with
    | Kind | Const _ | DB _ | Type _ | Pi (_,_,_,_) | Lam (_,_,_,_) as t' -> t'
    | App (f,a,lst) -> mk_App (hnf f) (hnf a) (List.map hnf lst)

(* Convertibility Test *)
let are_convertible t1 t2 =
  state_conv [ ( {ctx=LList.nil;term=t1;stack=[]} , {ctx=LList.nil;term=t2;stack=[]} ) ]

(* Strong Normal Form *)
let rec snf (t:term) : term =
  match whnf t with
    | Kind | Const _
    | DB _ | Type _ as t' -> t'
    | App (f,a,lst)     -> mk_App (snf f) (snf a) (List.map snf lst)
    | Pi (_,x,a,b)        -> mk_Pi dloc x (snf a) (snf b)
    | Lam (_,x,a,b)       -> mk_Lam dloc x (snf a) (snf b)

(* One-Step Reduction *)
let rec state_one_step : state -> state option = function
    (* Weak heah beta normal terms *)
    | { term=Type _ }
    | { term=Kind }
    | { term=Pi _ }
    | { term=Lam _; stack=[] } -> None
    | { ctx={ LList.len=k }; term=DB (_,_,n) } when (n>=k) -> None
    (* DeBruijn index: environment lookup *)
    | { ctx; term=DB (_,_,n); stack } (*when n<k*) ->
        state_one_step { ctx=LList.nil; term=Lazy.force (LList.nth ctx n); stack }
    (* Beta redex *)
    | { ctx; term=Lam (_,_,_,t); stack=p::s } ->
        Some { ctx=LList.cons (lazy (term_of_state p)) ctx; term=t; stack=s }
    (* Application: arguments go on the stack *)
    | { ctx; term=App (f,a,lst); stack=s } ->
        (* rev_map + rev_append to avoid map + append*)
        let tl' = List.rev_map ( fun t -> {ctx;term=t;stack=[]} ) (a::lst) in
          state_one_step { ctx; term=f; stack=List.rev_append tl' s }
    (* Constant Application *)
    | { ctx; term=Const (_,m,v); stack } ->
        begin
          match Env.get_infos dloc m v with
            | Decl _ -> None
            | Def (term,_) -> Some { ctx=LList.nil; term; stack }
            | Decl_rw (_,_,i,g) ->
                begin
                  match split_stack i stack with
                    | None -> None
                    | Some (s1,s2) ->
                        ( match rewrite [] s1 g with
                            | None -> None
                            | Some (ctx,term) -> Some { ctx; term; stack=s2 }
                        )
                end
        end

let one_step t =
  map_opt term_of_state
    (state_one_step { ctx=LList.nil; term=t; stack=[] })
