open Types

type cbn_state = {
  ctx : term Lazy.t subst;      (*context*)
  term : term;                  (*term to reduce*)
  stack : cbn_state list;       (*stack*)
}

let make_cbn ?(ctx=subst_empty) ?(stack=[]) term = {ctx;term;stack;}

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

let rec cbn_term_of_state {ctx;term;stack} : term =
  let t = ( if subst_is_empty ctx then term else Subst.psubst_l ctx term ) in
    match stack with
      | [] -> t
      | a::lst ->
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
    | {term=Type _}
    | {term=Kind}
    | {term=Pi _}
    | {term=Lam _; stack=[] } -> config
    (* Bound variable (to be substitute) *)
    | {ctx; term=Var (_,v); stack } ->
        begin match subst_find ctx v with
        | None -> config
        | Some (lazy t) -> cbn_reduce (make_cbn ~stack t)
        end
    (* Beta redex *)
    | {ctx; term=Lam (_,v,_,t); stack= p::s } ->
        cbn_reduce { ctx=subst_bind ctx v (lazy (cbn_term_of_state p)); term=t; stack=s }
    (* Application *)
    | {ctx; term=App (f,a,lst); stack=s } ->
        (* rev_map + rev_append to avoid map + append*)
        let tl' = List.rev_map (fun t -> make_cbn ~ctx t) (a::lst) in
        cbn_reduce { ctx; term=f; stack=List.rev_append tl' s; }
    (* Let binding *)
    | {ctx; term=Let (_,v,a,b); stack=s} ->
        (* push a (normalized, but lazily) in context and evaluate b *)
        let a' = lazy (cbn_term_of_state(cbn_reduce (make_cbn ~ctx a))) in
        let ctx = subst_bind ctx v a' in
        cbn_reduce { ctx; term=b; stack=s}
    (* Global variable*)
    | {term=Const (_,m,_)} when m==empty  -> config
    | {term=Const (_,m,v); stack=s }      ->
        begin
          match Env.get_infos dloc m v with
            | Def (te,_)        -> cbn_reduce { ctx=subst_empty; term=te; stack=s }
            | Decl _            -> config
            | Decl_rw (_,_,i,g) ->
                ( match split_stack i s with
                    | None                -> config
                    | Some (s1,s2)        ->
                        match rewrite (LList.make ~len:i s1) g with
                        | None              -> config
                        | Some (ctx,t)      -> cbn_reduce { ctx; term=t; stack= s2}
                )
        end
    | {term=Meta _}                       -> assert false

and rewrite (args:cbn_state LList.t) (g:dtree) =
  (* convert stack of states, into a substitution *)
  let _build_ctx sbuild args =
    List.fold_left
      (fun sigma (i,v) ->
        let t = lazy (cbn_term_of_state (LList.nth args i)) in
        subst_bind sigma v t
      ) subst_empty sbuild
  in
  (* assert ( nargs = List.lenght args ); *)
  match g with
    | Switch (i,cases,def)      ->
        begin
          (* assert (i<Array.length args); *)
          match cbn_reduce (LList.nth args i) with
            | {term=Const (_,m,v); stack=s}  ->
                ( match safe_find m v cases , def with
                    | Some g , _        ->
                        rewrite (LList.append_l (LList.remove i args) s) g
                    | None , Some g     -> rewrite args g
                    | _ , _             -> None )
            | {stack=s} ->
                (match def with
                   | Some g     -> rewrite args g
                   | None       -> None )
        end
    | Test (sbuild,[],te,def)          ->
        let ctx = _build_ctx sbuild args in
        Some (ctx, te)
    | Test (sbuild,lst,te,def)         ->
        let ctx = _build_ctx sbuild args in
        let conv_tests = List.map
          (fun (EqTerm (t1,t2)) ->
            {ctx;term=t1;stack=[]} , {ctx;term=t2;stack=[]}
          ) lst
        in
        if state_conv conv_tests then
          Some (ctx, te)
        else
          match def with
            | None    -> None
            | Some g  -> rewrite args g

and state_conv : (cbn_state*cbn_state) list -> bool = function
  | []                  -> true
  | (s1,s2)::lst        ->
      begin
        let t1 = cbn_term_of_state s1 in  (* TODO: not very efficient *)
        let t2 = cbn_term_of_state s2 in
          if term_eq t1 t2 then
            state_conv lst
          else
            let s1' = cbn_reduce s1 in
            let s2' = cbn_reduce s2 in
              match s1',s2' with (*states are beta-delta head normal*)
                | {term=Kind; stack=s} , {term=Kind; stack=s'}
                | {term=Type _; stack=s} , {term=Type _; stack=s'} ->
                    (* assert ( List.length s == 0 && List.length s' == 0 ) *)
                    state_conv lst
                | {term=Var (_,v); stack=s},
                  {term=Var (_,v'); stack=s'} ->
                    Var.equal v v' &&
                    begin match (add_to_list lst s s') with
                      | None          -> false
                      | Some lst'     -> state_conv lst'
                    end
                | {term=Const (_,m,v); stack=s}, {term=Const (_,m',v'); stack=s'} ->
                    ident_eq v v' && ident_eq m m' &&
                    begin match (add_to_list lst s s') with
                      | None          -> false
                      | Some lst'     -> state_conv lst'
                    end
                | {ctx; term=Lam (_,v,a,f); stack=s},
                  {ctx=ctx'; term=Lam (_,v',a',f'); stack=s'} ->
                    let arg = Lazy.lazy_from_val (mk_Unique ()) in
                    let x = make_cbn ~ctx a, make_cbn ~ctx:ctx' a' in
                    let y = make_cbn ~ctx:(subst_bind ctx v arg) f,
                            make_cbn ~ctx:(subst_bind ctx' v' arg) f' in
                    begin match add_to_list (x::y::lst) s s' with
                      | None        -> false
                      | Some lst'   -> state_conv lst'
                    end
                | {ctx; term=Pi (_,v_opt,a,f); stack=s},
                  {ctx=ctx'; term=Pi (_,v'_opt,a',f'); stack=s'} ->
                    let arg = Lazy.lazy_from_val (mk_Unique ()) in
                    let x = make_cbn ~ctx a, make_cbn ~ctx:ctx' a' in
                    let ctx = match v_opt with
                      | None -> ctx
                      | Some v -> subst_bind ctx v arg
                    and ctx' = match v'_opt with
                      | None -> ctx'
                      | Some v' -> subst_bind ctx' v' arg
                    in
                    let y = make_cbn ~ctx f, make_cbn ~ctx:ctx' f' in
                    begin match add_to_list (x::y::lst) s s' with
                      | None        -> false
                      | Some lst'   -> state_conv lst'
                    end
                | {term=Let _}, _
                | _, {term=Let _}
                | {term=Meta _} , _
                | _ , {term=Meta _}     -> assert false
                | _, _                  -> false
      end

(* Weak Normal Form *)
let whnf ?(let_ctx=subst_empty) t =
  let ctx = subst_map Lazy.from_val let_ctx in
  cbn_term_of_state (cbn_reduce (make_cbn ~ctx t))

(* Head Normal Form *)
let rec hnf ?(let_ctx=subst_empty) t =
  match whnf ~let_ctx t with
    | Kind | Const _ | Var _ | Type _ | Pi (_,_,_,_) | Lam (_,_,_,_) as t' -> t'
    | App (f,a,lst) ->
        mk_App (hnf ~let_ctx f) (hnf ~let_ctx a) (List.map (hnf ~let_ctx) lst)
    | Let _ | Meta _  -> assert false

(* Convertibility Test *)
let are_convertible ?(let_ctx=subst_empty) t1 t2 =
  let ctx = subst_map Lazy.from_val let_ctx in
  state_conv [ (make_cbn ~ctx t1, make_cbn ~ctx t2) ]

(* Strong Normal Form *)
let rec snf ?(let_ctx=subst_empty) (t:term) : term =
  match whnf ~let_ctx t with
    | Kind | Const _
    | Var _ | Type _ as t' -> t'
    | App (f,a,lst)     ->
        mk_App (snf ~let_ctx f) (snf ~let_ctx a) (List.map (snf ~let_ctx) lst)
    | Pi (_,x,a,b)      -> mk_Pi dloc x (snf ~let_ctx a) (snf ~let_ctx b)
    | Lam (_,x,a,b)     -> mk_Lam dloc x (snf ~let_ctx a) (snf ~let_ctx b)
    | Let _ | Meta _            -> assert false

(* One-Step Reduction *)
let rec state_one_step = function
  (* Weak normal terms *)
  | {term=Type _}
  | {term=Kind}
  | {term=Pi _}                       -> None
  | {term=Lam _; stack=[]}            -> None
  (* Bound variable (to be substitute) *)
  | {ctx; term=Var (_,v); stack} (*when n<k*)   ->
      begin match subst_find ctx v with
      | None -> None
      | Some (lazy t') -> Some (make_cbn ~stack t')
      end
  (* Beta redex *)
  | {ctx; term= Lam (_,v,_,t); stack=p::s}      ->
      Some {ctx=subst_bind ctx v (lazy (cbn_term_of_state p)); term=t; stack=s}
  (* Application *)
  | {ctx; term=App (f,a,args); stack=s }              ->
      let tl' = List.map (fun t -> make_cbn t) (a::args) in
      state_one_step {ctx; term=f; stack=tl' @ s; }
  (* Let binding *)
  | {ctx; term=Let (_,v,a,b); stack} ->
      (* push a in context (without evaluating it) and evaluate b *)
      let a' = lazy (cbn_term_of_state (make_cbn ~ctx a)) in
      let ctx= subst_bind ctx v a' in
      Some { ctx; term=b; stack}
  (* Global variable*)
  | {term=Const (_,m,_)} when m==empty  -> None
  | {term=Const (_,m,v); stack=s }      ->
      begin
        match Env.get_infos dloc m v with
          | Def (te,_)          -> Some (make_cbn ~stack:s te)
          | Decl _              -> None
          | Decl_rw (_,_,i,g)   ->
              begin match split_stack i s with
                  | None                -> None
                  | Some (s1,s2)        ->
                      match rewrite (LList.make ~len:i s1) g with
                        | None              -> None
                        | Some (ctx,t)      -> Some {ctx; term=t; stack=s2}
              end
      end
  | {term=Meta _}                       -> assert false

let one_step ?(let_ctx=subst_empty) t =
  let ctx = subst_map Lazy.from_val let_ctx in
  match state_one_step (make_cbn ~ctx t) with
    | None      -> None
    | Some st   -> Some ( cbn_term_of_state st )
