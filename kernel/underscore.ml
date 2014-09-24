open Term
open Rule
(* FIXME not compatible with the CoC flag *)

let error_convertibility te ctx exp inf =
  Print.fail (get_loc te)
    "Error while refining '%a' in context:\n%a.\nExpected: %a\nInferred: %a."
      Pp.pp_term te Pp.pp_context ctx Pp.pp_term exp Pp.pp_term inf

let error_product te ctx exp =
  Print.fail (get_loc te)
    "Error while refining '%a' in context:\n%a. The expected type is not a product.\nExpected: %a."
      Pp.pp_term te Pp.pp_context ctx Pp.pp_term exp

let rec number_of_jokers = function
  | Joker _ -> 1
  | Brackets _
  | MatchingVar _ -> 0
  | Pattern (_,_,_,args)
  | BoundVar (_,_,_,args) ->
      List.fold_left (fun n p -> n+(number_of_jokers p)) 0 args
  | Lambda (_,_,p) -> number_of_jokers p

let unshift te n ty =
  let rec aux k = function
  | DB (_,x,i) as t when i<k -> t
  | DB (_,x,i) ->
      if i>n then mk_DB dloc x (i-n-1)
      else Print.fail (get_loc te) "Cannot refine a type for '%a'." Pp.pp_term te
  | App (f,a,args) -> mk_App (aux k f) (aux k a) (List.map (aux k) args )
  | Lam (_,x,_,f) -> mk_Lam dloc x None (aux (k+1) f)
  | Pi  (_,x,a,b) -> mk_Pi dloc x (aux k a) (aux (k+1) b)
  | Type _ | Kind | Const _ as t -> t
  in
    aux 0 ty

let db_get_type l ctx n nb = Subst.shift (n+1+nb) (snd (List.nth ctx n))

let refine (nb_jokers:int) (ctx0:context) (te:term) : context =
  let arr = Array.make nb_jokers (qmark,mk_Kind) in
  let size = List.length ctx0 in

  let rec infer k (ctx:context) : term -> term = function
    | Const (l,md,id) -> Env.get_type l md id
    | DB (l,_,n) when n<k -> db_get_type l ctx n 0
    | DB (l,_,n) ->
        begin
          let n' = n-nb_jokers-k in
            assert ( n' >= 0 );
            assert ( n' < size );
            db_get_type l ctx0 n' nb_jokers
        end
    | App (f,a,args) ->
        snd (List.fold_left (check_app k ctx) (f,infer k ctx f) (a::args))
    | Lam _ -> assert false
    | Kind | Type _ | Pi _-> assert false

  and check_app k (ctx:context) (f,ty_f:term*term) (arg:term) : term*term =
    match Reduction.whnf ty_f with
      | Pi (_,_,a,b) -> ( check k ctx arg a; ( mk_App f arg [] , Subst.subst b arg ) )
      | _ -> error_product f ctx ty_f

  and check k ctx (te:term) (ty_exp:term) : unit =
    match te with
      | Lam (l,x,_,u) ->
          ( match Reduction.whnf ty_exp with
              | Pi (_,x,a1,b) -> check (k+1) ((x,a1)::ctx) u b
              | _ -> error_product te ctx ty_exp
          )
      | DB (_,_,n) when ( (n-k) < nb_jokers ) ->
          arr.(n-k) <- (qmark,unshift te n ty_exp)
      | _ ->
          let ty_inf = infer k ctx te in
            if Reduction.are_convertible ty_exp ty_inf then ()
            else error_convertibility te ctx ty_exp ty_inf
  in
  let _ = infer 0 [] te in
    (Array.to_list arr)@ctx0

let shift_pattern (nb:int) (p:pattern) : pattern =
  let cpt = ref (-1) in
  let get_cpt _ = incr cpt ; !cpt in
  let rec aux k = function
    | Joker l -> MatchingVar (l,qmark,k+(get_cpt ()),[])
    | Brackets t -> Brackets (Subst.shift nb t)
    | BoundVar (l,x,n,args) -> BoundVar(l,x,n,List.map (aux k) args)
    | Pattern (l,m,v,args) -> Pattern(l,m,v,List.map (aux k) args)
    | Lambda (l,x,pat) -> Lambda(l,x,aux (k+1) pat)
    | MatchingVar (l,x,n,args) -> MatchingVar(l,x,n+nb,args)
  in
    aux 0 p

let refine_rule (r:rule) : rule =
  let pat = Pattern(r.l,r.md,r.id,r.args) in
  let nb = number_of_jokers pat in
    if nb=0 then r
    else
      let pat2 = shift_pattern nb pat in
      let ctx2 = refine nb r.ctx (pattern_to_term pat2) in
      let rhs = Subst.shift nb r.rhs in
       (* Print.debug "NEW PATTERN: %a." Pp.pp_pattern pat2 ;
        Print.debug "NEW CONTEXT:\n %a." Pp.pp_context ctx2 ; *)
        match pat2 with
          | Pattern(_,_,_,args2) ->
              { l=r.l; ctx=ctx2; md=r.md; id=r.id; args=args2; rhs=rhs; }
          | _ -> assert false
