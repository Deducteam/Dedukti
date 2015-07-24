open Basics
open Term
open Rule

let coc = ref false

type typ = term
type context = (loc*ident*term) list

(* ********************** ERROR MESSAGES *)

type typing_error =
  | KindIsNotTypable
  | ConvertibilityError of term*context*term*term
  | VariableNotFound of loc*ident*int*context
  | SortExpected of term*context*term
  | ProductExpected of term*context*term
  | InexpectedKind of term*context
  | DomainFreeLambda of loc

exception TypingError of typing_error

(* ********************** CONTEXT *)

let get_type ctx l x n =
  try
    let (_,_,ty) = List.nth ctx n in Subst.shift (n+1) ty
  with Failure _ ->
    raise (TypingError (VariableNotFound (l,x,n,ctx)))

let extend_ctx a ctx = function
  | Type _ -> a::ctx
  | Kind when !coc -> a::ctx
  | ty_a ->
    let (_,_,te) = a in
    raise (TypingError (ConvertibilityError (te, ctx, mk_Type dloc, ty_a)))

(* ********************** TYPE CHECKING/INFERENCE FOR TERMS  *)

let rec infer sg (ctx:context) : term -> typ = function
  | Kind -> raise (TypingError KindIsNotTypable)
  | Type l -> mk_Kind
  | DB (l,x,n) -> get_type ctx l x n
  | Const (l,md,id) -> Signature.get_type sg l md id
  | App (f,a,args) ->
    snd (List.fold_left (check_app sg ctx) (f,infer sg ctx f) (a::args))
  | Pi (l,x,a,b) ->
      let ty_a = infer sg ctx a in
      let ctx2 = extend_ctx (l,x,a) ctx ty_a in
      let ty_b = infer sg ctx2 b in
      ( match ty_b with
        | Kind | Type _ -> ty_b
        | _ -> raise (TypingError (SortExpected (b, ctx2, ty_b))) )
  | Lam  (l,x,Some a,b) ->
      let ty_a = infer sg ctx a in
      let ctx2 = extend_ctx (l,x,a) ctx ty_a in
      let ty_b = infer sg ctx2 b in
        ( match ty_b with
            | Kind -> raise (TypingError (InexpectedKind (b, ctx2)))
            | _ -> mk_Pi l x a ty_b )
  | Lam  (l,x,None,b) -> raise (TypingError (DomainFreeLambda l))

and check sg (ctx:context) (te:term) (ty_exp:typ) : unit =
  match te with
  | Lam (l,x,None,u) ->
    ( match Reduction.whnf sg ty_exp with
      | Pi (_,_,a,b) -> check sg ((l,x,a)::ctx) u b
      | _ -> raise (TypingError (ProductExpected (te,ctx,ty_exp))) )
  | _ ->
    let ty_inf = infer sg ctx te in
    if Reduction.are_convertible sg ty_inf ty_exp then ()
    else raise (TypingError (ConvertibilityError (te,ctx,ty_exp,ty_inf)))

and check_app sg (ctx:context) (f,ty_f:term*typ) (arg:term) : term*typ =
  match Reduction.whnf sg ty_f with
    | Pi (_,_,a,b) ->
      let _ = check sg ctx arg a in (mk_App f arg [], Subst.subst b arg )
    | _ -> raise (TypingError ( ProductExpected (f,ctx,ty_f)))

let inference sg (te:term) : typ = infer sg [] te

let checking sg (te:term) (ty:term) : unit =
  let _ = infer sg [] ty in
  check sg [] te ty

(* **** PSEUDO UNIFICATION ********************** *)

let rec add_to_list q lst args1 args2 =
  match args1,args2 with
    | [], [] -> lst
    | a1::args1, a2::args2 -> add_to_list q ((q,a1,a2)::lst) args1 args2
    | _, _ -> raise (Invalid_argument "add_to_list")

module SS = Subst.S

let unshift_reduce sg q t =
  try Some (Subst.unshift q t)
  with Subst.UnshiftExn ->
    ( try Some (Subst.unshift q (Reduction.snf sg t))
      with Subst.UnshiftExn -> None )

let pseudo_unification sg (q:int) (a:term) (b:term) : SS.t option =
  let rec aux (sigma:SS.t) : (int*term*term) list -> SS.t option = function
    | [] -> Some sigma
    | (q,t1,t2)::lst ->
        begin
          let t1' = Reduction.whnf sg (SS.apply sigma t1 q) in
          let t2' = Reduction.whnf sg (SS.apply sigma t2 q) in
            if term_eq t1' t2' then aux sigma lst
            else
              match t1', t2' with
                | Kind, Kind | Type _, Type _ -> aux sigma lst
                | DB (_,_,n), DB (_,_,n') when ( n=n' ) -> aux sigma lst
                | Const (_,md,id), Const (_,md',id') when
                    ( Basics.ident_eq id id' && Basics.ident_eq md md' ) ->
                    aux sigma lst

                | DB (_,x,n), t
                | t, DB (_,x,n) when n>=q ->
                  begin
                    match unshift_reduce sg q t with
                    | None -> None
                    | Some t' ->
                      ( match SS.add sigma x (n-q) t' with
                        | None -> assert false
                        | Some sigma2 -> aux sigma2 lst )
                  end

                | Pi (_,_,a,b), Pi (_,_,a',b') ->
                    aux sigma ((q,a,a')::(q+1,b,b')::lst)
                | Lam (_,_,_,b), Lam (_,_,_,b') ->
                    aux sigma ((q+1,b,b')::lst)

                | App (DB (_,_,n),_,_), _
                | _, App (DB (_,_,n),_,_) when ( n >= q ) ->
                    if Reduction.are_convertible sg t1' t2' then aux sigma lst
                    else None

                | App (Const (l,md,id),_,_), _
                | _, App (Const (l,md,id),_,_) when (not (Signature.is_constant sg l md id)) ->
                    if Reduction.are_convertible sg t1' t2' then aux sigma lst
                    else None

                | App (f,a,args), App (f',a',args') ->
                    (* f = Kind | Type | DB n when n<q | Pi _
                     * | Const md.id when (is_constant md id) *)
                    aux sigma ((q,f,f')::(q,a,a')::(add_to_list q lst args args'))

                | _, _ -> None
        end
  in
  if term_eq a b then Some SS.identity
  else aux SS.identity [(q,a,b)]

(* **** TYPE CHECKING/INFERENCE FOR PATTERNS ******************************** *)

let rec infer_pattern sg (ctx:context) (q:int) (sigma:SS.t) (pat:pattern) : typ*SS.t =
  match pat with
  | Pattern (l,md,id,args) ->
    let (_,ty,si) = List.fold_left (infer_pattern_aux sg ctx q)
        (mk_Const l md id,SS.apply sigma (Signature.get_type sg l md id) q,sigma) args in
    (ty,si)
  | Var (l,x,n,args) ->
    let (_,ty,si) = List.fold_left (infer_pattern_aux sg ctx q)
        (mk_DB l x n,SS.apply sigma (get_type ctx l x n) q,sigma) args in
    (ty,si)
  | Brackets t -> ( infer sg ctx t , SS.identity )
  | Lambda (l,x,p) -> raise (TypingError (DomainFreeLambda l))

and infer_pattern_aux sg (ctx:context) (q:int) (f,ty_f,sigma0:term*term*SS.t) (arg:pattern) : term*typ*SS.t =
  match Reduction.whnf sg ty_f with
    | Pi (_,_,a,b) ->
        let sigma = check_pattern sg ctx q a sigma0 arg in
        let arg' = pattern_to_term arg in
        let b2 = SS.apply sigma b (q+1) in
        let arg2 = SS.apply sigma arg' q in
        ( Term.mk_App f arg' [], Subst.subst b2 arg2, sigma )
    | ty_f -> raise (TypingError ( ProductExpected (f,ctx,ty_f)))

and check_pattern sg (ctx:context) (q:int) (exp_ty:typ) (sigma0:SS.t) (pat:pattern) : SS.t =
  match pat with
  | Lambda (l,x,p) ->
    begin
      match Reduction.whnf sg exp_ty with
      | Pi (l,x,a,b) ->
        check_pattern sg ((l,x,a)::ctx) (q+1) b sigma0 p
      | exp_ty ->
        raise (TypingError ( ProductExpected (pattern_to_term pat,ctx,exp_ty)))
    end
  | Brackets t -> ( check sg ctx t exp_ty; SS.identity )
  | _ ->
    begin
      let (inf_ty,sigma1) = infer_pattern sg ctx q sigma0 pat in
      match pseudo_unification sg q exp_ty inf_ty with
      | None -> raise (TypingError (ConvertibilityError (pattern_to_term pat,ctx,exp_ty,inf_ty)))
      | Some sigma2 -> SS.merge sigma1 sigma2
    end

(* ************************************************************************** *)

let check_rule sg (ctx,le,ri:rule) : unit =
  let ctx = List.fold_left
      (fun ctx (l,id,ty) -> extend_ctx (l,id,ty) ctx (infer sg ctx ty))
      [] (List.rev ctx)
  in
  let (ty_le,sigma) = infer_pattern sg ctx 0 SS.identity le in
  let ri2 =
    if SS.is_identity sigma then ri
    else ( debug "%a" SS.pp sigma ; (SS.apply sigma ri 0) ) in
  let ty_ri = infer sg ctx ri2 in
  if not (Reduction.are_convertible sg ty_le ty_ri) then
    raise (TypingError (ConvertibilityError (ri,ctx,ty_le,ty_ri)))
