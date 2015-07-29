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
  | CannotInferTypeOfPattern of pattern*context
  | CannotSolveConstraints of rule * (int*term*term) list
  | BracketError1 of term*context
  | BracketError2 of term*context*term
  | FreeVariableDependsOnBoundVariable of loc*ident*int*context*typ

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

let rec pseudo_u sg (sigma:SS.t) : (int*term*term) list -> SS.t option = function
  | [] -> Some sigma
  | (q,t1,t2)::lst ->
    begin
      let t1' = Reduction.whnf sg (SS.apply sigma t1 q) in
      let t2' = Reduction.whnf sg (SS.apply sigma t2 q) in
      if term_eq t1' t2' then pseudo_u sg sigma lst
      else
        match t1', t2' with
        | Kind, Kind | Type _, Type _ -> pseudo_u sg sigma lst
        | DB (_,_,n), DB (_,_,n') when ( n=n' ) -> pseudo_u sg sigma lst
        | Const (_,md,id), Const (_,md',id') when
            ( Basics.ident_eq id id' && Basics.ident_eq md md' ) ->
          pseudo_u sg sigma lst

        | DB (_,x,n), t
        | t, DB (_,x,n) when n>=q ->
          begin
            match unshift_reduce sg q t with
            | None -> None
            | Some t' ->
              ( match SS.add sigma x (n-q) t' with
                | None -> assert false (*FIXME error message*)
                | Some sigma2 -> pseudo_u sg sigma2 lst )
          end

        | Pi (_,_,a,b), Pi (_,_,a',b') ->
          pseudo_u sg sigma ((q,a,a')::(q+1,b,b')::lst)
        | Lam (_,_,_,b), Lam (_,_,_,b') ->
          pseudo_u sg sigma ((q+1,b,b')::lst)

        | App (DB (_,_,n),_,_), _
        | _, App (DB (_,_,n),_,_) when ( n >= q ) ->
          if Reduction.are_convertible sg t1' t2' then pseudo_u sg sigma lst
          else None

        | App (Const (l,md,id),_,_), _
        | _, App (Const (l,md,id),_,_) when (not (Signature.is_constant sg l md id)) ->
          pseudo_u sg sigma lst

        | App (f,a,args), App (f',a',args') ->
          (* f = Kind | Type | DB n when n<q | Pi _
           * | Const md.id when (is_constant md id) *)
          pseudo_u sg sigma ((q,f,f')::(q,a,a')::(add_to_list q lst args args'))

        | _, _ -> None
    end

(* **** TYPE CHECKING/INFERENCE FOR PATTERNS ******************************** *)

type constraints = (int*term*term) list
type context2 = (loc*ident*typ) LList.t

(* Partial Context *)

type partial_context =
  { padding:int; (* expected size*)
    pctx:context2 (*partial context*)
  }

let pc_make (ctx:(loc*ident) list) : partial_context =
  let size = List.length ctx in
  assert ( size >= 0 );
  { padding=size; pctx=LList.nil }

let pc_get (delta:partial_context) (n:int) : term option =
  if n < delta.padding then None
  else
    let (_,_,ty) = List.nth (LList.lst delta.pctx) (n-delta.padding)
    in Some (Subst.shift (n+1) ty)

let pc_add (delta:partial_context) (n:int) (l:loc) (id:ident) (ty0:typ) : partial_context =
  assert ( n == delta.padding-1 && n >= 0 );
  let ty = Subst.unshift (n+1) ty0 in
  { padding = delta.padding - 1;
    pctx = LList.cons (l,id,ty) delta.pctx }

let pc_to_context (delta:partial_context) : context = LList.lst delta.pctx

let pc_to_context_wp (delta:partial_context) : context =
  let dummy = mk_DB dloc qmark 0 in
  let rec aux lst n =
    if n <= 0 then lst
    else aux ((dloc,qmark,dummy)::lst) (n-1)
  in
  aux (LList.lst delta.pctx) delta.padding

let pp_pcontext out delta =
  let lst = List.rev (LList.lst delta.pctx) in
  List.iteri (fun i (_,x,ty) -> Printf.fprintf out "%a[%i]:%a\n" pp_ident x i pp_term ty) lst;
  for i = 0 to delta.padding -1 do
    Printf.fprintf out "?[%i]:?\n" (i+LList.len delta.pctx)
  done

(* *** *)

let rec get_last = function
  | [] -> assert false
  | [a] -> ([],a)
  | hd::tl ->
    let (tl0,a) = get_last tl in
    (hd::tl0,a)

let unshift_n sg n te =
  try Subst.unshift n te
  with Subst.UnshiftExn -> Subst.unshift n (Reduction.snf sg te)

let rec infer_pattern sg (delta:partial_context) (sigma:context2) (lst:constraints) (pat:pattern) : typ * partial_context * constraints =
  match pat with
  | Pattern (l,md,id,args) ->
    let (_,ty,delta2,lst2) = List.fold_left (infer_pattern_aux sg sigma)
        ( mk_Const l md id , Signature.get_type sg l md id , delta , lst ) args
    in (ty,delta2,lst2)
  | Var (l,x,n,args) ->
    if n < (LList.len sigma) then
      let (_,ty,delta2,lst2) = List.fold_left (infer_pattern_aux sg sigma)
          ( mk_DB l x n, get_type (LList.lst sigma) l x n , delta , lst ) args
      in (ty,delta2,lst2)
    else
      let ctx = (LList.lst sigma)@(pc_to_context_wp delta) in
      raise (TypingError (CannotInferTypeOfPattern (pat,ctx))) (* not a pattern *)
  | Brackets _ ->
    let ctx = (LList.lst sigma)@(pc_to_context_wp delta) in
    raise (TypingError (CannotInferTypeOfPattern (pat,ctx)))
  | Lambda _ ->
    let ctx = (LList.lst sigma)@(pc_to_context_wp delta) in
    raise (TypingError (CannotInferTypeOfPattern (pat,ctx)))

and infer_pattern_aux sg (sigma:context2) (f,ty_f,delta,lst:term*typ*partial_context*constraints) (arg:pattern) : term * typ * partial_context * constraints =
  match Reduction.whnf sg ty_f with
    | Pi (_,_,a,b) ->
        let (delta2,lst2) = check_pattern sg delta sigma a lst arg in
        let arg' = pattern_to_term arg in
        ( Term.mk_App f arg' [], Subst.subst b arg', delta2 , lst2 )
    | ty_f ->
      let ctx = (LList.lst sigma)@(pc_to_context_wp delta) in
      raise (TypingError (ProductExpected (f,ctx,ty_f)))

and check_pattern sg (delta:partial_context) (sigma:context2) (exp_ty:typ) (lst:constraints) (pat:pattern) : partial_context * constraints =
(*   debug "check_pattern %a:%a" pp_pattern pat pp_term exp_ty; *)
  match pat with
  | Lambda (l,x,p) ->
    begin
      match Reduction.whnf sg exp_ty with
      | Pi (l,x,a,b) -> check_pattern sg delta (LList.cons (l,x,a) sigma) b lst p
      | exp_ty ->
        let ctx = (LList.lst sigma)@(pc_to_context_wp delta) in
        raise (TypingError ( ProductExpected (pattern_to_term pat,ctx,exp_ty)))
    end
  | Brackets te ->
        let te2 =
          try Subst.unshift (delta.padding + LList.len sigma) te
          with Subst.UnshiftExn ->
            let ctx = (LList.lst sigma)@(pc_to_context_wp delta) in
            raise (TypingError (BracketError1 (te,ctx)))
        in
        let ty2 =
          try unshift_n sg (delta.padding + LList.len sigma) exp_ty
          with Subst.UnshiftExn ->
            let ctx = (LList.lst sigma)@(pc_to_context_wp delta) in
            raise (TypingError (BracketError2 (te,ctx,exp_ty)))
        in
        check sg (pc_to_context delta) te2 ty2;
        ( delta, lst )
  | Var (l,x,n,[]) when ( n >= LList.len sigma ) ->
    begin
      let k = LList.len sigma in

      match pc_get delta (n-k) with
      | None ->
        ( try ( pc_add delta (n-k) l x (unshift_n sg k exp_ty), lst )
          with Subst.UnshiftExn ->
            let ctx = (LList.lst sigma)@(pc_to_context_wp delta) in
            raise (TypingError (FreeVariableDependsOnBoundVariable (l,x,n,ctx,exp_ty))) )
      | Some ty ->
        let inf_ty = Subst.shift k ty in
        ( delta, (k,inf_ty,exp_ty)::lst )
    end
  | Var (l,x,n,args) when (n>=LList.len sigma) ->
    begin
      let (args2,last) = get_last args in (*TODO use fold?*)
      match last with
      | Var (l2,x2,n2,[]) ->
        check_pattern sg delta sigma
          (mk_Pi l2 x2 (get_type (LList.lst sigma) l2 x2 n2) (Subst.subst_n n2 x2 exp_ty) )
          lst (Var(l,x,n,args2))
      | _ ->
        let ctx = (LList.lst sigma)@(pc_to_context_wp delta) in
        raise (TypingError (CannotInferTypeOfPattern (pat,ctx))) (* not a pattern *)
    end
  | _ ->
    begin
      let (inf_ty,delta2,lst2) = infer_pattern sg delta sigma lst pat in
      let q = LList.len sigma in
      ( delta2 , (q,inf_ty,exp_ty)::lst2 )
    end

(* ************************************************************************** *)

(* FIXME no need to traverse three times the terms... *)
let subst_context (sub:SS.t) (ctx:context) : context =
  try List.mapi ( fun i (l,x,ty) ->
      (l,x,
       Subst.unshift (i+1) (SS.apply sub (Subst.shift (i+1) ty) 0) )
    ) ctx
  with
  | Subst.UnshiftExn -> assert false (*FIXME*) (* may happen with non-linear patterns*)

let check_rule sg (ctx0,le,ri:rule) : rule2 =
  let delta = pc_make ctx0 in
  let (ty_le,delta,lst) = infer_pattern sg delta LList.nil [] le in
  assert ( delta.padding == 0 );
  let sub = match pseudo_u sg SS.identity lst with
    | None -> raise (TypingError (CannotSolveConstraints ((ctx0,le,ri),lst)))
    | Some s -> ( (*debug "%a" SS.pp s;*) s )
  in
  let (ri2,ty_le2,ctx2) =
    if SS.is_identity sub then (ri,ty_le,LList.lst delta.pctx)
    else (SS.apply sub ri 0,
          SS.apply sub ty_le 0,
          subst_context sub (LList.lst delta.pctx))
  in
  check sg ctx2 ri2 ty_le2;
  (ctx2,le,ri2)
(*   debug "Checked rule:\n [ %a ] %a --> %a" pp_context ctx2 pp_pattern le pp_term ri2 *)
