open Basic
open Format
open Rule
open Term


let coc = ref false

type typ = term

(* ********************** ERROR MESSAGES *)

type typing_error =
  | KindIsNotTypable
  | ConvertibilityError of term * typed_context * term * term
  | VariableNotFound of loc * ident * int * typed_context
  | SortExpected of term * typed_context * term
  | ProductExpected of term * typed_context * term
  | InexpectedKind of term * typed_context
  | DomainFreeLambda of loc
  | CannotInferTypeOfPattern of pattern * typed_context
  | CannotSolveConstraints of untyped_rule * (int * term * term) list
  | BracketError1 of term * typed_context
  | BracketError2 of term * typed_context*term
  | FreeVariableDependsOnBoundVariable of loc * ident * int * typed_context * term
  | NotImplementedFeature of loc

exception TypingError of typing_error

(* ********************** CONTEXT *)

let snf sg = Reduction.reduction sg Reduction.Snf

let whnf sg = Reduction.reduction sg Reduction.Whnf

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

let rec infer sg (ctx:typed_context) : term -> typ = function
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

and check sg (ctx:typed_context) (te:term) (ty_exp:typ) : unit =
  match te with
  | Lam (l,x,None,u) ->
    ( match whnf sg ty_exp with
      | Pi (_,_,a,b) -> check sg ((l,x,a)::ctx) u b
      | _ -> raise (TypingError (ProductExpected (te,ctx,ty_exp))) )
  | _ ->
    let ty_inf = infer sg ctx te in
    if Reduction.are_convertible sg ty_inf ty_exp then ()
    else raise (TypingError (ConvertibilityError (te,ctx,ty_exp,ty_inf)))

and check_app sg (ctx:typed_context) (f,ty_f:term*typ) (arg:term) : term*typ =
  match whnf sg ty_f with
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

let safe_add_to_list q lst args1 args2 =
  try Some (add_to_list q lst args1 args2)
  with Invalid_argument _ -> None

module SS = Subst.Subst

let unshift_reduce sg q t =
  try Some (Subst.unshift q t)
  with Subst.UnshiftExn ->
    ( try Some (Subst.unshift q (snf sg t))
      with Subst.UnshiftExn -> None )

let rec pseudo_u sg (sigma:SS.t) : (int*term*term) list -> SS.t option = function
  | [] -> Some sigma
  | (q,t1,t2)::lst ->
    begin
      let t1' = whnf sg (SS.apply sigma t1 q) in
      let t2' = whnf sg (SS.apply sigma t2 q) in
      if term_eq t1' t2' then pseudo_u sg sigma lst
      else
        match t1', t2' with
        | Kind, Kind | Type _, Type _ -> pseudo_u sg sigma lst
        | DB (_,_,n), DB (_,_,n') when ( n=n' ) -> pseudo_u sg sigma lst
        | Const (_,md,id), Const (_,md',id') when
            ( ident_eq id id' && ident_eq md md' ) ->
          pseudo_u sg sigma lst

        | DB (l1,x1,n1), DB (l2,x2,n2) when ( n1>=q && n2>=q) ->
          begin
            let (x,n,t) = if n1<n2 then (x1,n1,mk_DB l2 x2 (n2-q)) else (x2,n2,mk_DB l1 x1 (n1-q)) in
            match SS.add sigma x (n-q) t with
            | None -> assert false
            | Some sigma2 -> pseudo_u sg sigma2 lst
          end
        | DB (_,x,n), t when n>=q ->
          begin
            match unshift_reduce sg q t with
            | None -> None
            | Some t' ->
              ( match SS.add sigma x (n-q) t' with
                | None ->
                  ( match SS.add sigma x (n-q) (snf sg t') with
                    | None -> None
                    | Some sigma2 -> pseudo_u sg sigma2 lst )
                | Some sigma2 -> pseudo_u sg sigma2 lst )
          end
        | t, DB (_,x,n) when n>=q ->
          begin
            match unshift_reduce sg q t with
            | None -> None
            | Some t' ->
              ( match SS.add sigma x (n-q) t' with
                | None ->
                  ( match SS.add sigma x (n-q) (snf sg t') with
                    | None -> None
                    | Some sigma2 -> pseudo_u sg sigma2 lst )
                | Some sigma2 -> pseudo_u sg sigma2 lst )
          end

        | Pi (_,_,a,b), Pi (_,_,a',b') ->
          pseudo_u sg sigma ((q,a,a')::(q+1,b,b')::lst)
        | Lam (_,_,_,b), Lam (_,_,_,b') ->
          pseudo_u sg sigma ((q+1,b,b')::lst)

        | App (DB (_,_,n),_,_), _  when ( n >= q ) ->
          if Reduction.are_convertible sg t1' t2' then
            ( debug 2 "Ignoring constraint: %a ~ %a" pp_term t1' pp_term t2'; pseudo_u sg sigma lst )
          else None
        | _, App (DB (_,_,n),_,_) when ( n >= q ) ->
          if Reduction.are_convertible sg t1' t2' then
            ( debug 2 "Ignoring constraint: %a ~ %a" pp_term t1' pp_term t2'; pseudo_u sg sigma lst )
          else None

        | App (Const (l,md,id),_,_), _ when (not (Signature.is_injective sg l md id)) ->
          ( debug 2 "Ignoring constraint: %a ~ %a" pp_term t1' pp_term t2'; pseudo_u sg sigma lst )
        | _, App (Const (l,md,id),_,_) when (not (Signature.is_injective sg l md id)) ->
          ( debug 2 "Ignoring constraint: %a ~ %a" pp_term t1' pp_term t2'; pseudo_u sg sigma lst )

        | App (f,a,args), App (f',a',args') ->
          (* f = Kind | Type | DB n when n<q | Pi _
           * | Const md.id when (is_constant md id) *)
          begin
            match safe_add_to_list q lst args args' with
            | None -> None
            | Some lst2 -> pseudo_u sg sigma ((q,f,f')::(q,a,a')::lst2)
          end

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

let pc_to_context (delta:partial_context) : typed_context = LList.lst delta.pctx

let pc_to_context_wp (delta:partial_context) : typed_context =
  let dummy = mk_DB dloc qmark 0 in
  let rec aux lst n =
    if n <= 0 then lst
    else aux ((dloc,qmark,dummy)::lst) (n-1)
  in
  aux (LList.lst delta.pctx) delta.padding

let pp_pcontext fmt delta =
  let lst = List.rev (LList.lst delta.pctx) in
  List.iteri (fun i (_,x,ty) -> fprintf fmt "%a[%i]:%a\n" pp_ident x i pp_term ty) lst;
  for i = 0 to delta.padding -1 do
    fprintf fmt "?[%i]:?\n" (i+LList.len delta.pctx)
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
  with Subst.UnshiftExn -> Subst.unshift n (snf sg te)

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
  match whnf sg ty_f with
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
      match whnf sg exp_ty with
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
      let (args2,last) = get_last args in
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

let pp_context_inline fmt ctx =
  pp_list ", "
    (fun fmt (_,x,ty) -> fprintf fmt "%a: %a" pp_ident x pp_term ty )
    fmt (List.rev ctx)

let rec pp_term_j k fmt = function
  | Kind               -> Format.fprintf fmt "Kind"
  | Type _             -> Format.fprintf fmt "Type"
  | DB  (_,x,n) when n<k -> fprintf fmt "%a[%i]" pp_ident x n
  | DB  (_,x,n)        -> fprintf fmt "_"
  | Const (_,m,v)      -> fprintf fmt "%a.%a" pp_ident m pp_ident v
  | App (f,a,args)     -> pp_list " " (pp_term_wp_j k) fmt (f::a::args)
  | Lam (_,x,None,f)   -> fprintf fmt "%a => %a" pp_ident x pp_term f
  | Lam (_,x,Some a,f) -> fprintf fmt "%a:%a => %a" pp_ident x (pp_term_wp_j (k+1)) a pp_term f
  | Pi  (_,x,a,b)      -> fprintf fmt "%a:%a -> %a" pp_ident x (pp_term_wp_j (k+1)) a pp_term b

and pp_term_wp_j k fmt = function
  | Kind | Type _ | DB _ | Const _ as t -> pp_term_j k fmt t
  | t       -> fprintf fmt "(%a)" (pp_term_j k) t

(* TODO the term is traversed three times, this could be optimized. *)
let subst_context (sub:SS.t) (ctx:typed_context) : typed_context option =
  try Some ( List.mapi ( fun i (l,x,ty) ->
      (l,x, Subst.unshift (i+1) (SS.apply sub (Subst.shift (i+1) ty) 0) )
    ) ctx )
  with
  | Subst.UnshiftExn -> None

let check_rule sg (rule:untyped_rule) : typed_rule =
  (*  let ctx0,le,ri = rule.rule in *)
  let delta = pc_make rule.ctx in
  let (ty_le,delta,lst) = infer_pattern sg delta LList.nil [] rule.pat in
  assert ( delta.padding == 0 );
  let sub = match pseudo_u sg SS.identity lst with
    | None -> raise (TypingError (CannotSolveConstraints (rule,lst)))
    | Some s -> ( (*debug "%a" SS.pp s;*) s )
  in
  let sub = SS.mk_idempotent sub in
  let (ri2,ty_le2,ctx2) =
    if SS.is_identity sub then (rule.rhs,ty_le,LList.lst delta.pctx)
    else
      begin
        match subst_context sub (LList.lst delta.pctx) with
        | Some ctx2 -> ( SS.apply sub rule.rhs 0, SS.apply sub ty_le 0, ctx2 )
        | None ->
          begin
            (*TODO make Dedukti handle this case*)
            debug 1 "Failed to infer a typing context for the rule:\n%a."
              pp_untyped_rule rule;
            SS.iter (
              fun i (id,te) -> debug 2 "Try replacing '%a[%i]' by '%a'"
                  pp_ident id i (pp_term_j 0) te
            ) sub;
            raise (TypingError (NotImplementedFeature (get_loc_pat rule.pat) ) )
          end
      end
  in
  check sg ctx2 ri2 ty_le2;
  debug 2 "[ %a ] %a --> %a" pp_context_inline ctx2 pp_pattern rule.pat pp_term ri2;
  { name = rule.name;
    ctx = ctx2;
    pat = rule.pat;
    rhs = rule.rhs
  }
