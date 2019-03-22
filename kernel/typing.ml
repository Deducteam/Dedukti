open Basic
open Format
open Rule
open Term
open Reduction

type Debug.flag += D_typeChecking | D_rule
let _ = Debug.register_flag D_typeChecking "TypeChecking"
let _ = Debug.register_flag D_rule         "Rule"

let coc = ref false

let fail_on_unsatisfiable_constraints = ref false

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
  | UnsatisfiableConstraints of untyped_rule * (int * term * term)
  | BracketError1 of term * typed_context
  | BracketError2 of term * typed_context*term
  | FreeVariableDependsOnBoundVariable of loc * ident * int * typed_context * term
  | NotImplementedFeature of loc
  | Unconvertible of loc*term*term
  | Convertible of loc*term*term
  | Inhabit of loc*term*term

exception TypingError of typing_error

(* ********************** CONTEXT *)

let  snf = default_reduction  Snf
let whnf = default_reduction Whnf

let get_type ctx l x n =
  try let (_,_,ty) = List.nth ctx n in Subst.shift (n+1) ty
  with Failure _ -> raise (TypingError (VariableNotFound (l,x,n,ctx)))

let extend_ctx a ctx = function
  | Type _ -> a::ctx
  | Kind when !coc -> a::ctx
  | ty_a ->
    let (_,_,te) = a in
    raise (TypingError (ConvertibilityError (te, ctx, mk_Type dloc, ty_a)))

(* ********************** TYPE CHECKING/INFERENCE FOR TERMS  *)

let rec infer sg (ctx:typed_context) (te:term) : typ =
  Debug.(debug D_typeChecking "Inferring: %a" pp_term te);
  match te with
  | Kind -> raise (TypingError KindIsNotTypable)
  | Type l -> mk_Kind
  | DB (l,x,n) -> get_type ctx l x n
  | Const (l,cst) -> Signature.get_type sg l cst
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
  Debug.(debug D_typeChecking "Checking (%a): %a : %a" pp_loc (get_loc te) pp_term te pp_term ty_exp);
  match te with
  | Lam (l,x,None,b) ->
    begin
      match whnf sg ty_exp with
      | Pi (_,_,a,ty_b) -> check sg ((l,x,a)::ctx) b ty_b
      | _ -> raise (TypingError (ProductExpected (te,ctx,ty_exp)))
    end
  | Lam (l,x,Some a,b) ->
    begin
      match whnf sg ty_exp with
      | Pi (_,_,a',ty_b) ->
        ignore(infer sg ctx a);
        if not (Reduction.are_convertible sg a a')
        then raise (TypingError (ConvertibilityError ((mk_DB l x 0),ctx,a',a)))
        else check sg ((l,x,a)::ctx) b ty_b
      | _ -> raise (TypingError (ProductExpected (te,ctx,ty_exp)))
    end
  | _ ->
    let ty_inf = infer sg ctx te in
    Debug.(debug D_typeChecking "Checking convertibility: %a ~ %a"
             pp_term ty_inf pp_term ty_exp);
    if not (Reduction.are_convertible sg ty_inf ty_exp) then
      let ty_exp' = rename_vars_with_typed_context ctx ty_exp in
      raise (TypingError (ConvertibilityError (te,ctx,ty_exp',ty_inf)))

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

let rec pseudo_u sg (fail: int*term*term-> unit) (sigma:SS.t) : (int*term*term) list -> SS.t = function
  | [] -> sigma
  | (q,t1,t2)::lst ->
    begin
      let t1' = whnf sg (SS.apply sigma q t1) in
      let t2' = whnf sg (SS.apply sigma q t2) in
      let keepon () = pseudo_u sg fail sigma lst in
      if term_eq t1' t2' then keepon ()
      else
        let warn () = fail (q,t1,t2); keepon () in
        match t1', t2' with
        | Kind, Kind | Type _, Type _       -> assert false (* Equal terms *)
        | DB (_,_,n), DB (_,_,n') when n=n' -> assert false (* Equal terms *)
        | _, Kind | Kind, _ |_, Type _ | Type _, _ -> warn ()

        | Pi (_,_,a,b), Pi (_,_,a',b') ->
          pseudo_u sg fail sigma ((q,a,a')::(q+1,b,b')::lst)
        | Lam (_,_,_,b), Lam (_,_,_,b') ->
          pseudo_u sg fail sigma ((q+1,b,b')::lst)

        (* Potentially eta-equivalent terms *)
        | Lam (_,i,_,b), a when !Reduction.eta ->
          let b' = mk_App (Subst.shift 1 a) (mk_DB dloc i 0) [] in
          pseudo_u sg fail sigma ((q+1,b,b')::lst)
        | a, Lam (_,i,_,b) when !Reduction.eta ->
          let b' = mk_App (Subst.shift 1 a) (mk_DB dloc i 0) [] in
          pseudo_u sg fail sigma ((q+1,b,b')::lst)

        | Const (_,c), Const (_,c') when name_eq c c' -> keepon ()
        | Const (l,cst), t when not (Signature.is_static sg l cst) ->
          ( match unshift_reduce sg q t with None -> warn () | Some _ -> keepon ())
        | t, Const (l,cst) when not (Signature.is_static sg l cst) ->
          ( match unshift_reduce sg q t with None -> warn () | Some _ -> keepon ())

        | DB (l1,x1,n1), DB (l2,x2,n2) when n1>=q && n2>=q ->
           let (n,t) = if n1<n2
                       then (n1,mk_DB l2 x2 (n2-q))
                       else (n2,mk_DB l1 x1 (n1-q)) in
           pseudo_u sg fail (SS.add sigma (n-q) t) lst
        | DB (_,_,n), t when n>=q ->
          begin
            let n' = n-q in
            match unshift_reduce sg q t with
            | None -> warn ()
            | Some ut ->
               let t' = if Subst.occurs n' ut then ut else snf sg ut in
               if Subst.occurs n' t' then warn ()
               else pseudo_u sg fail (SS.add sigma n' t') lst
          end
        | t, DB (_,_,n) when n>=q ->
          begin
            let n' = n-q in
            match unshift_reduce sg q t with
            | None -> warn ()
            | Some ut ->
               let t' = if Subst.occurs n' ut then ut else snf sg ut in
               if Subst.occurs n' t' then warn ()
               else pseudo_u sg fail (SS.add sigma n' t') lst
          end

        | App (DB (_,_,n),_,_), _  when n >= q ->
          if Reduction.are_convertible sg t1' t2' then keepon () else warn ()
        | _ , App (DB (_,_,n),_,_) when n >= q ->
          if Reduction.are_convertible sg t1' t2' then keepon () else warn ()

        | App (Const (l,cst),_,_), _ when not (Signature.is_static sg l cst) -> keepon ()
        | _, App (Const (l,cst),_,_) when not (Signature.is_static sg l cst) -> keepon ()

        | App (f,a,args), App (f',a',args') ->
          (* f = Kind | Type | DB n when n<q | Pi _
           * | Const name when (is_static name) *)
          begin
            match safe_add_to_list q lst args args' with
            | None -> warn () (* Different number of arguments. *)
            | Some lst2 -> pseudo_u sg fail sigma ((q,f,f')::(q,a,a')::lst2)
          end

        | _, _ -> warn ()
    end

(* **** TYPE CHECKING/INFERENCE FOR PATTERNS ******************************** *)

type constraints = (int * term * term) list
type context2    = (loc * ident * typ) LList.t

(* Partial Context *)

type partial_context =
  { padding : int;     (* expected size   *)
    pctx    : context2 (* partial context *)
  }

let pc_make (ctx:(loc*ident) list) : partial_context =
  let size = List.length ctx in
  assert ( size >= 0 );
  { padding=size; pctx=LList.nil }

let pc_in (delta:partial_context) (n:int) : bool = n >= delta.padding

let pc_get (delta:partial_context) (n:int) : term =
  let (_,_,ty) = LList.nth delta.pctx (n-delta.padding)
  in Subst.shift (n+1) ty

let pc_add (delta:partial_context) (n:int) (l:loc) (id:ident) (ty0:typ) : partial_context =
  assert ( n == delta.padding-1 && n >= 0 );
  let ty = Subst.unshift (n+1) ty0 in
  { padding = delta.padding - 1;
    pctx = LList.cons (l,id,ty) delta.pctx }

let pc_to_context (delta:partial_context) : typed_context = LList.lst delta.pctx

let pc_to_context_wp (delta:partial_context) : typed_context =
  let dummy = (dloc, dmark, mk_DB dloc dmark (-1)) in
  let rec aux lst = function 0 -> lst | n -> aux (dummy::lst) (n-1) in
  aux (pc_to_context delta) delta.padding

let pp_pcontext fmt delta =
  let lst = List.rev (LList.lst delta.pctx) in
  List.iteri (fun i (_,x,ty) -> fprintf fmt "%a[%i]:%a\n" pp_ident x i pp_term ty) lst;
  for i = 0 to delta.padding -1 do
    fprintf fmt "?[%i]:?\n" (i+LList.len delta.pctx)
  done

(* *** *)

let get_last =
  let rec aux acc = function
  | [] -> assert false
  | [a] -> (List.rev acc, a)
  | hd::tl -> aux (hd::acc) tl in
  aux []

let unshift_n sg n te =
  try Subst.unshift n te
  with Subst.UnshiftExn -> Subst.unshift n (snf sg te)

let rec infer_pattern sg (delta:partial_context) (sigma:context2)
    (lst:constraints) (pat:pattern) : typ * partial_context * constraints =
  match pat with
  | Pattern (l,cst,args) ->
    let (_,ty,delta2,lst2) = List.fold_left (infer_pattern_aux sg sigma)
        ( mk_Const l cst , Signature.get_type sg l cst , delta , lst ) args
    in (ty,delta2,lst2)
  | Var (l,x,n,args) when n < LList.len sigma ->
    let (_,ty,delta2,lst2) = List.fold_left (infer_pattern_aux sg sigma)
        ( mk_DB l x n, get_type (LList.lst sigma) l x n , delta , lst ) args
    in (ty,delta2,lst2)
  | Var _ | Brackets _ | Lambda _ ->
    let ctx = (LList.lst sigma)@(pc_to_context_wp delta) in
    raise (TypingError (CannotInferTypeOfPattern (pat,ctx)))

and infer_pattern_aux sg (sigma:context2)
    (f,ty_f,delta,lst:term*typ*partial_context*constraints)
    (arg:pattern) : term * typ * partial_context * constraints =
  match whnf sg ty_f with
    | Pi (_,_,a,b) ->
        let (delta2,lst2) = check_pattern sg delta sigma a lst arg in
        let arg' = pattern_to_term arg in
        ( Term.mk_App f arg' [], Subst.subst b arg', delta2 , lst2 )
    | ty_f ->
      let ctx = (LList.lst sigma)@(pc_to_context_wp delta) in
      raise (TypingError (ProductExpected (f,ctx,ty_f)))

and check_pattern sg (delta:partial_context) (sigma:context2) (exp_ty:typ)
    (lst:constraints) (pat:pattern) : partial_context * constraints =
  Debug.(debug D_rule "Checking pattern %a:%a" pp_pattern pat pp_term exp_ty);
  match pat with
  | Lambda (l,x,p) ->
    begin
      match whnf sg exp_ty with
      | Pi (_,_,a,b) -> check_pattern sg delta (LList.cons (l,x,a) sigma) b lst p
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
  | Var (l,x,n,[]) when n >= LList.len sigma ->
    begin
      let k = LList.len sigma in
      if pc_in delta (n-k)
      then
        let inf_ty = Subst.shift k (pc_get delta (n-k)) in
        ( delta, (k,inf_ty,exp_ty)::lst )
      else
        ( try ( pc_add delta (n-k) l x (unshift_n sg k exp_ty), lst )
          with Subst.UnshiftExn ->
            let ctx = (LList.lst sigma)@(pc_to_context_wp delta) in
            raise (TypingError (FreeVariableDependsOnBoundVariable (l,x,n,ctx,exp_ty))) )
    end
  | Var (l,x,n,args) when n >= LList.len sigma ->
    begin
      let (args2, last) = get_last args in
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

(* TODO the term is traversed three times, this could be optimized. *)
let subst_context (sub:SS.t) (ctx:typed_context) : typed_context =
  List.mapi ( fun i (l,x,ty) ->
              (l,x, Subst.unshift (i+1) (SS.apply sub 0 (Subst.shift (i+1) ty)) )
    ) ctx

let check_rule sg (rule:untyped_rule) : SS.t * typed_rule =
  let fail = if !fail_on_unsatisfiable_constraints
    then (fun x -> raise (TypingError (UnsatisfiableConstraints (rule,x))))
    else (fun (q,t1,t2) ->
        Debug.(debug D_warn "Unsatisfiable constraint: %a ~ %a%s"
                 pp_term t1 pp_term t2
                 (if q > 0 then Format.sprintf " (under %i abstractions)" q else ""))) in
  let delta = pc_make rule.ctx in
  let (ty_le,delta,lst) = infer_pattern sg delta LList.nil [] rule.pat in
  assert ( delta.padding == 0 );
  let sub = SS.mk_idempotent (pseudo_u sg fail SS.identity lst) in
  let ri2    = SS.apply sub 0 rule.rhs in
  let ty_le2 = SS.apply sub 0 ty_le    in
  let ctx = LList.lst delta.pctx in
  let ctx2 =
    if SS.is_identity sub then ctx
    else try subst_context sub ctx
         with Subst.UnshiftExn -> (* TODO make Dedukti handle this case *)
           Debug.(
                debug D_rule "Failed to infer a typing context for the rule:\n%a"
                  pp_untyped_rule rule;
                let ctx_name n = let _,name,_ = List.nth ctx n in name in
                debug D_rule "Tried inferred typing substitution: %a" (SS.pp ctx_name) sub);
        raise (TypingError (NotImplementedFeature (get_loc_pat rule.pat) ) )
  in
  check sg ctx2 ri2 ty_le2;
  Debug.(debug D_rule "[ %a ] %a --> %a"
           pp_context_inline ctx2 pp_pattern rule.pat pp_term ri2);
  sub,
  { name = rule.name;
    ctx = ctx2;
    pat = rule.pat;
    rhs = rule.rhs
  }

let typed_rule_of_rule_infos s ri =
  let ur =
    { name = ri.name
    ; ctx  = infer_rule_context ri
    ; pat  = pattern_of_rule_infos ri
    ; rhs  = ri.rhs} in
  check_rule s ur
