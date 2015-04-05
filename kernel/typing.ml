open Basics
open Term
open Rule

let coc = ref false

type 'a judgment0 = { ctx:'a; te:term; ty: term; }

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

module Context :
sig
  type t
  val empty : t
  val add : loc -> ident -> t judgment0 -> t
  val unsafe_add : t -> loc -> ident -> term -> t
  val get_type : t -> loc -> ident -> int -> term
  val is_empty : t -> bool
  val to_context : t -> (loc*ident*term) list
  val destruct : t -> ((loc*ident*term)*t) option
end =
struct
  type t = (loc*ident*term) list

  let empty = []
  let is_empty ctx = ( ctx=[] )
  let to_context ctx = ctx

  let add l x jdg : context =
    match jdg.ty with
      | Type _ -> (l,x,jdg.te) :: jdg.ctx
      | Kind when !coc -> (l,x,jdg.te) :: jdg.ctx
      (*Note that this is the only place where the coc flag has an effect *)
      | _ -> raise (TypingError (ConvertibilityError
                                   (jdg.te, to_context jdg.ctx, mk_Type dloc, jdg.ty)))

  let unsafe_add ctx l x ty = (l,x,ty)::ctx

  let get_type ctx l x n =
    try
      let (_,_,ty) = List.nth ctx n in Subst.shift (n+1) ty
    with Failure _ ->
      raise (TypingError (VariableNotFound (l,x,n,ctx)))

  let destruct = function
    | [] -> None
    | a::b -> Some (a,b)
end

type judgment = Context.t judgment0

(* ********************** TYPE CHECKING/INFERENCE FOR TERMS  *)

let rec infer sg (ctx:Context.t) : term -> judgment = function
  | Kind -> raise (TypingError KindIsNotTypable)
  | Type l ->
      { ctx=ctx; te=mk_Type l; ty= mk_Kind; }
  | DB (l,x,n) ->
      { ctx=ctx; te=mk_DB l x n; ty= Context.get_type ctx l x n }
  | Const (l,md,id) ->
      { ctx=ctx; te=mk_Const l md id; ty=Signature.get_type sg l md id; }
  | App (f,a,args) ->
      List.fold_left (check_app sg) (infer sg ctx f) (a::args)
  | Pi (l,x,a,b) ->
      let jdg_a = infer sg ctx a in
      let jdg_b = infer sg (Context.add l x jdg_a) b in
        ( match jdg_b.ty with
            | Kind | Type _ as ty -> { ctx=ctx; te=mk_Pi l x a jdg_b.te; ty=ty }
            | _ -> raise (TypingError
                            (SortExpected (jdg_b.te, Context.to_context jdg_b.ctx, jdg_b.ty)))
        )
  | Lam  (l,x,Some a,b) ->
      let jdg_a = infer sg ctx a in
      let jdg_b = infer sg (Context.add l x jdg_a) b in
        ( match jdg_b.ty with
            | Kind -> raise (TypingError
                               (InexpectedKind (jdg_b.te, Context.to_context jdg_b.ctx)))
            | _ -> { ctx=ctx; te=mk_Lam l x (Some a) jdg_b.te;
                     ty=mk_Pi l x a jdg_b.ty }
        )
  | Lam  (l,x,None,b) -> raise (TypingError (DomainFreeLambda l))

and check sg (te:term) (jty:judgment) : judgment =
  let ty_exp = jty.te in
  let ctx = jty.ctx in
    match te with
      | Lam (l,x,None,u) ->
          ( match Reduction.whnf sg jty.te with
              | Pi (_,_,a,b) as pi ->
                  let ctx2 = Context.unsafe_add ctx l x a in
                  (* (x) might as well be Kind but here we do not care*)
                  let _ = check sg u { ctx=ctx2; te=b; ty=mk_Type dloc (* (x) *); } in
                    { ctx=ctx; te=mk_Lam l x None u; ty=pi; }
              | _ -> raise (TypingError
                              (ProductExpected (te,Context.to_context jty.ctx,jty.te)))
          )
      | _ ->
        let jte = infer sg ctx te in
          if Reduction.are_convertible sg jte.ty ty_exp then
            { ctx=ctx; te=te; ty=ty_exp; }
          else
            raise (TypingError (
              ConvertibilityError (te,Context.to_context ctx,ty_exp,jte.ty)))

and check_app sg jdg_f arg =
  match Reduction.whnf sg jdg_f.ty with
    | Pi (_,_,a,b) ->
        (* (x) might be Kind if CoC flag is on but it does not matter here *)
        let _ = check sg arg { ctx=jdg_f.ctx; te=a; ty=mk_Type dloc (* (x) *); } in
          { ctx=jdg_f.ctx; te=mk_App jdg_f.te arg []; ty=Subst.subst b arg; }
    | _ ->
        raise (TypingError (
          ProductExpected (jdg_f.te,Context.to_context jdg_f.ctx,jdg_f.ty)))

let inference sg (te:term) : judgment =
  infer sg Context.empty te

let checking sg (te:term) (ty_exp:term) : judgment =
  let jty = infer sg Context.empty ty_exp in
    check sg te jty

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

let rec infer_pattern sg (ctx:Context.t) (q:int) (sigma:SS.t) (pat:pattern) : term*SS.t =
  match pat with
  | Pattern (l,md,id,args) ->
    let (_,ty,si) = List.fold_left (infer_pattern_aux sg ctx q)
        (mk_Const l md id,SS.apply sigma (Signature.get_type sg l md id) q,sigma) args in
    (ty,si)
  | Var (l,x,n,args) ->
    let (_,ty,si) = List.fold_left (infer_pattern_aux sg ctx q)
        (mk_DB l x n,SS.apply sigma (Context.get_type ctx l x n) q,sigma) args in
    (ty,si)
  | Brackets t -> ( (infer sg ctx t).ty, SS.identity )
  | Lambda (l,x,p) -> raise (TypingError (DomainFreeLambda l))

and infer_pattern_aux sg (ctx:Context.t) (q:int) (f,ty_f,sigma0:term*term*SS.t) (arg:pattern) : term*term*SS.t =
  match Reduction.whnf sg ty_f with
    | Pi (_,_,a,b) ->
        let sigma = check_pattern sg ctx q a sigma0 arg in
        let arg' = pattern_to_term arg in
        let b2 = SS.apply sigma b (q+1) in
        let arg2 = SS.apply sigma arg' q in
        ( Term.mk_App f arg' [], Subst.subst b2 arg2, sigma )
    | ty_f -> raise (TypingError ( ProductExpected (f,Context.to_context ctx,ty_f)))

and check_pattern sg (ctx:Context.t) (q:int) (exp_ty:term) (sigma0:SS.t) (pat:pattern) : SS.t =
  match pat with
  | Lambda (l,x,p) ->
      begin
        match Reduction.whnf sg exp_ty with
          | Pi (l,x,a,b) ->
              let ctx2 = Context.unsafe_add ctx l x a in
                check_pattern sg ctx2 (q+1) b sigma0 p
          | exp_ty -> raise (TypingError ( ProductExpected (pattern_to_term pat,Context.to_context ctx,exp_ty)))
      end
   | Brackets t ->
     ( ignore (check sg t { ctx; te=exp_ty; ty=Term.mk_Type dloc; });
       SS.identity )
  | _ ->
      begin
        let (inf_ty,sigma1) = infer_pattern sg ctx q sigma0 pat in
          match pseudo_unification sg q exp_ty inf_ty with
            | None ->
              raise (TypingError (ConvertibilityError (pattern_to_term pat,Context.to_context ctx,exp_ty,inf_ty)))
            | Some sigma2 -> SS.merge sigma1 sigma2
      end

(* ************************************************************************** *)

let check_rule sg (ctx,le,ri:rule) : unit =
  let ctx =
    List.fold_left (fun ctx (l,id,ty) -> Context.add l id (infer sg ctx ty) )
      Context.empty (List.rev ctx) in
  let (ty_inf,sigma) = infer_pattern sg ctx 0 SS.identity le in
  let ri2 =
    if SS.is_identity sigma then ri
    else ( debug "%a" SS.pp sigma ; (SS.apply sigma ri 0) ) in
  let j_ri = infer sg ctx ri2 in
  if not (Reduction.are_convertible sg ty_inf j_ri.ty) then
    raise (TypingError (ConvertibilityError (ri,Context.to_context ctx,ty_inf,j_ri.ty)))

(* ********************** JUDGMENTS *)

type judgmentExn =
  | DistinctContexts
  | LambdaKind
  | LambdaEmptyContext
  | PiSort
  | PiEmptyContext
  | AppNotAPi
  | AppNotConvertible
  | ConvSort
  | ConvError

exception JudgmentExn of judgmentExn
(* TODO check also the signature *)

let check_contexts ctx1 ctx2 =
  if ctx1 != ctx2 then raise (JudgmentExn DistinctContexts)

let mk_Type ctx l = { ctx=ctx; te=mk_Type l; ty= mk_Kind; }

let mk_Const sg ctx l md id =
  { ctx=ctx; te=mk_Const l md id; ty= Signature.get_type sg l md id; }

 let mk_Var ctx l x n =
  { ctx=ctx; te=mk_DB l x n; ty= Context.get_type ctx l x n }

let mk_App sg f arg =
  check_contexts f.ctx arg.ctx ;
  match Reduction.whnf sg f.ty with
    | Pi (_,_,a,b) ->
        if Reduction.are_convertible sg a arg.ty then
          { ctx=f.ctx; te=mk_App f.te arg.te []; ty=Subst.subst b arg.te; }
        else raise (JudgmentExn AppNotConvertible)
    | _ -> raise (JudgmentExn AppNotAPi)

let mk_Lam b =
  match b.ty with
    | Kind -> raise (JudgmentExn LambdaKind)
    | _ ->
        begin
          match Context.destruct b.ctx with
            | Some ((l,x,a),ctx') ->
                { ctx=ctx'; te=mk_Lam l x (Some a) b.te; ty=mk_Pi l x a b.ty }
            | None -> raise (JudgmentExn LambdaEmptyContext)
        end

let mk_Pi b =
  match b.ty with
    | Kind | Type _ as ty ->
        begin
          match Context.destruct b.ctx with
            | Some ((l,x,a),ctx') -> { ctx=ctx'; te=mk_Pi l x a b.te; ty=ty }
            | None -> raise (JudgmentExn PiEmptyContext)
        end
    | _ -> raise (JudgmentExn PiSort)

let mk_Conv sg a b =
  check_contexts a.ctx b.ctx;
  match b.ty with
    | Kind | Type _ ->
        if Reduction.are_convertible sg a.ty b.te then
          { ctx=a.ctx; te=a.te; ty=b.te }
        else raise (JudgmentExn ConvError)
    | _ -> raise (JudgmentExn ConvSort)
