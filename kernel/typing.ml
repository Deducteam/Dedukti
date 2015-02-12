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

(* ********************** RULE TYPECHECKING  *)

let check_rule sg (ctx0,pat,rhs:rule) : unit =
  let ctx =
    List.fold_left (fun ctx (l,id,ty) -> Context.add l id (infer sg ctx ty) )
      Context.empty (List.rev ctx0) in
  let jl = infer sg ctx (pattern_to_term pat) in
  let jr = infer sg ctx rhs in
    if not (Reduction.are_convertible sg jl.ty jr.ty) then
      raise (TypingError (ConvertibilityError (rhs,ctx0,jl.ty,jr.ty)))

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
