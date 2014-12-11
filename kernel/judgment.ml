open Basics
open Term
open Rule

let coc = ref false

type 'a judgment0 = { ctx:'a; let_ctx : LetCtx.t; te:term; ty: term; }
type rule_judgment = context * pattern * term

(* ********************** ERROR MESSAGES *)

let error_convertibility te ctx exp inf =
  if ctx = [] then
    Print.fail (get_loc te)
      "Error while typing '%a'.\nExpected: %a\nInferred: %a."
      Pp.pp_term te Pp.pp_term exp Pp.pp_term inf
  else
    Print.fail (get_loc te)
      "Error while typing '%a' in context:\n%a.\nExpected: %a\nInferred: %a."
      Pp.pp_term te Pp.pp_context ctx Pp.pp_term exp
      Pp.pp_term inf

let error_sort_expected te ctx inf =
  if ctx = [] then
    Print.fail (get_loc te)
      "Error while typing '%a' in context:\n%a.\nExpected: a sort.\nInferred: %a."
      Pp.pp_term te Pp.pp_context ctx Pp.pp_term inf
  else
    Print.fail (get_loc te)
      "Error while typing '%a'.\nExpected: a sort.\nInferred: %a."
      Pp.pp_term te Pp.pp_term inf

let error_product_expected te ctx inf =
  if ctx = [] then
    Print.fail (get_loc te)
      "Error while typing '%a' in context:\n%a.\nExpected: a product type.\nInferred: %a."
      Pp.pp_term te Pp.pp_context ctx Pp.pp_term inf
  else
    Print.fail (get_loc te)
      "Error while typing '%a'.\nExpected: a product type.\nInferred: %a."
      Pp.pp_term te Pp.pp_term inf

let error_inexpected_kind te ctx =
  if ctx = [] then
    Print.fail (get_loc te)
      "Error while typing '%a' in context:\n%a.\nExpected: anything but Kind.\nInferred: Kind."
      Pp.pp_term te Pp.pp_context ctx
  else
    Print.fail (get_loc te)
      "Error while typing '%a'.\nExpected: anything but Kind.\nInferred: Kind."
      Pp.pp_term te

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
      | _ -> error_convertibility jdg.te
               (to_context jdg.ctx) (mk_Type dloc) jdg.ty

  let unsafe_add ctx l x ty = (l,x,ty)::ctx

  let get_type ctx l x n =
    try
      let (_,_,ty) = List.nth ctx n in Subst.shift (n+1) ty
    with Failure _ ->
      Print.fail l "The variable '%a' was not found in context:\n"
        Pp.pp_term (mk_DB l x n) Pp.pp_context ctx

end

type judgment = Context.t judgment0

(* ********************** TYPE CHECKING/INFERENCE FOR TERMS  *)

let rec infer let_ctx (ctx:Context.t) : term -> judgment = function
  | Kind -> Print.fail dloc "Kind is not typable."
  | Type l ->
      { ctx=ctx; te=mk_Type l; ty= mk_Kind; let_ctx }
  | DB (l,x,n) ->
      { ctx=ctx; te=mk_DB l x n; ty= Context.get_type ctx l x n; let_ctx; }
  | Const (l,md,id) ->
      { ctx=ctx; te=mk_Const l md id; ty= Env.get_type l md id; let_ctx; }
  | App (f,a,args) ->
      List.fold_left (check_app let_ctx) (infer let_ctx ctx f) (a::args)
  | Let (l, x, a, b) ->
      infer (LetCtx.cons (a,let_ctx) let_ctx) ctx b
  | Pi (l,x,a,b) ->
      let jdg_a = infer let_ctx ctx a in
      let jdg_b = infer (LetCtx.cons_none let_ctx) (Context.add l x jdg_a) b in
        ( match jdg_b.ty with
            | Kind | Type _ as ty ->
                { ctx=ctx; te=mk_Pi l x a jdg_b.te; ty; let_ctx; }
            | _ -> error_sort_expected jdg_b.te
                     (Context.to_context jdg_b.ctx) jdg_b.ty
        )
  | Lam  (l,x,Some a,b) ->
      let jdg_a = infer let_ctx ctx a in
      let jdg_b = infer (LetCtx.cons_none let_ctx) (Context.add l x jdg_a) b in
        ( match jdg_b.ty with
            | Kind ->
                error_inexpected_kind jdg_b.te (Context.to_context jdg_b.ctx)
            | _ ->
                { ctx=ctx; te=mk_Lam l x (Some a) jdg_b.te;
                  ty=mk_Pi l x a jdg_b.ty; let_ctx }
        )
  | Lam  (l,x,None,b) ->
      Print.fail l "Cannot infer the type of domain-free lambda."

and check let_ctx (te:term) (jty:judgment) : judgment =
  let ty_exp = jty.te in
  let ctx = jty.ctx in
    match te with
      | Lam (l,x,None,u) ->
          ( match Reduction.whnf let_ctx jty.te with
              | Pi (_,_,a,b) as pi ->
                  let ctx2 = Context.unsafe_add ctx l x a in
                  (* (x) might as well be Kind but here we do not care*)
                  let _ = check let_ctx u { ctx=ctx2; te=b; ty=mk_Type dloc (* (x) *); let_ctx } in
                    { ctx=ctx; te=mk_Lam l x None u; ty=pi; let_ctx }
              | _ ->
                  error_product_expected te (Context.to_context jty.ctx) jty.te
          )
      | _ ->
        let jte = infer let_ctx ctx te in
          if Reduction.are_convertible let_ctx jte.ty ty_exp then
            { ctx=ctx; te=te; ty=ty_exp; let_ctx }
          else
            error_convertibility te (Context.to_context ctx) ty_exp jte.ty

and check_app let_ctx jdg_f arg =
  match Reduction.whnf let_ctx jdg_f.ty with
    | Pi (_,_,a,b) ->
        (* (x) might be Kind if CoC flag is on but it does not matter here *)
        let _ = check let_ctx arg
          { ctx=jdg_f.ctx; te=a; ty=mk_Type dloc (* (x) *); let_ctx } in
        { ctx=jdg_f.ctx; te=mk_App jdg_f.te arg []; ty=Subst.subst b arg; let_ctx }
    | _ ->
        error_product_expected jdg_f.te (Context.to_context jdg_f.ctx) jdg_f.ty

let inference (te:term) : judgment = infer LetCtx.empty Context.empty te

let checking (te:term) (ty_exp:term) : judgment =
  let jty = infer LetCtx.empty Context.empty ty_exp in
    check jty.let_ctx te jty

(* ********************** RULE TYPECHECKING  *)

let check_rule (ctx0,pat,rhs:rule) : rule_judgment =
  let ctx =
    List.fold_left (fun ctx (l,id,ty) -> Context.add l id (infer LetCtx.empty ctx ty) )
      Context.empty (List.rev ctx0) (*FIXME*) in
  let jl = infer LetCtx.empty ctx (pattern_to_term pat) in
  let jr = infer LetCtx.empty ctx rhs in
    if Reduction.are_convertible LetCtx.empty jl.ty jr.ty then
      ( ctx0, pat, rhs )
    else
      error_convertibility rhs ctx0 jl.ty jr.ty

(* ********************** SAFE REDUCTION *)

let whnf j = { j with te= Reduction.whnf LetCtx.empty j.te }
let hnf j = { j with te= Reduction.hnf LetCtx.empty j.te }
let snf j = { j with te= Reduction.snf LetCtx.empty j.te }
let one j = match Reduction.one_step LetCtx.empty j.te with
  | Some te2 -> { j with te=te2 }
  | None -> j

let conv_test j1 j2 = Reduction.are_convertible LetCtx.empty j1.te j2.te
let check_test j1 j2 = Reduction.are_convertible LetCtx.empty j1.ty j2.te

(* ********************** SIGNATURE *)

let declare (l:loc) (id:ident) (jdg:judgment) =
  assert ( Context.is_empty jdg.ctx );
  match jdg.ty with
    | Kind | Type _ -> Env.declare l id jdg.te
    | _ -> error_sort_expected jdg.te [] jdg.ty

let define (l:loc) (id:ident) (jdg:judgment) =
  assert ( Context.is_empty jdg.ctx );
  Env.define l id jdg.te jdg.ty

let define_op (l:loc) (id:ident) (jdg:judgment) =
  assert( Context.is_empty jdg.ctx );
  Env.declare l id jdg.ty

let add_rules jdgs =
  Env.add_rules jdgs

let declare2 l id ty =
  declare l id (inference ty)

let define2 l id te ty_opt =
  match ty_opt with
    | None -> define l id (inference te)
    | Some ty -> define l id (checking te ty)

let define_op2 l id te ty_opt =
  match ty_opt with
    | None -> define_op l id (inference te)
    | Some ty -> define_op l id (checking te ty)

let add_rules2 rules =
  add_rules (List.map check_rule rules)

(*
let mk_Type ctx l = { ctx=ctx; te=mk_Type l; ty= mk_Kind; }

let mk_Const ctx l md id =
  { ctx=ctx; te=mk_Const l md id; ty= Env.get_type l md id; }

 let mk_Var ctx l x n =
  { ctx=ctx; te=mk_DB l x n; ty= ctx_get_type l x ctx n }

let mk_App f arg =
  assert ( f.ctx = arg.ctx ); (*FIXME*)
  match Reduction.whnf f.ty with
    | Pi (_,_,a,b) ->
        if Reduction.are_convertible a arg.ty then
          { ctx=f.ctx; te=mk_App f.te arg.te []; ty=Subst.subst b arg.te; }
        else assert false (*FIXME*)
    | _ -> assert false (*FIXME*)

let mk_Lam b =
  match b.ty with
    | Kind -> assert false (*FIXME*)
    | _ ->
        begin
          match b.ctx with
            | (l,x,a)::lst ->
                { ctx=lst; te=mk_Lam l x (Some a) b.te; ty=mk_Pi l x a b.ty }
            | _ -> assert false (*FIXME*)
        end

let mk_Pi b =
  match b.ty with
    | Kind | Type _ as ty ->
        begin
          match b.ctx with
            | (l,x,a)::lst -> { ctx=lst; te=mk_Pi l x a b.te; ty=ty }
            | _ -> assert false (*FIXME*)
        end
    | _ -> assert false (*FIXME*)

let mk_Conv a b =
  assert (a.ctx = b.ctx); (*FIXME*)
  match b.ty with
    | Kind | Type _ ->
        if Reduction.are_convertible a.ty b.te then
          { ctx=a.ctx; te=a.te; ty=b.te }
        else assert false (*FIXME*)
    | _ -> assert false (*FIXME*)
 *)
