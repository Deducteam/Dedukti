open Basics
open Term
open Rule

let coc = ref false
let errors_in_snf = ref false (*FIXME desactivated*)

type 'a judgment0 = { ctx:'a; te:term; ty: term; }
type rule_judgment = context * pattern * term

(* ********************** ERROR MESSAGES *)

let pp_context out = function
  | [] -> ()
  | ctx -> Printf.fprintf out " in context:\n%a" Pp.pp_context ctx

let error_convertibility te ctx exp inf =
(*   let exp = if !errors_in_snf then Reduction.snf exp else exp in *)
(*   let inf = if !errors_in_snf then Reduction.snf inf else inf in *)
    Print.fail (get_loc te)
      "Error while typing '%a'%a.\nExpected: %a\nInferred: %a."
      Pp.pp_term te pp_context ctx Pp.pp_term exp Pp.pp_term inf

let error_sort_expected te ctx inf =
(*   let inf = if !errors_in_snf then Reduction.snf inf else inf in *)
    Print.fail (get_loc te)
      "Error while typing '%a'%a.\nExpected: a sort.\nInferred: %a."
      Pp.pp_term te pp_context ctx Pp.pp_term inf

let error_product_expected te ctx inf =
(*   let inf = if !errors_in_snf then Reduction.snf inf else inf in *)
    Print.fail (get_loc te)
      "Error while typing '%a'%a.\nExpected: a product type.\nInferred: %a."
      Pp.pp_term te pp_context ctx Pp.pp_term inf

let error_inexpected_kind te ctx =
  Print.fail (get_loc te)
    "Error while typing '%a'%a.\nExpected: anything but Kind.\nInferred: Kind."
    Pp.pp_term te pp_context ctx

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

let rec infer sg (ctx:Context.t) : term -> judgment = function
  | Kind -> Print.fail dloc "Kind is not typable."
  | Type l ->
      { ctx=ctx; te=mk_Type l; ty= mk_Kind; }
  | DB (l,x,n) ->
      { ctx=ctx; te=mk_DB l x n; ty= Context.get_type ctx l x n }
  | Const (l,md,id) ->
      { ctx=ctx; te=mk_Const l md id; ty= Signature.get_type sg l md id; }
  | App (f,a,args) ->
      List.fold_left (check_app sg) (infer sg ctx f) (a::args)
  | Pi (l,x,a,b) ->
      let jdg_a = infer sg ctx a in
      let jdg_b = infer sg (Context.add l x jdg_a) b in
        ( match jdg_b.ty with
            | Kind | Type _ as ty -> { ctx=ctx; te=mk_Pi l x a jdg_b.te; ty=ty }
            | _ -> error_sort_expected jdg_b.te
                     (Context.to_context jdg_b.ctx) jdg_b.ty
        )
  | Lam  (l,x,Some a,b) ->
      let jdg_a = infer sg ctx a in
      let jdg_b = infer sg (Context.add l x jdg_a) b in
        ( match jdg_b.ty with
            | Kind ->
                error_inexpected_kind jdg_b.te (Context.to_context jdg_b.ctx)
            | _ -> { ctx=ctx; te=mk_Lam l x (Some a) jdg_b.te;
                     ty=mk_Pi l x a jdg_b.ty }
        )
  | Lam  (l,x,None,b) ->
      Print.fail l "Cannot infer the type of domain-free lambda."

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
              | _ ->
                  error_product_expected te (Context.to_context jty.ctx) jty.te
          )
      | _ ->
        let jte = infer sg ctx te in
          if Reduction.are_convertible sg jte.ty ty_exp then
            { ctx=ctx; te=te; ty=ty_exp; }
          else
            error_convertibility te (Context.to_context ctx) ty_exp jte.ty

and check_app sg jdg_f arg =
  match Reduction.whnf sg jdg_f.ty with
    | Pi (_,_,a,b) ->
        (* (x) might be Kind if CoC flag is on but it does not matter here *)
        let _ = check sg arg { ctx=jdg_f.ctx; te=a; ty=mk_Type dloc (* (x) *); } in
          { ctx=jdg_f.ctx; te=mk_App jdg_f.te arg []; ty=Subst.subst b arg; }
    | _ ->
        error_product_expected jdg_f.te (Context.to_context jdg_f.ctx) jdg_f.ty

let inference sg (te:term) : judgment = infer sg Context.empty te

let checking sg (te:term) (ty_exp:term) : judgment =
  let jty = infer sg Context.empty ty_exp in
    check sg te jty

(* ********************** RULE TYPECHECKING  *)

let check_rule sg (ctx0,pat,rhs:rule) : rule_judgment =
  let ctx =
    List.fold_left (fun ctx (l,id,ty) -> Context.add l id (infer sg ctx ty) )
      Context.empty (List.rev ctx0) in
  let jl = infer sg ctx (pattern_to_term pat) in
  let jr = infer sg ctx rhs in
    if Reduction.are_convertible sg jl.ty jr.ty then
      ( ctx0, pat, rhs )
    else
      error_convertibility rhs ctx0 jl.ty jr.ty

(* ********************** SAFE REDUCTION *)
(*
let whnf j = { j with te= Reduction.whnf j.te }
let hnf j = { j with te= Reduction.hnf j.te }
let snf j = { j with te= Reduction.snf j.te }
let one j = match Reduction.one_step j.te with
  | Some te2 -> { j with te=te2 }
  | None -> j

let conv_test j1 j2 = Reduction.are_convertible j1.te j2.te
let check_test j1 j2 = Reduction.are_convertible j1.ty j2.te

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

let check_contexts ctx1 ctx2 =
  if ctx1 != ctx2 then raise (JudgmentExn DistinctContexts)

let mk_Type ctx l = { ctx=ctx; te=mk_Type l; ty= mk_Kind; }

let mk_Const ctx l md id =
  { ctx=ctx; te=mk_Const l md id; ty= Env.get_type l md id; }

 let mk_Var ctx l x n =
  { ctx=ctx; te=mk_DB l x n; ty= Context.get_type ctx l x n }

let mk_App f arg =
  check_contexts f.ctx arg.ctx ;
  match Reduction.whnf f.ty with
    | Pi (_,_,a,b) ->
        if Reduction.are_convertible a arg.ty then
          { ctx=f.ctx; te=mk_App f.te arg.te []; ty=Subst.subst b arg.te; }
        else raise (JudgmentExn AppNotConvertible)
    | _ -> raise (JudgmentExn AppNotAPi)


let mk_Lam b =
  match b.ty with
    | Kind -> raise (JudgmentExn LambdaKind)
    | _ ->
        begin
          match b.ctx with
            | (l,x,a)::lst ->
                { ctx=lst; te=mk_Lam l x (Some a) b.te; ty=mk_Pi l x a b.ty }
            | _ -> raise (JudgmentExn LambdaEmptyContext)
        end

let mk_Pi b =
  match b.ty with
    | Kind | Type _ as ty ->
        begin
          match b.ctx with
            | (l,x,a)::lst -> { ctx=lst; te=mk_Pi l x a b.te; ty=ty }
            | _ -> raise (JudgmentExn PiEmptyContext)
        end
    | _ -> raise (JudgmentExn PiSort)

let mk_Conv a b =
  check_contexts a.ctx b.ctx;
  match b.ty with
    | Kind | Type _ ->
        if Reduction.are_convertible a.ty b.te then
          { ctx=a.ctx; te=a.te; ty=b.te }
        else raise (JudgmentExn ConvError)
    | _ -> raise (JudgmentExn ConvSort)
 *)
