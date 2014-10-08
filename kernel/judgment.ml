open Basics
open Term
open Rule

(* ********************** CONTEXT *)

type 'a judgment0 = { ctx:'a; te:term; ty: term; }

module Context :
sig
  type t
  val empty : t
  val add : loc -> ident -> t judgment0 -> t
  val unsafe_add : t -> loc -> ident -> term -> t
  val get_type : t -> loc -> ident -> int -> term
  val is_empty : t -> bool
end =
struct
  type t = (loc*ident*term) list

  let empty = []
  let is_empty ctx = ( ctx=[] )

  let add l x jdg : context =
    match jdg.ty with
      | Type _ -> (l,x,jdg.te) :: jdg.ctx
      | _ -> assert false (*FIXME*)

  let unsafe_add ctx l x ty = (l,x,ty)::ctx

  let get_type ctx l x n =
    try
      let (_,_,ty) = List.nth ctx n in Subst.shift (n+1) ty
    with Failure _ ->
      Print.fail l "The variable '%a' was not found in context:\n"
        Pp.pp_term (mk_DB l x n) Pp.pp_context ctx
end

(* ********************** JUDGMENT *)

type judgment = Context.t judgment0
type rule_judgment = context * pattern * term

(* ********************** TYPE CHECKING/INFERENCE FOR TERMS  *)

let rec infer (ctx:Context.t) : term -> judgment = function
  | Kind -> Print.fail dloc "Kind is not typable."
  | Type l ->
      { ctx=ctx; te=mk_Type l; ty= mk_Kind; }
  | DB (l,x,n) ->
      { ctx=ctx; te=mk_DB l x n; ty= Context.get_type ctx l x n }
  | Const (l,md,id) ->
      { ctx=ctx; te=mk_Const l md id; ty= Env.get_type l md id; }
  | App (f,a,args) ->
      List.fold_left check_app (infer ctx f) (a::args)
  | Pi (l,x,a,b) ->
      let jdg_a = infer ctx a in
      let jdg_b = infer (Context.add l x jdg_a) b in
        begin
          match jdg_b.ty with
            | Kind | Type _ as ty -> { ctx=ctx; te=mk_Pi l x a jdg_b.te; ty=ty }
            | _ -> assert false (*FIXME*)
        end
  | Lam  (l,x,Some a,b) ->
      let jdg_a = infer ctx a in
      let jdg_b = infer (Context.add l x jdg_a) b in
        begin
          match jdg_b.ty with
            | Kind -> assert false (*FIXME*)
            | _ -> { ctx=ctx; te=mk_Lam l x (Some a) jdg_b.te;
                     ty=mk_Pi l x a jdg_b.ty }
        end
  | Lam  (l,x,None,b) ->
      Print.fail l "Cannot infer the type of domain-free lambda."

and check (te:term) (jty:judgment) : judgment =
  let ty_exp = jty.te in
  let ctx = jty.ctx in
    match te with
      | Lam (l,x,None,u) ->
          begin
            match Reduction.whnf jty.te with
              | Pi (_,_,a,b) as pi ->
                  let ctx2 = Context.unsafe_add ctx l x a in
                  (* (x) might as well be Kind but here we do not care*)
                  let _ = check u { ctx=ctx2; te=b; ty=mk_Type dloc (* (x) *); } in
                    { ctx=ctx; te=mk_Lam l x None u; ty=pi; }
              | _ -> assert false (*FIXME*)
          end
      | _ ->
        let jte = infer ctx te in
          if Reduction.are_convertible jte.ty ty_exp then
            { ctx=ctx; te=te; ty=ty_exp; }
          else
            assert false (*FIXME*)

and check_app jdg_f arg =
  match Reduction.whnf jdg_f.ty with
    | Pi (_,_,a,b) ->
        let _ = check arg { ctx=jdg_f.ctx; te=a; ty=mk_Type dloc; } in
          { ctx=jdg_f.ctx; te=mk_App jdg_f.te arg []; ty=Subst.subst b arg; }
    | _ ->
        assert false (*FIXME*)

let inference (te:term) : judgment = infer Context.empty te

let checking (te:term) (ty_exp:term) : judgment =
  let jty = infer Context.empty ty_exp in
    check te jty

(* ********************** RULE TYPECHECKING  *)

let check_rule (ctx0,pat,rhs:rule) : rule_judgment =
  let ctx =
    List.fold_left (fun ctx (l,id,ty) -> Context.add l id (infer ctx ty) )
      Context.empty (List.rev ctx0) (*FIXME*) in
  let jl = infer ctx (pattern_to_term pat) in
  let jr = infer ctx rhs in
    if Reduction.are_convertible jl.ty jr.ty then
      ( ctx0, pat, rhs )
    else
      assert false (*FIXME*)

(* ********************** SAFE REDUCTION *)

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
  if Context.is_empty jdg.ctx then
    ( match jdg.ty with
      | Kind | Type _ -> Env.declare l id jdg.te
      | _ -> assert false (*FIXME*) )
  else
    assert false (*FIXME*)

let define (l:loc) (id:ident) (jdg:judgment) =
  if Context.is_empty jdg.ctx then
    Env.define l id jdg.te jdg.ty
  else
    assert false (*FIXME*)

let define_op (l:loc) (id:ident) (jdg:judgment) =
  if Context.is_empty jdg.ctx then
    Env.declare l id jdg.ty
  else
    assert false (*FIXME*)

let add_rules (jdgs:rule_judgment list) = Env.add_rules jdgs

let declare2 l id ty = declare l id (inference ty)

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
