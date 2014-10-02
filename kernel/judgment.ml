open Basics
open Term
open Rule

type context = (loc*ident*term) list
let c_of_c ctx = ctx
type judgment = { ctx:context; te:term; ty: term; }
type pattern_judgment = { pctx:context; pat:pattern; pty:term; }
type rule_judgment = context * pattern * term

(* ********************** CONTEXT *)

let declare l id jdg =
  assert ( jdg.ctx = [] ); (*FIXME*)
  match jdg.ty with
    | Kind |Type _ -> Env.declare l id jdg.te
    | _ -> assert false (*FIXME*)

let define l id jdg =
  assert ( jdg.ctx = [] ); (*FIXME*)
  Env.define l id jdg.te jdg.ty

let define_op l id jdg =
  assert ( jdg.ctx = [] ); (*FIXME*)
  Env.declare l id jdg.ty

let add_rules jdgs = Env.add_rules jdgs

(* ********************** CONTEXT *)

let ctx_empty = []

let ctx_add (l:loc) (x:ident) (jdg:judgment) : context =
    match jdg.ty with
      | Type _ -> (l,x,jdg.te) :: jdg.ctx
      | _ -> assert false (*FIXME*)

(* **********************  JUDGMENT *)

let mk_Type ctx l = { ctx=ctx; te=mk_Type l; ty= mk_Kind; }

let mk_Const ctx l md id =
  { ctx=ctx; te=mk_Const l md id; ty= Env.get_type l md id; }

let ctx_get_type l x ctx n =
  try
    let (_,_,ty) = List.nth ctx n in Subst.shift (n+1) ty
  with Failure _ -> Print.fail l "The variable '%a' was not found in context:\n"
                      Pp.pp_term (mk_DB l x n) Pp.pp_context ctx

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

let invert_Pi_judgment (pi:judgment) : judgment*judgment =
  match Reduction.whnf pi.te with
    | Pi (l,x,a,b) ->
        let jdg_a = { ctx=pi.ctx; te=a; ty=Term.mk_Type dloc; } in
          ( jdg_a , { ctx=ctx_add l x jdg_a; te=b; ty=pi.ty; } )
    | _ -> assert false (*FIXME*)

let get_expected_arg_type f =
  match Reduction.whnf f.ty with
    | Pi (l,x,a,b) -> { ctx=f.ctx; te=a; ty=Term.mk_Type dloc; }
    | _ -> assert false (*FIXME*)

(* **********************  PATTERN JUDGMENT *)

let mk_Pattern_aux (ctx:context) (ty_f:term) (arg:pattern_judgment) : term =
  assert ( ctx = arg.pctx ); (*FIXME*)
  match Reduction.whnf ty_f with
    | Pi (_,_,a,b) ->
        if Reduction.are_convertible a arg.pty then
          Subst.subst b (pattern_to_term arg.pat)
        else assert false (*FIXME*)
    | _ -> assert false (*FIXME*)

let mk_Pattern_Var ctx l x n =
  { pctx=ctx; pat=Var(l,x,n,[]); pty=ctx_get_type l x ctx n; }

let mk_Pattern_Const ctx l md id =
    { pctx=ctx; pat=Pattern(l,md,id,[]); pty=Env.get_type l md id }

let mk_Pattern_App f arg =
  assert ( f.pctx = arg.pctx ); (*FIXME*)
  match Reduction.whnf f.pty with
    | Pi (_,_,a,b) ->
        if Reduction.are_convertible a arg.pty then
          let pat =
            match f.pat with
              | Var (l,x,n,args) -> Var(l,x,n,args@[arg.pat])
              | Pattern (l,md,id,args) -> Pattern(l,md,id,args@[arg.pat])
              | Lambda _ -> assert false (*FIXME*)
              | Brackets _ -> assert false (*FIXME*)
              | Joker _ -> assert false (*FIXME*)
          in
            { pctx=f.pctx; pat=pat; pty=Subst.subst b (pattern_to_term arg.pat) }
        else
          assert false (*FIXME*)
    | _ -> assert false (*FIXME*)

let mk_Pattern_Lambda j =
  match j.pty with
    | Kind -> assert false (*FIXME*)
    | ty -> ( match j.pctx with
               | (l,x,a)::lst ->
                   { pctx=lst; pat=Lambda(l,x,j.pat); pty=Term.mk_Pi l x a ty }
               | _ -> assert false ) (*FIXME*)

let mk_Pattern_Brackets j = { pctx=j.ctx; pat=Brackets j.te; pty=j.ty }

let get_expected_arg_type_p f =
  match Reduction.whnf f.pty with
    | Pi (l,x,a,b) -> { ctx=f.pctx; te=a; ty=Term.mk_Type dloc; }
    | _ -> assert false (*FIXME*)

(* **********************  RULE JUDGMENT *)

let mk_Rule (lhs:pattern_judgment) (rhs:judgment) : rule_judgment =
  assert( lhs.pctx = rhs.ctx ); (*FIXME*)
  if Reduction.are_convertible lhs.pty rhs.ty then
    ( lhs.pctx , lhs.pat , rhs.te )
  else
    assert false (*FIXME*)

(* **********************  REDUCTION *)

let whnf j = { j with te= Reduction.whnf j.te }
let hnf j = { j with te= Reduction.hnf j.te }
let snf j = { j with te= Reduction.snf j.te }
let one j = match Reduction.one_step j.te with
  | Some te2 -> { j with te=te2 }
  | None -> j

let conv j1 j2 = Reduction.are_convertible j1.te j2.te
let check j1 j2 = Reduction.are_convertible j1.ty j2.te
