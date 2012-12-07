open Environ
open Names
open Term
open Typeops
open Dkterm
open Declarations

exception RuleDoesNotExist
exception Typehasnotype
exception NotASort
exception NotACoqVar
exception AnonymousCoqVar
exception NotImplementedYet of string
exception ShouldNotAppear
exception EmptyArrayInApp

(* Some useful stuff not present in the standard library *)

let array_forall p a =
  let r = ref true in
  let i = ref 0 in
  let n = Array.length a in
    while !r && !i < n do
      r := p a.(!i);
      i := !i+1
    done; !r


(* resetting the rel_context of an environment *)
let change_rel_context e rel =
  let pre_env = Environ.pre_env e in
  Environ.env_of_pre_env
    { pre_env with
      Pre_env.env_rel_context = rel }


(**************** Fresh vars **********************)

(* TODO: better fresh vars *)
module VarMap = Map.Make
  (struct
     type t = string
     let compare = compare
   end)


(* Get a new name beginning with a prefix. *)
let fresh_var =
  let fresh_map = ref VarMap.empty in
  fun prefix ->
    let i =
      try VarMap.find prefix !fresh_map
      with Not_found -> 0 in
    fresh_map := VarMap.add prefix (i+1) !fresh_map;
    prefix ^ string_of_int i







(*********** Translation constr (i.e. coq term) to dkterm  *************)

let which_dotpi r = match r with
  | Prop Null,Prop Null  -> "dotpipp"
  | Prop Null,Prop Pos   -> "dotpips"
  | Prop Null,Type _     -> "dotpipt"
  | Prop Pos,Prop Null   -> "dotpisp"
  | Type _,Prop Null     -> "dotpitp"
  | Prop Pos,Type _      -> "dotpist"
  | Type _,Prop Pos      -> "dotpits"
  | Prop Pos,Prop Pos    -> "dotpiss"
  | Type _,Type _        -> "dotpitt"

let which_e s = match s with
  | Prop Pos  -> "eset"
  | Prop Null -> "eprop"
  | Type _    -> "etype"

exception Not_a_sort

(*********** Sort inference **********)

let rec get_sort env args t =
  match Reduction.whd_betadeltaiota env t with
      Sort e -> e
    | Prod(_,ty,te) -> (let env = push_rel (Anonymous, None, ty) env in
			match args with
			    [] -> raise Not_a_sort
			  | a::q -> get_sort env q (subst1 a te))
    | _ -> raise Not_a_sort

let dummy_type = Type (Univ.Atom Univ.Set)

let infer_unsafe e t = Environ.j_type (fst (infer e t))

let infer_sort env t =
  let rec aux env args t =
    match collapse_appl (Reduction.whd_betadeltaiota env t) with
	Rel i -> let _,_,ty = Environ.lookup_rel i env in
	  get_sort env args ty
      | Var v -> let _,_,ty = lookup_named v env in
	  get_sort env args ty
      | Const c -> (match constant_type env c with
			NonPolymorphicType ty -> get_sort env args ty
		      | _ -> dummy_type)
      | Sort _ -> dummy_type
      | Prod(n,ty,te) ->
	  let env = push_rel (n,None,ty) env in
	    aux env args te
      | Cast (_,_,ty) ->  get_sort env args ty
      | LetIn (_,eq,_,te) ->  aux env args (subst1 eq te)
      | App (te, aa) ->
	  aux env
	    (Array.fold_right (fun a l -> a :: l) aa args)
	    te
      | Ind(ind,num) -> (
	  try
	    let p = (lookup_mind ind env).mind_packets.(num) in
	      match p.mind_arity with
                  Monomorphic ar ->  ar.mind_sort
                | Polymorphic par -> dummy_type
	  with Not_found -> failwith ("infer sort: unknown inductive"))
      | Case (ind, ret_ty, matched, branches)  ->
	    (* Get the arguments of the type of the matched term. *)
	  let matched_args =
	    match collapse_appl (Reduction.whd_betadeltaiota env (infer_unsafe env matched))
	    with App(Ind(i),t) when i = ind.ci_ind -> t
	      | Ind(i) when i = ind.ci_ind -> [||]
	      | o -> failwith "term_trans: matched term badly typed"
	  in
	  let indices = Array.sub matched_args ind.ci_npar
	    (Array.length matched_args - ind.ci_npar)
	  in
	  let app_type = match
	    Array.fold_left
	      (function Lambda(_,_,t) -> fun i -> subst1 i t
	       | _ -> fun _ -> raise Not_a_sort) ret_ty indices
	  with
	      Lambda(_,_,t) -> subst1 matched t
	    | _ -> raise Not_a_sort
	  in
	    get_sort env args app_type
      | Fix ((struct_arg_nums, num_def),(names, body_types, body_terms)) ->
	  let sigma =  let rec aux = function
	      0 -> []
	    | n -> Fix((struct_arg_nums, n),(names, body_types, body_terms)) ::
		aux (n-1) in aux (Array.length names)
	  in
	    get_sort env args (substl sigma body_types.(num_def))
      | t -> raise Not_a_sort
  in
    aux env [] t

(* Given environment e, get the scene in which t plays. *)
let get_e e t =  which_e (infer_sort e t)

(* given environment e, gives the \dot\Pi corresponding to \Pi n : t1 t2 *)
let get_dotpi e n t1 t2 =
  let e1 =  push_rel (n,None,t1) e in
    which_dotpi (infer_sort e t1, infer_sort e1 t2)


(* From coq names to string. *)
let path_to_string path =
  let rec aux suffixe = function
      [] -> ""
    | [n] -> string_of_id n ^ suffixe
    | n::q -> aux ("_" ^ string_of_id n ^ suffixe) q
  in aux "" (repr_dirpath path)

let rec module_path_to_string = function
  | MPfile path -> path_to_string path
  | MPdot(mp, lab) ->
      module_path_to_string mp ^ "_" ^ string_of_label lab
  | MPbound mbid -> match repr_mbid mbid with
      _, lab, path ->
	path_to_string path ^ "_" ^ lab


let name_to_string n = "var_" ^ match n with
  | Anonymous -> fresh_var "_dk_anon"
  | Name s -> string_of_id s

let get_identifier n = "var_" ^ match n with
  | Anonymous -> fresh_var "_dk_anon"
  | Name s -> string_of_id s


(* Get an identifier depending on the current environment e. *)
let rec get_identifier_rel erel n =
  match n with
    | Anonymous -> get_identifier_rel erel (Name (id_of_string "_dk_anon"))
    | Name s ->
	let rec compute_alpha ch =
	  let rec alpha_counts n = function
	      (Name i,_,_)::q when string_of_id i = ch -> alpha_counts (n+1) q
	    | (Anonymous, _, _)::q when ch = "_dk_anon" -> alpha_counts (n+1) q
	    | _::q -> alpha_counts n q
	  | [] -> n
	  in
	  let n = alpha_counts 0 erel in
	    if n = 0 then ch
	    else compute_alpha (ch ^ "zqz" ^ string_of_int (n-1))
	in
	  "var_" ^ compute_alpha (string_of_id s)

let nth_rel_in_env n e =
  let rec aux = function
      1, (n,_,_)::l -> get_identifier_rel l n
    | n, x::l -> aux (n-1,l)
    | _ -> failwith "nth_rel_in_env: context not big enough"
  in
    aux (n, Environ.rel_context e)

let get_identifier_env e =
get_identifier_rel (Environ.rel_context e)

(* From coq names to dedukti ids. *)
let name_to_qid n = Id (get_identifier n)


(* Hash table containing already defined fixpoints *)
let fix_tbl = Hashtbl.create 100


(* The translation needs some environment *)
type trans_env = {
  env : Environ.env; (* a coq environment containing already
			declared constructions *)
  decls : Dkterm.statement list; (* an accumulator containing the declarations
				    to be generated (in reverse order) *)
  mp : module_path; (* module path of the current module *)
  functors : module_path list; (* names of the module arguments in functors *)
  functor_parameters : (qid * dkterm) list; (* parameters to be added
					      to each declarations, in
					      reverse order *)
  applied_modules : module_path list (* modules applied to the current module *)
}

(* smart constructors for Dedutki declarations in a functorial context *)
let declaration (id,t) tenv =
  Declaration(id,
	      List.fold_left
		(fun c (i,t) -> DPi(i,t,c))
		t tenv.functor_parameters)

let rule (vars, l ,r) tenv =
  let rec aux = function
    | DApp(c,d) -> DApp(aux c,d)
    | c -> List.fold_right (fun (i,t) c -> DApp(c,DVar i))
      tenv.functor_parameters c
  in
  (List.rev_append tenv.functor_parameters vars, aux l, r)


(* id_with_path m p i return Dedukti variable corresponding to p.i *)
let id_with_path tenv mpath id =
  if List.exists ((=) mpath) tenv.functors then
    DVar (Id ("fp_" ^ id))
  else if tenv.mp = mpath
  then
    DVar (Id id)
  else
    let m = module_path_to_string mpath in
    DVar (Qid (m, id))


(* translate a binding n : t1. t2, given translation functions ft1 and
   ft2 for t1 and t2 *)
let bind_trans ft1 ft2 n t1 t2 tenv =
  let t_tt1, tenv1 = ft1 tenv t1  in
  let te1 = { tenv1 with env = push_rel (n,None,t1) tenv1.env } in
  let t_tt2, tenv2 = ft2 te1 t2  in
  let t_n = get_identifier_rel (Environ.rel_context tenv.env) n in
  t_n, t_tt1, t_tt2, { tenv2 with env = tenv.env }

let push_decl tenv d = { tenv with decls = d::tenv.decls }

let get_inductive_args tenv ind matched =
  let rec aux t =
    match collapse_appl t with
	App(Ind(i),t) when i = ind.ci_ind -> t
      | Ind(i) when i = ind.ci_ind -> [||]
      | t ->
	let t' = Reduction.whd_betadeltaiota tenv.env t in
	if t = t'
	then failwith "term_trans: matched term badly typed"
	else aux t'
  in
  aux (infer_unsafe tenv.env matched)

exception Partial_const

(* Applies every variable in the rel context of the environment tenv.env to
   c.
   Returns the Dedukti environment of variables,
           the translation environment,
           the Dedukti term
*)
let rec app_rel_context tenv c =
  Environ.fold_rel_context
    (fun e (n,_,t) (vs, tenv, c) ->
      (let v = get_identifier_rel (Environ.rel_context e) n in
       let t_tt, tenv = type_trans_aux { tenv with env = e } t in
       (Id v, t_tt)::vs, tenv, DApp(c,DVar (Id v))))
    tenv.env ~init:([], tenv, c)

(* translation of a constructor, with guards
   if the constructor is only partially applied, use eta-expansion
*)
and trans_construct_aux dotp tenv mod_path l ind j i args =
  let ddot = if dotp then fun x -> DDot x else fun x -> x in
  let m_induc =
    try lookup_mind ind tenv.env
    with Not_found -> failwith ("term translation: unknown inductive "^string_of_label l) in
  let induc = m_induc.mind_packets.(j)
  in
  let name = string_of_id induc.mind_consnames.(i-1) in
  let constr = id_with_path tenv mod_path name
  and guard = id_with_path tenv mod_path (string_of_id induc.mind_typename ^ "__constr")
  in
  let constr_type = induc.mind_nf_lc.(i-1) in
  try let applied_constr,tenv =
        Array.fold_left
          (fun (c,tenv') a ->
            let a_tt, tenv = term_trans_aux { tenv' with env = tenv.env } a in
            DApp(c, a_tt), tenv)
          (constr,tenv) args in
      let rec app_args = function
        | Prod(_, _, t2), a::q ->
          app_args (subst1 a t2, q)
        | App(Rel _, params), [] ->
          Array.fold_left
            (fun (c,tenv) a ->
              let a_tt, tenv = term_trans_aux tenv a
              in
              DApp(c, ddot a_tt), tenv)
            (guard,tenv) params
        | Rel _, [] ->
          guard, tenv
        | _ -> raise Partial_const
      in
      let guard_params, tenv =
        app_args (constr_type, Array.to_list args)
      in
      (*	if m_induc.mind_finite
              then*)
      DApp(guard_params, applied_constr), tenv
  (*	else
      if laz
      then DFun(Id (fresh_var "_dk_anon"), DVar(Qid("Coq1univ","lazy")),
      applied_constr), decls
      else applied_constr, decls*)
  with Partial_const ->
    let sigma =
      let rec aux = function
        | 0 -> []
        | n -> Ind(ind, n-1)::aux (n-1)
      in aux m_induc.mind_ntypes
    in
    let constr_type = substl sigma constr_type in
    let rec eta tenv args = function
      | Prod(_, t1, t2) ->
        let v = fresh_var "eta_" in
        let tt_1, tenv' = type_trans_aux tenv t1 in
        let tenv' =
          { tenv' with
            env = push_rel
              (Name (id_of_string v), None, t1) tenv.env } in
        let res, tenv = eta tenv' (("var_" ^ v)::args) t2 in
        DFun(Id ("var_" ^ v), tt_1, res), tenv
      | App(Ind _, params) ->
        let guard, tenv =
          Array.fold_left
            (fun (c,tenv) a ->
              let a_tt, tenv' = term_trans_aux tenv a
              in
              DApp(c, a_tt), tenv')
            (guard,tenv) params in
        let applied_constr =
          List.fold_right (fun v c -> DApp(c, DVar(Id v))) args constr in
        (*	  if m_induc.mind_finite
                then*)
        DApp(guard, applied_constr), tenv
      (*	  else
                if laz then
                DFun(Id (fresh_var "_dk_anon"), DVar(Qid("Coq1univ","lazy")),
                applied_constr), decls
                else applied_constr, decls*)
      | Ind _ ->
        let applied_constr =
          List.fold_right (fun v c -> DApp(c, DVar(Id v))) args constr in
        (*	  if m_induc.mind_finite
                then*)
        DApp(guard, applied_constr), tenv
      (*	  else if laz then
                DFun(Id (fresh_var "_dk_anon"), DVar(Qid("Coq1univ","lazy")),
                applied_constr), decls
                else applied_constr, decls *)
      | _ -> failwith "ill-formed type for a constructor"
    in
    let constr,tenv' = eta tenv [] constr_type in
    Array.fold_left
      (fun (c,d) a ->
        let a_tt, d = term_trans_aux d a in
        DApp(c, a_tt), d)
      (constr,{tenv' with env = tenv.env}) args


(* Translation of t as a term, given the current module path label, an
   environment e and a set of intermediary declarations decls (in
   reverse order). *)
and term_trans_aux tenv t =


  (* translation of an inductive type *)
  let trans_ind ind num args =
    let mod_path, _, l = repr_mind ind in
    try
      let inductive = lookup_mind ind tenv.env in
      let name = string_of_id inductive.mind_packets.(num).mind_typename in
      let base,tenv = Array.fold_left
	(fun (u1,tenv) u2 ->
	  let u_tt2, tenv = term_trans_aux tenv u2 in
	  DApp(u1, u_tt2), tenv)
	(id_with_path tenv mod_path name, tenv) args
      in
      (*      if inductive.mind_finite || not laz
	      then*)
      base, tenv
    (*   else
	 let rec eta base args decls = function
	 0, _ ->
	 DApp(DVar(Qid("Coq1univ", "lazify")),
	 List.fold_left
	 (fun c a -> DApp(c,a))
	 base args),
	 decls
	 | n, (name,_,ty)::q ->
	 let v = fresh_var "i_" in
	 let ty_tt, decls' = type_trans_aux laz
	 label
	 { e with env_rel_context = q }
	 ty decls in
	 let c, decls = eta base (DVar(Id v)::args) decls' (n-1,q) in
	 DFun(Id v, ty_tt, c), decls
	 | _ -> failwith "trans_ind: wrong context"
	 in
	 eta base [] decls
	 (List.length inductive.mind_packets.(num).mind_arity_ctxt
	 - Array.length args,
	 inductive.mind_packets.(num).mind_arity_ctxt)
	 raise (NotImplementedYet "coinductives with lazy")*)
    with Not_found -> failwith ("term translation: unknown inductive "
				^ string_of_label l)
  in

  (* This is the main translation *)
  (* The translation is by induction on the term. *)
  match collapse_appl t with
      (* first cases are a special case for the constructors *)
      App(Construct((ind,j), i), args) ->
	let (mod_path,_,l) = repr_mind ind in
        let r, te = trans_construct_aux false tenv mod_path l ind j i args in
	r, { te with env = tenv.env }

    | Construct((ind,j), i) ->
      let (mod_path,_,l) = repr_mind ind in
      let r, te = trans_construct_aux false tenv mod_path l ind j i [||] in
      r, { te with env = tenv.env }

    (* special cases for the inductives (no real need for it) *)
    (*| App(Ind(ind, num), args) ->
      let r, te = trans_ind ind num args in
      r, { te with env = tenv.env }
    *)

    | Ind(ind, num)  ->
      let r, te = trans_ind ind num [||] in
      r, { te with env = tenv.env }

    | Rel n -> DVar (Id (nth_rel_in_env n tenv.env)), tenv

    | Var v  -> DVar(Id (string_of_id v)), tenv

    | Meta _ -> raise ShouldNotAppear

    | Evar _ -> raise ShouldNotAppear

    | Sort s -> (match s with
	| Prop Null -> DVar (Qid ("Coq1univ","dotprop"))
	| Prop Pos ->  DVar (Qid ("Coq1univ","dotset"))
	| Type _ ->    DVar (Qid ("Coq1univ","dottype")))  (*** !!! Attention a Type 0 ***)
      , tenv

    | Cast (t,_,_) -> (* Are casts really needed? *)
      term_trans_aux tenv t


    | Prod (n,t1,t2)  ->
      let t_n, t_tt1, t_tt2, tenv' = bind_trans term_trans_aux term_trans_aux
	n t1 t2 tenv in
      DApp (DApp (DVar(Qid("Coq1univ",get_dotpi tenv.env n t1 t2)), t_tt1),
	    DFun (Id t_n,
		  DApp(DVar(Qid ("Coq1univ",get_e tenv.env t1)), t_tt1),
		  t_tt2)), tenv'

    | Lambda (n,t1,t2)  ->
      let t_n, t_tt1, t_tt2, tenv' = bind_trans type_trans_aux term_trans_aux
	n t1 t2 tenv in
      DFun (Id t_n,
	    t_tt1,
	    t_tt2), tenv'

    | LetIn (var, eq, ty, body)  ->
      (* let x : T = A in B in [H1 : T1, ..., Hn : Tn] is translated as
	 fresh_x : H1 : ||T1|| -> ... Hn : ||Tn|| -> ||T||.
	 [H1 : ||T1||, ..., Hn : ||Tn||] fresh_x H1 Hn --> A.
	 |B| where x is replaced by fresh_x H1 Hn
      *)
      let fresh_x = fresh_var ("let_" ^ name_to_string var ^ "_") in
      let dk_env, tenv', lhs =
	app_rel_context tenv (DVar (Id fresh_x)) in
      let t_ty, tenv' = type_trans_aux { tenv' with env = tenv.env } ty in

      let n = List.length (rel_context tenv.env) in
      let arg =
	App(Var fresh_x, Array.init n (fun i -> Rel (n - i)))
      in
      let new_body = subst1 arg body in
      let teq, tenv' = term_trans_aux { tenv' with env = tenv.env } eq in
      let new_env = push_decl
	(push_decl { tenv' with
          env = push_named (fresh_x, Some(it_mkLambda_or_LetIn eq (rel_context tenv.env)), it_mkProd_or_LetIn ty (rel_context tenv.env)) tenv.env}
	   (Declaration(Id fresh_x, List.fold_left
	     (fun c (x, t) -> DPi(x, t, c))
	     t_ty dk_env)))
	(RuleSet [List.rev dk_env, lhs, teq])
      in
      term_trans_aux new_env new_body

    | App (t1,a)  ->
      let tt, tenv' =  Array.fold_left
	(fun (u1,te1) u2 ->
	  let u_tt2, te2 = term_trans_aux {te1 with env = tenv.env} u2 in
	  DApp(u1, u_tt2), te2)
	(term_trans_aux tenv t1) a
      in tt, { tenv' with env = tenv.env }

    | Const co -> let (mod_path,dp,name) = repr_con co in
		  id_with_path tenv mod_path (string_of_label name), tenv

    | Case (ind, ret_ty, matched, branches)  ->
      let mind_body = lookup_mind (fst ind.ci_ind) tenv.env in
      let case_name =
	string_of_id mind_body.mind_packets.(snd ind.ci_ind).mind_typename ^ "__case"
      in
      (* Get the arguments of the type of the matched term. *)
      let matched_args = get_inductive_args tenv ind matched in
      let mp, _, _ = repr_mind (fst ind.ci_ind) in
      let r = ref (id_with_path tenv mp case_name)
      and d = ref tenv in
      for i = 0 to Array.length matched_args - 1 do
	let arg_tt, tenv' =
	  term_trans_aux !d matched_args.(i)  in
	r := DApp(!r, arg_tt);
	d := tenv'
      done;
      let ret_ty_tt, tenv' =
	(*	      (* We cannot translate the return type directly in the case of
		      a coinductive since the matched argument must be forced and
		      is therefore not lazy *)
		      let rec trans_ret_ty e decls = function
		      0, Lambda(n, ty, te) ->
		      let ty_tt, dd = type_trans_aux laz label e ty decls in
		      let ty_tt = match ty_tt with
		      DApp(e,DApp(DVar(Qid("Coq1univ","lazify")),t)) ->
		      DApp(e,t)
		      | _ -> failwith "term_trans: incorrect return type in case"
		      in
		      let e1 = push_rel (n, None, ty) e in
		      let v = get_identifier_env e n in
		      let te_tt, decls2 = term_trans_aux laz label e1 te dd in
		      let te_tt = subst v
		      (DFun(Id (fresh_var "_dk_anon"),
		      DVar(Qid("Coq1univ", "lazy")),
		      DVar(Id v)))
		      te_tt
		      in
		      DFun (Id v, ty_tt, te_tt), decls2

		      | m, Lambda(n, ty, te) ->
		      let ty_tt, dd = type_trans_aux laz label e ty decls in
		      let e1 = push_rel (n,None,ty) e in
		      let te_tt, decls2 = trans_ret_ty e1 dd (m-1, te) in
		      DFun ((Id (get_identifier_env e n)),
		      ty_tt,
		      te_tt), decls2
		      | _ -> failwith "term_trans: incorrect return type in case"
		      in
		      if mind_body.mind_finite then *)
	term_trans_aux !d ret_ty
      (*		else
			trans_ret_ty e !d
			(Array.length matched_args - ind.ci_npar, ret_ty) *)
      in
      r := DApp(!r, ret_ty_tt);
      d := tenv';
      Array.iter
	(fun b ->
	  let b_tt, tenv' = term_trans_aux !d b  in
	  r := DApp(!r, b_tt);
	  d := tenv')
	branches;
      let m_tt, tenv' = term_trans_aux !d matched in
      let m_tt =
				(*	if mind_body.mind_finite
					then *) m_tt
      (*else DApp(m_tt, DVar(Qid("Coq1univ", "force")))*)
      in
      DApp(!r, m_tt), { tenv' with env = tenv.env }

    | Fix(((struct_arg_nums, num_def),(names, body_types, body_terms))as fix)
      -> begin
	let nb_def = Array.length names in
	let env_length = - (List.length (rel_context tenv.env)) in
	let rel_types = Array.map (liftn env_length (1+nb_def)) body_types and
	    rel_terms = Array.map (liftn env_length (1+nb_def)) body_terms in
	try Hashtbl.find fix_tbl (rel_types,rel_terms,num_def), tenv with
	    Not_found ->
	      (* If the bodies of the fixpoints are closed, we do not need
		 to add the context, and we can use the hashtable. *)
	      let closed =
		array_forall (closedn (Array.length body_terms))
		  body_terms
	      in
	      let fixpoint_context, env_context =
		if closed
		then [], Environ.rel_context tenv.env
		else Environ.rel_context tenv.env, []
	      in
	      (* Get fresh names for the fixpoints. *)
	      let names = Array.map
		(function
		Name n -> fresh_var (string_of_id n ^ "_")
		  | Anonymous -> fresh_var "fix_"
		)
		names in
	      (* first push declarations of the fixpoint functions *)
	      let tenv',_ =
		Array.fold_left (fun (tenv,i) body_type ->
		  let t, tenv' = type_trans_aux
		    { tenv with env =
			change_rel_context tenv.env env_context }
		    (it_mkProd_or_LetIn
		       body_type
		       fixpoint_context
		    ) in
		  push_decl tenv' (declaration(Id names.(i), t) tenv'), i+1
		) (tenv, 0) body_types in
	      let tenv = {tenv' with env = tenv.env }
	      in
	      (* Translation of one inductive fixpoint. *)
	      let one_trans struct_arg_num name body_type body_term tenv =
		(* Recursively applies all the variables in the context at the
		   point of the fixpoint definition down to the recursive
		   variable, and creates a rule outside of the current context. *)
		let env_vars, tenv', fix =
		  if closed
		  then [], {tenv with env = change_rel_context tenv.env env_context}, DVar(Id name)
		  else app_rel_context tenv
		    (DVar(Id name)) in
		let rec make_rule tenv vars fix rhs = function
		  | 0, Prod(n, a, _) ->
		    let s = get_identifier_rel (Environ.rel_context tenv.env) n in
		    (* Adds s:Typeofs to the list of things to
		       apply to the fixpoint. *)
		    let a_tt, tenv' = type_trans_aux tenv a in
		    let vars = List.rev_append vars
		      [Id s, a_tt]
		    in
		    let ind, args =
		      (* we have to compute a because the
			 inductive type can be hidden behind a
			 definition. *)
		      match collapse_appl (Reduction.whd_betadeltaiota tenv.env a)
		      with
			  App(Ind(i), l) -> i, l
			| Ind(i) -> i, [||]
			| _ -> failwith "term translation: structural argument is not an inductive type"
		    in
		    let i__constr =
		      try
			let name = (lookup_mind (fst ind) tenv.env).mind_packets.(snd ind).mind_typename
			in
			let mp,_,_ = repr_mind (fst ind) in
			id_with_path tenv mp
			  (string_of_id name ^ "__constr")
		      with Not_found -> failwith ("term translation: unknown inductive in structural argument")
		    in
		    let guard f = Array.fold_left
		      (fun c a ->
			let a_tt, _ = term_trans_aux tenv a in
			DApp(c, f a_tt))
		      i__constr args
		    in
		    let last_arg f = DApp(guard f, DVar (Id s)) in
		    (* This is the final case, apply the recursive variable to
		       f and create a rule f x1...xn --> rhs x1...xn. *)
		    { tenv with decls =
			RuleSet [rule(List.rev_append env_vars vars,
			     DApp(fix, last_arg (fun x -> DDot x)),
			     DApp(rhs, last_arg (fun x ->      x))
			) tenv']
			  ::tenv'.decls }
		  | n, Prod(nom, a, t) ->
		    let s = get_identifier_rel
		      (Environ.rel_context tenv.env) nom
		    in
		    let a_tt, tenv' = type_trans_aux tenv a in
		    let tenv' = { tenv' with
		      env = push_rel (nom, None, a) tenv'.env } in
		    (* This is not the final case, apply the current variable
		       to f x1...xi and to rhs and call yourself
		       recursively. *)
		    make_rule tenv' ((Id s, a_tt)::vars)
		      (DApp(fix, DVar (Id s)))
		      (DApp(rhs, DVar (Id s)))
		      (n-1, t)
		  | _ -> failwith "fixpoint translation: ill-formed type" in
		(* Here we need to give the right parameters to
		   make_rule. *)
		let n = List.length fixpoint_context in
		(* The variable arguments to pass to fix have de bruijn
		   indices from 1 to n. *)
		let rel_args = Array.init n (fun i -> Rel (n - i)) in
		(*rhs_env  adds the names of the mutually defined recursive
		  functions in the context for the rhs*)
		let _,rhs_env = Array.fold_left
		  (fun (i,e) n ->
		    i+1,
		    push_named
		      (id_of_string n, None, it_mkProd_or_LetIn body_types.(i) fixpoint_context)
		      e)
		  (0,tenv.env) names in
		(* We use the just defined context to replace the indexes in the
		   rhs that refer to recursive calls (the rhs is typed in the
		   context with the recursive functions and their types). *)
		let sigma = Array.fold_left
		  (fun l n -> App(Var (id_of_string n), rel_args)::l) [] names
		in
		let rhs, tenv2 =
		  term_trans_aux {tenv' with env = rhs_env }
		    (substl sigma body_term)
		in
		make_rule { tenv2 with env = tenv.env}
		  [] fix rhs (struct_arg_num, body_type)
	      in
	      (* And we iterate this process over the body of every. *)
	      let _, tenv' = Array.fold_left
		(fun (i,te) struct_arg_num -> i+1,
		  one_trans struct_arg_num names.(i)
		    body_types.(i) body_terms.(i) { te with env = tenv.env })
		(0,tenv) struct_arg_nums in
	      (* The term corresponding to the fix point is the identifier
		 to which the context is applied. *)
	      let _, tenv = Array.fold_left (fun (i,tenv') name ->
		let _, tenv, t =
		  if closed
		  then [], {tenv' with env = tenv.env}, DVar(Id names.(num_def))
		  else app_rel_context {tenv' with env = tenv.env}
		    (DVar(Id name))
		in
		Hashtbl.add fix_tbl (rel_types,rel_terms,i) t;
		i+1, tenv
	      ) (0,tenv') names in
	      Hashtbl.find fix_tbl (rel_types,rel_terms,num_def), tenv
      end

    | CoFix (num_def, (names, body_types, body_terms)) ->
      (*      let closed =
	      array_forall (closedn (Array.length body_terms))
	      body_terms
	      in
	      let fixpoint_context, env_context =
	      if closed
	      then [], Environ.rel_context e
	      else Environ.rel_context e, []
	      in
	      let names = Array.map
	      (function
	      Name n -> fresh_var (string_of_id n ^ "_")
	      | Anonymous -> fresh_var "fix_"
	      )
	      names in
      (* Translation of one inductive fixpoint. *)
	      let one_trans name body_type body_term decls =
      (* Declare the type of the fixpoint function. *)
	      let decls' =
	      let t, decls' = type_trans_aux laz label
	      (change_rel_context e env_context)
	      (it_mkProd_or_LetIn
	      body_type
	      fixpoint_context
	      ) decls in
	      Declaration(Id name, t)::decls' in
	      let env_vars, fix, decls' =
	      if closed
	      then [], DVar(Id name), decls'
	      else app_rel_context e (DVar(Id name)) decls' in
	      let rec make_rule e vars fix rhs decls = function
	      Prod(nom, a, t) ->
	      let s = get_identifier_rel (Environ.rel_context e) nom in
	      let e' = push_rel (nom, None, a) e in
	      let a_tt, decls' = type_trans_aux laz label e a decls in
      (* This is the not final case, apply the current variable
	      to f x1...xi and to rhs and call yourself
	      recursively. *)
	      make_rule e' ((Id s, a_tt)::vars)
	      (DApp(fix, DVar (Id s)))
	      (DApp(rhs, DVar (Id s)))
	      decls'
	      t
	      | App(Ind _,_) | Ind _ ->
	      Rule(List.rev env_vars,
	      DApp(fix, DVar(Qid("Coq1univ","force"))),
	      DApp(rhs, DVar(Qid("Coq1univ","force")))
	      )
	      ::decls'

	      | _ -> failwith "cofixpoint translation: ill-formed type"
	      in
	      let n = List.length fixpoint_context in
      (* The variable arguments to pass to fix have de bruijn
	      indices from 1 to n. *)
	      let rel_args = Array.init n (fun i -> Rel (n - i)) in
      (*rhs_env  adds the names of the mutually defined recursive
	      functions in the context for the rhs*)
	      let _,rhs_env = Array.fold_left
	      (fun (i,e) n ->
	      i+1,
	      push_named
	      (id_of_string n, None, it_mkProd_or_LetIn body_types.(i) fixpoint_context)
	      e)
	      (0,e) names in
      (* We use the just defined context to replace the indexes in the
	      rhs that refer to recursive calls (the rhs is typed in the
	      context with the recursive functions and their types). *)
	      let sigma = Array.fold_left
	      (fun l n -> App(Var (id_of_string n), rel_args)::l) [] names
	      in
	      let rhs, decls2 =
	      term_trans_aux laz label rhs_env (substl sigma body_term) decls'
	      in
	      make_rule e [] fix rhs decls2 body_type
	      in
      (* And we iterate this process over the body of every. *)
	      let _, decls' = Array.fold_left
	      (fun (i,decls) name -> i+1,
	      one_trans name body_types.(i) body_terms.(i) decls)
	      (0,decls) names in
      (* The term corresponding to the fix point is the identifier
	      to which the context is applied. *)
	      let _, t, decls =
	      if closed
	      then [], DVar(Id names.(num_def)), decls'
	      else app_rel_context e (DVar(Id names.(num_def))) decls'
	      in
	      t, decls
      *)
      raise (NotImplementedYet "term_trans : cofixpoint")

(*** Translation of t as a type, given an environment e. ***)

and type_trans_aux tenv t = match t with
  | Sort s -> (match s with
      | Prop Pos  -> DVar(Qid("Coq1univ","Uset"))
      | Prop Null -> DVar(Qid("Coq1univ","Uprop"))
      | Type _    -> DVar(Qid("Coq1univ","Utype"))), tenv

  | Prod(n,t1,t2) ->
    let t_n, t_tt1, t_tt2, tenv' = bind_trans type_trans_aux type_trans_aux
      n t1 t2 tenv in
    DPi(Id t_n,t_tt1,t_tt2),
    tenv'
  | t -> let t', tenv' = term_trans_aux tenv t  in
	 DApp(DVar(Qid("Coq1univ",get_e tenv.env t)), t'),
	 { tenv' with env = tenv.env }


(* Translation functions without environment. *)
(* let term_trans label t = term_trans_aux true label (get_base_env ()) t []

let type_trans label t = type_trans_aux true label (get_base_env ()) t []
*)

(*** Translation of a declaration in a structure body. ***)


(* tenv : translation environment
   params_num : number of parameters of the constructor
   params_dec : offset of the parameters in the environment
   cons_name : Coq term of the constructor
   typ : type of the constructor

   returns the constructor with variables for the parameters and the other args
           the indices of the type of constructor
               (arguments that are not parameters)
           the new translation environment
           the auxiliary declarations
*)
let make_constr tenv params_num params_dec cons_name typ =
  let cons_name_with_params = App(cons_name,
				  Array.init params_num
				    (fun i -> Rel(params_dec + params_num - i))) in
  let rec aux tenv vars c = function
      0, Prod(n, t1, t2) ->
	let t_tt1, tenv' = type_trans_aux tenv t1 in
	let v = fresh_var "c_arg_" in
	let te' = { tenv' with
	  env =  push_rel (Name (id_of_string v), None, t1) tenv.env }  in
	  aux te' ((Id("var_"^v), t_tt1)::vars) (App(c,[|Var("var_" ^ v)|])) (0, t2)
    | 0, App(_,args) ->
	let ind_num = Array.length args - params_num in
	let ind = Array.make ind_num DKind in
	let d = ref tenv in
          for i = 0 to Array.length ind - 1 do
	    let a_i, d' = term_trans_aux !d args.(i+params_num) in
              ind.(i) <- a_i;
	      d := d'
          done;
	  !d, c, ind, vars
    | 0, _ -> tenv, c, [||], vars
    | n, Prod(_, _, t2) -> aux tenv vars c
	(n-1, subst1 (Rel (params_dec + n)) t2)
    | _ -> failwith "inductive translation: ill-typed constructor"
  in
  let tenv', c, ind, vars = aux tenv [] cons_name_with_params (params_num, typ) in
  let trans_constr ind j i args =
      let (mod_path,_,l) = repr_mind ind in
      let te = { tenv' with env = tenv.env } in
      let r, te' = trans_construct_aux true te mod_path l ind j i args in
      r, { te' with env = te.env } in
  let res, tenv =
    match collapse_appl c with
    | App(Construct((ind, j), i), args) -> trans_constr ind j i args
    | Construct((ind, j), i) -> trans_constr ind j i [| |]
    | _ -> failwith "Seems that I forgot one case." in
  res, ind, vars, tenv

(* Auxiliary function for make_constr_func_type *)
let rec make_constr_func_type' cons_name num_treated num_param num_indices num_args = function
    Prod(n, t1, t2) ->
      Prod(n, t1,
	   make_constr_func_type' cons_name num_treated num_param num_indices
	     (num_args+1) t2)
  | App(_,args) ->
      App(App(Rel (num_args + 1 + num_treated), (* P *)
	      Array.init (Array.length args - num_param)
			(fun i -> args.(i+ num_param))),
		  [| App(Construct cons_name,
			 Array.init (num_args+num_param)
			   (fun i -> if i < num_param
			    then Rel(num_args + 1 + num_treated + num_param + num_indices - i)
			    else Rel(num_args + num_param - i))) |] )
  |  _ ->
	      App(Rel (num_args + 1 + num_treated),
		  [| App(Construct cons_name,
			 Array.init (num_args+num_param)
			   (fun i -> if i < num_param
			    then Rel(num_args + 1 + num_treated + num_param + num_indices - i)
			    else Rel(num_args + num_param - i))) |] )


(* Makes the type of the function in the __case of an inductive type
   corresponding to a constructor
   make_constr_func_type cons_name num_treated type :
     cons_name : name of the constructor
     num_treated : number of the constructor in the inductive definition
     num_param : number of parameters
     num_indices : number of indices
     typ : type of the constructor
 *)
let make_constr_func_type cons_name num_treated num_param num_indices typ =
  let rec aux = function
      0, t -> make_constr_func_type' cons_name num_treated num_param
        num_indices 0 (lift num_treated t)
    | n, Prod(_, t1, t2) ->
	aux (n-1, subst1 (Rel(n + num_indices + 1)) t2)
    | _ -> failwith "inductive translation: ill-formed constructor type"
  in aux (num_param, typ)



(* translate a packet of a mutual inductive definition (i.e. a single inductive)
   tenv : environment
   ind : path of the current inductive
   params : parameter context of the mutual inductive definition
   constr_types : type of the constructors in p
   p : packet
*)
let packet_translation finite tenv ind params constr_types p =
  let rec trans_constr tenv = function
      Prod(n,ty,te) ->
	let t_n, ty_tt, te_tt, tenv' = bind_trans type_trans_aux trans_constr
	  n ty te tenv in
	DPi(Id t_n, ty_tt, te_tt), tenv'
    | te -> type_trans_aux tenv te
  in
  let n_params = List.length params in
  let n_indices = List.length p.mind_arity_ctxt - n_params in
    (* Add the constructors to the environment  *)
  let indices = (* arguments that are not parameters *)
    let rec aux accu = function
	0, _ -> List.rev accu
      | n, x :: q -> aux (x::accu) (n-1, q)
      | _ -> failwith "inductive translation: ill-formed arity"
    in aux [] (n_indices, p.mind_arity_ctxt)
  in
  let constr_decl name c tenv =
    let c_tt, tenv' =
      trans_constr tenv c in
      push_decl tenv' (declaration (Id name, c_tt) tenv') in
  let nb_constrs =  Array.length p.mind_consnames in
  let case_name = DVar (Id (string_of_id p.mind_typename ^ "__case")) in
  let this_env, param_vars, case_name =
    List.fold_right
      (fun (_,_,t) (te, vars, c) ->
	 let v = fresh_var "param_" in
	 let t_tt, _ = type_trans_aux te t in
	 let te = {te with env = push_rel (Name(id_of_string v), None, t) te.env} in
         let param_id = Id("var_"^v) in
	   te, (param_id, t_tt)::vars, DApp(c, DDot(DVar(param_id)))
      )
      params ({ tenv with decls = [] }, [], case_name)
  in
  let this_env, indices_vars =
    List.fold_right
      (fun (_,_,t) (te, vars) ->
	 let v = fresh_var "in_" in
	 let t_tt, _ = type_trans_aux te t in
	 let te = {te with env = push_rel (Name(id_of_string v), None, t) te.env} in
         let indice_id = Id("var_"^v) in
	   te, (indice_id, t_tt)::vars)
       indices (this_env, [])
  in
  let this_env, p_var, case_rem = (* Build the return type. *)
    let v = fresh_var "P_" in
    let t = it_mkProd_or_LetIn
      (Prod(Name (id_of_string "i"),
            App(Ind(ind),
		let n = List.length p.mind_arity_ctxt in
                let idx i =
                  if i < n_params
                  then Rel(n + n_indices - i)
                  else Rel(n - i)
                in Array.init n idx),
	    Term.Sort (Term.Type (Univ.Atom Univ.Set))))
      (snd (List.fold_right
        (fun (n, c, t) (i, l) -> succ i, (n, c, liftn n_indices i t) :: l)
        indices (1, []))) in
    let t_tt, te = type_trans_aux this_env t in
    let te = {te with env = push_rel (Name (id_of_string v), None, t) te.env} in
      te, (Id("var_"^v), t_tt),
    [ DVar(Id("var_"^v)) ]
  in
  let _,(this_env as tenv1),func_vars, case_rem = Array.fold_left
    (fun (i,te,vars,cr) cons_name  ->
       let t = make_constr_func_type
	 (ind, i+1) i n_params n_indices
	 constr_types.(i) in
       let v = fresh_var "f_" in
       let t_tt, te' = type_trans_aux te t in
       let te' = {te' with env= push_rel (Name (id_of_string v), None, t) te'.env} in
	 i+1,te',(Id("var_"^v), t_tt)::vars, DVar(Id("var_"^v)) :: cr)
    (0,this_env,[],case_rem) p.mind_consnames
  in
  let _, this_env = (* Compile constructors' definitions. *)
    Array.fold_left
      (fun (i, te) cons_name ->
	 i+1, constr_decl (string_of_id cons_name) constr_types.(i) te)
      (0,this_env) p.mind_consnames
  in
  let (m,_) as m_var, this_env =
    let v = fresh_var "m_" in
    let t = App(Ind(ind),
		let n = List.length p.mind_arity_ctxt in
                let idx i = Rel(n + nb_constrs + 1 - i) in
                Array.init n idx)
    in
    let t_tt, te' = type_trans_aux this_env t in
    let te = {te' with env = push_rel (Name (id_of_string v), None, t) te'.env} in
      (Id("var_"^v), t_tt), { te' with env = this_env.env }
  in
  let end_type, this_env =
    term_trans_aux this_env (
	App(Rel(nb_constrs + 1),
	    Array.init n_indices (fun i -> Rel(n_indices+1+nb_constrs-i)))
    ) in
  let end_type =
    DApp(DVar(Qid("Coq1univ","etype")),
	 DApp(end_type,
	      if finite
	      then DVar m
	      else
		DFun(Id (fresh_var "_dk_anon"),
		     DVar(Qid("Coq1univ","lazy")),
		     DVar m)))
  in
  let rec add_binding_from_vars c = function
      [] -> c
    | (v,t)::q -> add_binding_from_vars (DPi(v,t,c)) q
  in
  let i__case_dk_type = add_binding_from_vars
    (let v,t = m_var in DPi(v,t,end_type)) func_vars in
  let i__case_dk_type = add_binding_from_vars
    (let v,t = p_var in DPi(v,t,i__case_dk_type)) indices_vars in
  let i__case_trans = add_binding_from_vars i__case_dk_type param_vars in
  let params_dec = n_indices + nb_constrs + 1 in
    (* declaration of the __case type *)
  let this_env = push_decl this_env
    (declaration(
      Id (string_of_id p.mind_typename ^ "__case"),
      i__case_trans) this_env)
  in
  let case_rem = List.rev case_rem in
  let te = { this_env with env = tenv1.env } in
  let _,rs =
    Array.fold_left
      (fun (i, rs) cons_name ->
	 let constr, indices, c_vars, te' =
	   make_constr te n_params params_dec
	     (Construct (ind,i+1)) constr_types.(i) in
	 i+1,
	 (rule((* ENV *) List.rev_append param_vars
	    (p_var::List.rev_append func_vars (List.rev c_vars)),
            (* LHS *)
            begin
              let app c a = DApp(c, a) in
              let dapp c a = app c (DDot a) in
              let case_name = Array.fold_left dapp case_name indices in
              let case_name = List.fold_left app case_name case_rem in
              DApp(case_name, constr)
            end,
            (* RHS *)
            match List.nth func_vars (List.length func_vars-i-1)
            with id,_ ->
              List.fold_right (fun (v,_) c -> DApp(c, DVar v))
                c_vars (DVar id)) te') :: rs)
	   (0,[]) p.mind_consnames in
    { tenv with decls =  RuleSet rs :: te.decls @ tenv.decls}



(* Printing declarations *)
let rec print_decls module_name imports stmts =
  let output_file = open_out (module_name ^ ".dk")
  in
  List.iter (fun i ->
  Printf.fprintf output_file "#%s\n" (path_to_string i)) imports;
  output_module output_file
    (match stmts with
      [] -> (* Dedukti does not like empty files, adding a
	       dummy declaration. *)
	[Declaration(Id("dummy"),DType)];
    | _ -> stmts);
  prerr_endline ("closing module " ^ (module_name));
  close_out output_file


let add_constants env mtb =
  match mtb.typ_expr with
  | SEBstruct l  ->
    List.fold_left
      (fun e (c,SFBconst cb) ->
	let c_mp = mtb.typ_mp,[], c in
	add_constant (c_mp,c_mp) cb e) env l
  | _ -> raise (NotImplementedYet "non struct argument of a functor")



let const_type_to_term = function
  | NonPolymorphicType t -> t
  | PolymorphicArity(context, arity) ->
    it_mkProd_or_LetIn (Sort (Type arity.poly_level))
      context

let add_param tenv mtb =
  match mtb.typ_expr with
      SEBstruct l ->
	snd(List.fold_left (fun (te,l) (c,SFBconst cb) ->
	  let fp_c = "fp_" ^ c in
	  let t_cb,te = type_trans_aux te (const_type_to_term cb.const_type) in
	  let te = { te with env =
	      Environ.add_constant
		(make_con tenv.mp empty_dirpath c)
		cb te.env} in
	  te, (Id fp_c,t_cb)::l) ({tenv with functors = tenv.mp::tenv.functors}
				     ,[])
	      l)


let rec struct_elem_copy tenv mp_src sup_args (label, decl) = match decl with
	SFBconst sbfc ->
	  let tenv = {tenv with env =
	      Environ.add_constraints sbfc.const_constraints tenv.env } in
	  let ttype, type_tenv =
	    let t = const_type_to_term sbfc.const_type in
	    type_trans_aux tenv t
	  in
	  let id = Id (string_of_label label) in
	  let tterm = id_with_path tenv
	    mp_src label in
	  let tterm = List.fold_right
	    (fun a c -> DApp(c,a)) sup_args tterm
	  in
          push_decl (push_decl type_tenv (declaration(id, ttype) type_tenv))
		 (RuleSet [rule([],DVar id, tterm) type_tenv])

       | SFBmind m ->
	 let mind = make_mind tenv.mp empty_dirpath label in
	 let _,mind_names =
	   Array.fold_left
	     (fun (i, l) _ -> i+1, Ind(mind, i)::l)
	     (0, []) m.mind_packets
	 in
	 let te,_ =
	   Array.fold_right
	    (fun p (te,i) ->
	      let constr_types = Array.map (substl mind_names) p.mind_nf_lc in
	      let _,env = Array.fold_left
		(fun (j, env) consname ->
		  j + 1,
		  Environ.push_named (consname, None, constr_types.(j)) env)
		(0, tenv.env) p.mind_consnames in
	      let te = {te with env = env} in
	      let _,te = Array.fold_left
		(fun (j, te) consname ->
		  j + 1,
		 let t,te = type_trans_aux te constr_types.(j)  in
		 push_decl (push_decl te (RuleSet [rule([], DVar (Id consname),
					       id_with_path te mp_src consname) te]))
		   (declaration(Id consname, t) te))
		(0, te) p.mind_consnames in
	      te
		, i-1)
	     m.mind_packets (tenv,
			     Array.length m.mind_packets - 1)
	 in
	 fst
	   (Array.fold_right
	      (fun p (te,i) ->
		let t, te' = type_trans_aux
		  te
		  (it_mkProd_or_LetIn
		     (Sort (match p.mind_arity with
			 Monomorphic ar ->  ar.mind_sort
		       | Polymorphic par ->
			 Type par.poly_level))
		     p.mind_arity_ctxt
		  )
		in
		let end_type__constr =
		  let n = List.length p.mind_arity_ctxt in
		  App(Ind(mind,i-1),
		      Array.init n (fun i -> Rel(n-i)))
		in
		let t__constr, te' = type_trans_aux te'
		  (it_mkProd_or_LetIn
		     (Prod(Anonymous,
			   end_type__constr,
			   lift 1 end_type__constr))
		     p.mind_arity_ctxt
		  )
		in
		let type_name = string_of_id p.mind_typename in
		{te' with decls = declaration(Id type_name,
					      t) te'
		     ::RuleSet [rule([], DVar (Id type_name),
			    id_with_path te' mp_src type_name) te']
			 ::
			   (if m.mind_finite then
			       declaration(Id (type_name ^ "__constr"),
					   t__constr) te'
			       :: RuleSet [rule([], DVar (Id (type_name ^ "__constr")),
				       id_with_path te' mp_src (type_name ^ "__constr")) te']
				   :: te'.decls else te'.decls)}, i+1)
	      m.mind_packets (te,1))
       | SFBmodule mb ->
	 let kn = MPdot(tenv.mp, label) in
	 let modtype = Modops.module_type_of_module tenv.env (Some kn) mb in
	 let tenv' = { tenv with env = Modops.add_module mb
	     (add_modtype kn modtype tenv.env)} in
	 let tenv'' = mb_trans {tenv' with mp = MPdot(tenv.mp,label);
	   applied_modules = []
			       } mb in
	 let mod_name = module_path_to_string tenv.mp
	   ^ "_" ^ string_of_label label
	 in
	 print_decls mod_name [] (List.rev tenv''.decls);
	 tenv'

       | SFBmodtype mty ->
	 (* we do not translate module types, but we put them in the
	    environment *)
	 let kn = MPdot(tenv.mp, label) in
	 { tenv with env = add_modtype kn mty tenv.env }



(* bridge between two modules (translation of ident) *)
and module_copy tenv mp_src mod_type =
  let add_args dl m sup_args =
    List.fold_left
      (fun args (l,_) -> id_with_path tenv m l::args) sup_args dl
  in
  let rec aux sup_args = function
    | [], SEBstruct l ->
      List.fold_right (fun e te -> struct_elem_copy te mp_src sup_args e) l tenv
    | m::q, SEBfunctor(id, mtb, body) ->
      let subst = Mod_subst.map_mp (MPbound id) m Mod_subst.empty_delta_resolver in
      let body = Modops.subst_struct_expr subst body in
      let SEBstruct dl = mtb.typ_expr in
      aux (add_args dl m sup_args) (q, body)
    | _ -> raise (NotImplementedYet "module_copy: not a struct")
  in
  aux [] (tenv.applied_modules, mod_type)


(* Translation of a declaration in a structure. *)
and sb_decl_trans tenv (label, decl) =
  prerr_endline ("declaring "^ string_of_label label);
  match decl with
      (* Declaration of a constant (theorem, definition, etc.). *)
      SFBconst sbfc ->
	let tenv0 = {tenv with env =
	    Environ.add_constraints sbfc.const_constraints tenv.env } in
	let ttype, tenv0 =
	  let t = const_type_to_term sbfc.const_type in
	  type_trans_aux tenv t
	in
	let id = Id (string_of_label label) in
	let tenv =
	  match sbfc.const_body with
	      None -> (* This is a Coq axiom *)
		push_decl tenv (declaration(id, ttype) tenv)
	    | Some cb -> let tterm, tenv =
			   let c = force cb in
			   term_trans_aux tenv c
                         in
			 push_decl (push_decl tenv (declaration(id, ttype) tenv))
			   (RuleSet [rule([],DVar id, tterm) tenv])
	in
	{ tenv with env =
	    Environ.add_constant
	      (make_con tenv.mp empty_dirpath label)
	      sbfc tenv0.env;
	}

    (* Declaration of a (co-)inductive type. *)
    | SFBmind m ->
      if not m.mind_finite
      then (prerr_endline
	      "mind_translation: coinductive types translated as inductive types";
	    sb_decl_trans tenv (label, SFBmind { m with mind_finite = true })
      )
      else
      (* Add the mutual inductive type declaration to the environment. *)
	let mind = make_mind tenv.mp empty_dirpath label in
	let tenv = { tenv with env =
	    Environ.add_constraints m.mind_constraints
	      (Environ.add_mind mind m tenv.env) }
	in
      (* The names and typing context of the inductive type. *)
	let _,mind_names =
	  Array.fold_left
	    (fun (i, l) _ -> i+1, Ind(mind, i)::l)
	    (0, []) m.mind_packets
	in
      (* Add the inductive type declarations in dedukti. *)
	let tenv,n_decls,c_decls,_ =
	  (Array.fold_right
	     (fun p (te,n_decls,c_decls,i) ->
	       let t, te' = type_trans_aux te
		 (it_mkProd_or_LetIn
		    (Sort (match p.mind_arity with
			Monomorphic ar ->  ar.mind_sort
		      | Polymorphic par ->
			Type par.poly_level))
		    p.mind_arity_ctxt
		 )
	       in
	       let end_type__constr =
		 let n = List.length p.mind_arity_ctxt in
		 App(Ind(mind,i-1),
		     Array.init n (fun i -> Rel(n-i)))
	       in
	       let t__constr, te' = type_trans_aux te'
		 (it_mkProd_or_LetIn
		    (Prod(Anonymous,
			  end_type__constr,
			  lift 1 end_type__constr))
		    p.mind_arity_ctxt
		 )
	       in
	       let n_decls =
		 declaration(Id (string_of_id p.mind_typename),
			     t) te'::n_decls
	       in
	       let c_decls =
		 if m.mind_finite then
		     declaration(Id (string_of_id p.mind_typename ^ "__constr"),
				  t__constr) te'::c_decls
		 else c_decls in
	       te', n_decls, c_decls, i-1)
	     m.mind_packets (tenv,[],[],Array.length m.mind_packets))
	in
	let tenv = List.fold_left push_decl
	  (List.fold_left push_decl tenv n_decls) c_decls
	in
	let tenv,_ =
	  Array.fold_right
	    (fun p (te,i) ->
	      let constr_types = Array.map (substl mind_names) p.mind_nf_lc in
	      let _,env = Array.fold_left
		(fun (j, env) consname ->
		  j + 1,
		  Environ.push_named (consname, None, constr_types.(j)) env)
		(0, tenv.env) p.mind_consnames in
	      packet_translation m.mind_finite {te with env = env }
		(mind, i)
		m.mind_params_ctxt constr_types p, i-1)
	    m.mind_packets (tenv,
			    Array.length m.mind_packets - 1)
	in
	tenv
    | SFBmodule mb ->
      let kn = MPdot(tenv.mp, label) in
      let env = Modops.add_module mb tenv.env in
      let modtype =  Modops.module_type_of_module env (Some kn) mb
      in
      let tenv' = { tenv with env = add_modtype kn modtype env } in
      let tenv'' = mb_trans { tenv' with mp = MPdot(tenv.mp, label)} mb in
      let mod_name = module_path_to_string tenv.mp
	^ "_" ^ string_of_label label
      in
      print_decls mod_name [] (List.rev tenv''.decls);
      tenv'
    | SFBmodtype mty ->
      (* we do not translate module types, but we put them in the
	 environment *)
      let kn = MPdot(tenv.mp, label) in
      { tenv with
	env = add_modtype kn mty tenv.env }


(* translation of an SEB *)
and seb_trans tenv = function
  | SEBstruct sbs ->
    List.fold_left sb_decl_trans tenv sbs


  | SEBfunctor (arg_id, mtb, body) ->
    let kn = MPbound arg_id in
    let env = add_modtype kn mtb tenv.env in
    let env = add_constants env mtb in
    seb_trans { tenv with env = env;
      functors = MPbound arg_id::tenv.functors;
      functor_parameters =
	add_param
	  { tenv with mp = MPbound arg_id }
	  mtb
	      } body

  | SEBident mp ->
    let mod_type =
      (try
	 (lookup_modtype mp tenv.env).typ_expr
       with Not_found ->
	 try
	   (lookup_module mp tenv.env).mod_type
	 with Not_found ->
           prerr_endline ("seb_trans: " ^ string_of_mp mp); raise Not_found
      ) in
    module_copy tenv mp mod_type

  | SEBapply(m1, SEBident m2path, _) ->
    seb_trans { tenv with applied_modules = m2path::tenv.applied_modules }
      m1

  |  _ -> raise (NotImplementedYet "With and Apply")



(* translation of a module body *)
and mb_trans tenv mb =
  let tenv = {tenv with env = Environ.add_constraints mb.mod_constraints tenv.env } in
  match mb.mod_expr with
      Some s -> seb_trans tenv s
    | None -> failwith "empty module body"

(* vi: nolist: sw=2
*)
