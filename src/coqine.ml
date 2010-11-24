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
  Environ.env_of_pre_env
    { Environ.pre_env e with
      Pre_env.env_rel_context = rel }


(**************** Fresh vars **********************)

(* TODO: better fresh vars *)
module VarMap = Map.Make
  (struct
     type t = string
     let compare = compare
   end)

let fresh_map = ref VarMap.empty

(* Get a new name beginning with a prefix. *)
let fresh_var prefix =
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
	      | _ -> failwith "term_trans: matched term badly typed"
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


(* id_with_path i p return Dedukti variable corresponding to p.i *)
let id_with_path self_path mpath id =
  if self_path = mpath then
    DVar (Id id)
  else
    let m = module_path_to_string mpath in
    DVar (Qid (m, id))

let name_to_string n = match n with
  | Anonymous -> fresh_var "_dk_anon"
  | Name s -> string_of_id s

let get_identifier n = match n with
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
	    else compute_alpha (ch ^ "xxx" ^ string_of_int (n-1))
	in
	  compute_alpha (string_of_id s)

let nth_rel_in_env n e =
  let rec aux = function
      1, (n,_,_)::l -> get_identifier_rel l n
    | n, x::l -> aux (n-1,l)
    | _ -> failwith "nth_rel_in_env: context not big enough"
  in
    aux (n, Environ.rel_context e)

let get_identifier_env e = get_identifier_rel (Environ.rel_context e)

(* From coq names to dedukti ids. *)
let name_to_qid n = Id (get_identifier n)


(* base_env is the environment in which inductive declarations are
   progressively added. *)
let base_env = ref Environ.empty_env

let get_base_env () = !base_env

(* Hash table containing already defined closed fixpoints *)
let fix_tbl = Hashtbl.create 100


exception Partial_const

(* Translation of t as a term, given the current module path label, an
   environment e and a set of intermediary declarations decls (in
   reverse order). *)
let rec term_trans_aux laz label e t decls =

  (* Applies every variable in the rel context of the environment e to c. *)
  let app_rel_context e c decls =
    Environ.fold_rel_context
      (fun e (n,_,t) (vs, c, decls1) ->
	(let v = get_identifier_rel (Environ.rel_context e) n in
	 let t_tt, decls2 = type_trans_aux laz label e t decls1 in
	 (Id v, t_tt)::vs, DApp(c,DVar (Id v)), decls2))
      e ~init:([], c, decls)
  in

  (* translation of a constructor, with guards
     if the constructor is only partially applied, use eta-expansion
  *)
  let trans_construct mod_path l ind j i args =
    let m_induc =
      try lookup_mind ind e
      with Not_found -> failwith ("term translation: unknown inductive "^string_of_label l) in
    let induc = m_induc.mind_packets.(j)
    in
    let name = string_of_id induc.mind_consnames.(i-1) in
    let constr = id_with_path label mod_path name
    and guard = id_with_path label mod_path (string_of_id induc.mind_typename ^ "__constr")
    in
    let constr_type = induc.mind_nf_lc.(i-1) in
      try let applied_constr,decls =
	Array.fold_left
	  (fun (c,decls) a ->
	     let a_tt, decls' = term_trans_aux laz label e a decls
	     in
	       DApp(c, a_tt), decls')
	  (constr,decls) args in
      let rec app_args = function
	  Prod(_, _, t2), a::q ->
	    app_args (subst1 a t2, q)
	| App(Rel _, params), [] ->
	    Array.fold_left
	      (fun (c,decls) a ->
		 let a_tt, decls' = term_trans_aux laz label e a decls
		 in
		   DApp(c, a_tt), decls')
	      (guard,decls) params
	| Rel _, [] ->
	    guard, decls
	| _ -> raise Partial_const
      in
      let guard_params, decls =
	app_args (constr_type, Array.to_list args)
      in
	if m_induc.mind_finite
	then
	  DApp(guard_params, applied_constr), decls
	else
	  if laz
	  then DFun(Id (fresh_var "_dk_anon"), DVar(Qid("Coq1univ","lazy")),
	       applied_constr), decls
	  else applied_constr, decls
      with Partial_const ->
	let sigma =
	  let rec aux = function
	      0 -> []
	    | n -> Ind(ind, n-1)::aux (n-1)
	  in aux m_induc.mind_ntypes
	in
	let constr_type = substl sigma constr_type in
	let rec eta e args = function
	    Prod(_, t1, t2) ->
	      let v = fresh_var "g_" in
	      let e' = push_rel (Name (id_of_string v), None, t1) e in
	      let tt_1, decls' = type_trans_aux laz label e t1 decls in
	      let res, dec = eta e' (v::args) t2 in
		DFun(Id v, tt_1, res), dec
	  | App(Ind _, params) ->
	      let guard, decls	=
		Array.fold_left
		  (fun (c,decls) a ->
		     let a_tt, decls' = term_trans_aux laz label e a decls
		     in
		       DApp(c, a_tt), decls')
		  (guard,decls) params in
	      let applied_constr =
                List.fold_right (fun v c -> DApp(c, DVar(Id v))) args constr in
		if m_induc.mind_finite
		then
		  DApp(guard, applied_constr), decls
		else
		  if laz then
		    DFun(Id (fresh_var "_dk_anon"), DVar(Qid("Coq1univ","lazy")),
			 applied_constr), decls
		  else applied_constr, decls
	  | Ind _ ->
	      let applied_constr =
		List.fold_right (fun v c -> DApp(c, DVar(Id v))) args constr in
		if m_induc.mind_finite
		then
		  DApp(guard, applied_constr), decls
		else if laz then
		  DFun(Id (fresh_var "_dk_anon"), DVar(Qid("Coq1univ","lazy")),
		       applied_constr), decls
		else applied_constr, decls
	  | _ -> failwith "ill-formed type for a constructor"
	in
	let constr,decls = eta e [] constr_type in
	  Array.fold_left
	    (fun (c,d) a ->
	       let a_tt, d = term_trans_aux laz label e a d in
		 DApp(c, a_tt), d) (constr,decls) args
  in

  (* translation of an inductive type *)
  let trans_ind ind num args =
    let mod_path, _, l = repr_mind ind in
      try
	let inductive = lookup_mind ind e in
	let name = string_of_id inductive.mind_packets.(num).mind_typename in
	let base,decls = Array.fold_left
	  (fun (u1,decls1) u2 ->
	     let u_tt2, decls2 = term_trans_aux laz label e u2 decls1 in
	       DApp(u1, u_tt2), decls2)
	  (id_with_path label mod_path name, decls) args
	in
	  if inductive.mind_finite || not laz
	  then
	    base, decls
	  else
(*	    let rec eta base args decls = function
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
	    inductive.mind_packets.(num).mind_arity_ctxt)*)
	    raise (NotImplementedYet "coinductives with lazy")
      with Not_found -> failwith ("term translation: unknown inductive "
				  ^ string_of_label l)
  in

    (* The translation is by induction on the term. *)
    match collapse_appl t with
	(* first case is a special case for the constructors *)
        App(Construct((ind,j), i), args) ->
	  let (mod_path,_,l) = repr_mind ind in
          trans_construct mod_path l ind j i args

      | Construct((ind,j), i) ->
	  let (mod_path,_,l) = repr_mind ind in
	  trans_construct mod_path l ind j i [||]

      | App(Ind(ind, num), args) ->
	  trans_ind ind num args

      | Ind(ind, num)  ->
	  trans_ind ind num [||]

      | Rel n -> DVar (Id (nth_rel_in_env n e)), decls

      | Var v  -> DVar(Id (string_of_id v)), decls

      | Meta _ -> raise ShouldNotAppear

      | Evar _ -> raise ShouldNotAppear

      | Sort s -> (match s with
		     | Prop Null -> DVar (Qid ("Coq1univ","dotprop"))
		     | Prop Pos ->  DVar (Qid ("Coq1univ","dotset"))
		     | Type _ ->    DVar (Qid ("Coq1univ","dottype")))  (*** !!! Attention a Type 0 ***)
	  , decls

      | Cast (t,_,_) -> (* Are casts really needed? *)
	  term_trans_aux laz label e t decls


      | Prod (n,t1,t2)  ->
	  let t_tt1, decls1 = term_trans_aux laz label e t1 decls
	  and e1 = push_rel (n,None,t1) e in
	  let t_tt2, decls2 = term_trans_aux laz label e1 t2 decls1 in
	    DApp (DApp (DVar(Qid("Coq1univ",get_dotpi e n t1 t2)), t_tt1),
		  DFun (Id (get_identifier_rel (Environ.rel_context e) n),
			DApp(DVar(Qid ("Coq1univ",get_e e t1)), t_tt1),
			t_tt2)), decls2

      | Lambda (n,t1,t2)  ->
	  let t_tt1, decls1 = type_trans_aux laz label e t1 decls
	  and e1 = push_rel (n,None,t1) e in
	  let t_tt2, decls2 = term_trans_aux laz label e1 t2 decls1 in
	    DFun ((Id (get_identifier_rel (Environ.rel_context e) n)),
		  t_tt1,
		  t_tt2), decls2

      | LetIn (var, eq, ty, body)  ->
	  term_trans_aux laz label e (subst1 eq body) decls


      | App (t1,a)  ->
	  Array.fold_left
	    (fun (u1,decls1) u2 ->
	       let u_tt2, decls2 = term_trans_aux laz label e u2 decls1 in
		 DApp(u1, u_tt2), decls2)
	    (term_trans_aux laz label e t1 decls) a

      | Const co -> let (mod_path,dp,name) = repr_con co in
	  id_with_path label mod_path (string_of_label name), decls

      | Case (ind, ret_ty, matched, branches)  ->
	  let mind_body = lookup_mind (fst ind.ci_ind) e in
	  let case_name =
	    string_of_id mind_body.mind_packets.(snd ind.ci_ind).mind_typename ^ "__case"
	  in
	    (* Get the arguments of the type of the matched term. *)
	  let matched_args =
	    match collapse_appl (Reduction.whd_betadeltaiota e (infer_unsafe e matched))
	    with App(Ind(i),t) when i = ind.ci_ind -> t
	      | Ind(i) when i = ind.ci_ind -> [||]
	      | _ -> failwith "term_trans: matched term badly typed"
	  in
	  let mp, _, _ = repr_mind (fst ind.ci_ind) in
	  let r = ref (id_with_path label mp case_name)
	  and d = ref decls in
	    for i = 0 to ind.ci_npar - 1 do
	      (* We cannot use Array.fold_left since we only need
		 the parameters. *)
	      let arg_tt, decls' =
		term_trans_aux laz label e matched_args.(i) !d in
		r := DApp(!r, arg_tt);
		d := decls'
	    done;
	    let ret_ty_tt, decls' =
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
		  term_trans_aux laz label e ret_ty !d
(*		else
		  trans_ret_ty e !d
		    (Array.length matched_args - ind.ci_npar, ret_ty) *)
	    in
	      r := DApp(!r, ret_ty_tt);
	      d := decls';
	      Array.iter
		(fun b ->
		   let b_tt, decls' = term_trans_aux laz label e b !d in
		     r := DApp(!r, b_tt);
		     d := decls')
		branches;
	      for i = ind.ci_npar to Array.length matched_args - 1 do
		let arg_tt, decls' =
		  term_trans_aux laz label e matched_args.(i) !d in
		  r := DApp(!r, arg_tt);
		  d := decls'
	      done;
	      let m_tt, decls' = term_trans_aux laz label e matched !d in
	      let m_tt =
		if mind_body.mind_finite
		then m_tt
		else DApp(m_tt, DVar(Qid("Coq1univ", "force")))
	      in
		DApp(!r, m_tt), decls'

      | Fix(((struct_arg_nums, num_def),(names, body_types, body_terms))as fix)
	-> begin
	  try Hashtbl.find fix_tbl fix, decls with
	      Not_found ->
		(* If the bodies of the fixpoints are closed, we do not need
		   to add the context, and we can use the hashtable. *)
		let closed =
		  array_forall (closedn (Array.length body_terms))
		    body_terms
		in
		let fixpoint_context, env_context =
		  if closed
		  then [], Environ.rel_context e
		  else Environ.rel_context e, []
		in
		  (* Get fresh names for the fixpoints. *)
		let names = Array.map
		  (function
		       Name n -> fresh_var (string_of_id n ^ "_")
		     | Anonymous -> fresh_var "fix_"
		  )
		  names in
		  (* Translation of one inductive fixpoint. *)
		let one_trans struct_arg_num name body_type body_term decls =
		  (* Declare the type of the fixpoint function. *)
		  let decls' =
		    let t, decls' = type_trans_aux laz label
		      (change_rel_context e env_context)
		      (it_mkProd_or_LetIn
			 body_type
			 fixpoint_context
		      ) decls in
		      Declaration(Id name, t)::decls' in
		    (* Recursively applies all the variables in the context at the
		       point of the fixpoint definition down to the recursive
		       variable, and creates a rule outside of the current context. *)
		  let env_vars, fix, decls' =
		    if closed
		    then [], DVar(Id name), decls'
		    else app_rel_context e (DVar(Id name)) decls' in
		  let rec make_rule e vars fix rhs decls = function
		      0, Prod(n, a, _) ->
			let s = get_identifier_rel (Environ.rel_context e) n in
			  (* Adds s:Typeofs to the list of things to
			     apply to the fixpoint. *)
			let a_tt, decls' = type_trans_aux laz label e a decls in
			let vars = List.rev_append vars
			  [Id s, a_tt]
			in
			let ind, args =
			  (* we have to compute a because the
			     inductive type can be hidden behind a
			     definition. *)
			  match collapse_appl (Reduction.whd_betadeltaiota e a) with
			      App(Ind(i), l) -> i, l
			    | Ind(i) -> i, [||]
			    | _ -> failwith "term translation: structural argument is not an inductive type"
			in
			let i__constr =
			  try
			    let name = (lookup_mind (fst ind) e).mind_packets.(snd ind).mind_typename
			    in
			    let mp,_,_ = repr_mind (fst ind) in
			      id_with_path label mp
				(string_of_id name ^ "__constr")
			  with Not_found -> failwith ("term translation: unknown inductive in structural argument")
			in
			let guard = Array.fold_left
			  (fun c a ->
			     let a_tt, _ = term_trans_aux laz label e a decls in
			       DApp(c, a_tt))
			  i__constr args
			in
			let last_arg = DApp(guard, DVar (Id s)) in
			  (* This is the final case, apply the recursive variable to
			     f and create a rule f x1...xn --> rhs x1...xn. *)
			  Rule(List.rev_append env_vars vars,
			       DApp(fix, last_arg),
			       DApp(rhs, last_arg)
			      )
			  ::decls'
		    | n, Prod(nom, a, t) ->
			let s = get_identifier_rel (Environ.rel_context e) nom
			in
			let e' = push_rel (nom, None, a) e in
			let a_tt, decls' = type_trans_aux laz label e a decls in
			  (* This is the not final case, apply the current variable
			     to f x1...xi and to rhs and call yourself
			     recursively. *)
			  make_rule e' ((Id s, a_tt)::vars)
			    (DApp(fix, DVar (Id s)))
			    (DApp(rhs, DVar (Id s)))
			    decls'
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
		    make_rule e [] fix rhs decls2 (struct_arg_num, body_type)
		in
		  (* And we iterate this process over the body of every. *)
		let _, decls' = Array.fold_left
		  (fun (i,decls) struct_arg_num -> i+1,
		     one_trans struct_arg_num names.(i)
		       body_types.(i) body_terms.(i) decls)
		  (0,decls) struct_arg_nums in
		  (* The term corresponding to the fix point is the identifier
		     to which the context is applied. *)
		let _, t, decls =
		  if closed
		  then [], DVar(Id names.(num_def)), decls'
		  else app_rel_context e (DVar(Id names.(num_def))) decls'
		in
		  if closed then Hashtbl.add fix_tbl fix t;
		  t, decls
	end

      | CoFix (num_def, (names, body_types, body_terms)) ->
	  let closed =
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


(*** Translation of t as a type, given an environment e. ***)

and type_trans_aux laz label e t decls = match t with
  | Sort s -> (match s with
		 | Prop Pos  -> DVar(Qid("Coq1univ","Uset"))
		 | Prop Null -> DVar(Qid("Coq1univ","Uprop"))
		 | Type _    -> DVar(Qid("Coq1univ","Utype"))), decls

  | Prod(n,t1,t2) ->  let t_tt1, decls1 = type_trans_aux laz label e t1 decls and e1 = push_rel (n,None,t1) e in
    let t_tt2, decls2 = type_trans_aux laz label e1 t2 decls1 in
      DPi(Id (get_identifier_env e n),t_tt1,t_tt2), decls2

  | t -> let t', decls' = term_trans_aux laz label e t decls in
      DApp(DVar(Qid("Coq1univ",get_e e t)), t'), decls'


(* Translation functions without environment. *)
let term_trans label t = term_trans_aux true label (get_base_env ()) t []

let type_trans label t = type_trans_aux true label (get_base_env ()) t []

(*** Translation of a declaration in a structure body. ***)


(* e : environment
   decls : accumulator for the declarations
   params_num : number of parameters of the constructor
   params_dec : offset of the parameters in the environment
   cons_name : Coq term of the constructor
   typ : type of the constructor

   returns the constructor with variables for the parameters and the other args
           the indices of the type of constructor
               (arguments that are not parameters)
           the declaration of the new variables
           the auxiliary declarations
*)
let make_constr label env decls params_num params_dec cons_name typ =
  let cons_name_with_params = App(cons_name,
				  Array.init params_num
				    (fun i -> Rel(params_dec + params_num - i))) in
  let rec aux e vars decls c = function
      0, Prod(n, t1, t2) ->
	let v = fresh_var "c_arg_" in
	let e' = push_rel (Name (id_of_string v), None, t1) e in
	let t_tt1, decls' = type_trans_aux true label e t1 decls in
	  aux e' ((Id v, t_tt1)::vars) decls' (App(c,[|Var (id_of_string v)|])) (0, t2)
    | 0, App(_,args) ->
	let ind_num = Array.length args - params_num in
	let ind = Array.make ind_num DKind in
	let d = ref decls in
          for i = 0 to Array.length ind - 1 do
	    let a_i, d' = term_trans_aux true label e args.(i+params_num) decls in
              ind.(i) <- a_i;
	      d := d'
          done;
	  e, c, ind, vars, decls
    | 0, _ -> e, c, [||], vars, decls
    | n, Prod(_, _, t2) -> aux e vars decls c
	(n-1, subst1 (Rel (params_dec + n)) t2)
    | _ -> failwith "inductive translation: ill-typed constructor"
  in
  let _, c, ind, vars, decls = aux env [] decls cons_name_with_params (params_num, typ)
  in
  let res, decls = term_trans_aux false label env c decls in
    res, ind, vars, decls

(* Auxiliary function for make_constr_func_type *)
let rec make_constr_func_type' cons_name num_treated num_param num_args = function
    Prod(n, t1, t2) ->
      Prod(n, t1,
	   make_constr_func_type' cons_name num_treated num_param
	     (num_args+1) t2)
  | App(_,args) ->
      App(App(Rel (num_args + 1 + num_treated),
	      Array.init (Array.length args - num_param)
			(fun i -> args.(i+ num_param))),
		  [| App(Construct cons_name,
			 Array.init (num_args+num_param)
			   (fun i -> if i < num_param
			    then Rel(num_args + 1 + num_treated + num_param - i)
			    else Rel(num_args + num_param - i))) |] )
  |  _ ->
	      App(Rel (num_args + 1 + num_treated),
		  [| App(Construct cons_name,
			 Array.init (num_args+num_param)
			   (fun i -> if i < num_param
			    then Rel(num_args + 1 + num_treated + num_param - i)
			    else Rel(num_args + num_param - i))) |] )


(* Makes the type of the function in the __case of an inductive type
   corresponding to a constructor
   make_constr_func_type cons_name num_treated type :
     cons_name : name of the constructor
     num_treated : number of the constructor in the inductive definition
     num_param : number of parameters
     type : type of the constructor
 *)
let make_constr_func_type cons_name num_treated num_param typ =
  let rec aux = function
      0, t -> make_constr_func_type' cons_name num_treated num_param
	0 (lift num_treated t)
    | n, Prod(_, t1, t2) ->
	aux (n-1, subst1 (Rel(n+1)) t2)
    | _ -> failwith "inductive translation: ill-formed constructor type"
  in aux (num_param, typ)



(* translate a packet of a mutual inductive definition (i.e. a single inductive)
   env : environment
   ind : path of the current inductive
   params : parameter context of the mutual inductive definition
   constr_types : type of the constructors in p
   p : packet
   decls : accumulator of declarations
*)
let packet_translation finite label env ind params constr_types p decls =
  (* translate arguments with lazy, but the base type without *)
  let rec trans_constr e decls = function
      Prod(n,ty,te) ->
	let ty_tt, decls1 = type_trans_aux true label e ty decls
	and e1 = push_rel (n,None,ty) e in
	let te_tt, decls2 = trans_constr e1 decls1 te in
	  DPi(Id (get_identifier_env e n), ty_tt, te_tt), decls2
    | te -> type_trans_aux false label e te decls
  in
  let n_params = List.length params in
    (* Add the constructors to the environment  *)
  let indices = (* arguments that are not parameters *)
    let rec aux accu = function
	0, _ -> List.rev accu
      | n, x :: q -> aux (x::accu) (n-1, q)
      | _ -> failwith "inductive translation: ill-formed arity"
    in aux [] (List.length p.mind_arity_ctxt - n_params,
	       p.mind_arity_ctxt)
  in
  let constr_decl name c decls =
    let c_tt, decls' =
      trans_constr env decls c in
      Declaration (Id name, c_tt)::decls' in
  let nb_constrs =  Array.length p.mind_consnames in
  let case_name = DVar (Id (string_of_id p.mind_typename ^ "__case")) in
  let env, param_vars, case_name, this_decls =
    List.fold_right
      (fun (n,_,t) (e, vars, c, decls) ->
	 let v = fresh_var "param_" in
	 let e' = push_rel (Name (id_of_string v), None, t) e in
	 let t_tt, decls' = type_trans_aux true label e t decls in
	   e',(Id v, t_tt)::vars, DApp(c, DVar (Id v)), decls'
      )
      params (env, [], case_name, [])
  in
  let p_var, case_name, env, this_decls =
    let v = fresh_var "P_" in
    let t = it_mkProd_or_LetIn
      (Prod(Name (id_of_string "i"),
            App(Ind(ind),
		let n = List.length p.mind_arity_ctxt in
		  Array.init n (fun i -> Rel (n-i))),
	    Term.Sort (Term.Type (Univ.Atom Univ.Set))))
      indices in
    let e = push_rel (Name (id_of_string v), None, t) env in
    let t_tt, decls' = type_trans_aux true label env t this_decls in
      (Id v, t_tt),
    DApp(case_name, DVar(Id v)),
    e,
    decls'
  in
  let _,env,func_vars, case_name, this_decls = Array.fold_left
    (fun (i,e,vars,c,decls) cons_name  ->
       let t = make_constr_func_type
	 (ind, i+1) i n_params
	 constr_types.(i) in
       let v = fresh_var "f_" in
       let e' = push_rel (Name (id_of_string v), None, t) e in
       let t_tt, decls' = type_trans_aux true label e t decls in
	 i+1,e',(Id v, t_tt)::vars, DApp(c, DVar (Id v)), decls'
    )
    (0,env,[],case_name, this_decls) p.mind_consnames
  in
  let _, this_decls =
    Array.fold_left
      (fun (i, decls) cons_name ->
	 i+1, constr_decl (string_of_id cons_name) constr_types.(i) decls)
      (0,this_decls) p.mind_consnames
  in
  let _,indices_vars, env', this_decls =
    List.fold_left
      (fun (i,vars,e,decls) (a,_,t) ->
	 let v = fresh_var "in_" in
	 let e' = push_rel (Name (id_of_string v), None, t) e in
	 let t_tt, decls' = type_trans_aux true label e
	   (liftn (nb_constrs + 1) i t) decls in
	   i+1,(Id v, t_tt)::vars, e', decls')
      (0,[],env,this_decls) indices
  in
  let (m,_) as m_var, this_decls =
    let v = fresh_var "m_" in
    let t = App(Ind(ind),
		let n = List.length p.mind_arity_ctxt in
		  Array.init n (fun i ->
				  if i < n_params
				  then Rel(n - i + nb_constrs + 1)
				  else Rel(n-i)))
    in
    let e = push_rel (Name (id_of_string v), None, t) env' in
    let t_tt, decls' = type_trans_aux false label env' t this_decls in
      (Id v, t_tt), decls'
  in
  let end_type, this_decls =
    term_trans_aux true label env' (
	App(Rel(nb_constrs + 1 + List.length indices),
	    let n = List.length indices
	    in Array.init n (fun i -> Rel(n-i)))
    ) this_decls in
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
    (let v,t = m_var in DPi(v,t,end_type)) indices_vars in
  let i__case_dk_type = add_binding_from_vars i__case_dk_type func_vars in
  let i__case_trans = add_binding_from_vars
    (let v,t = p_var in DPi(v,t,i__case_dk_type)) param_vars in
  let params_dec = nb_constrs + 1 in
    (* declaration of the __case type *)
  let this_decls =
    Declaration(
      Id (string_of_id p.mind_typename ^ "__case"),
      i__case_trans)::this_decls
  in
  let _,this_decls =
    Array.fold_left
      (fun (i, d) cons_name ->
	 let constr, indices, c_vars, d' =
	   make_constr label env d n_params params_dec
	     (Construct (ind,i+1)) constr_types.(i) in
	   i+1,
	 Rule(List.rev_append param_vars
		(p_var::List.rev_append func_vars (List.rev c_vars)),
	      DApp(Array.fold_left (fun c a -> DApp(c,a))
		     case_name indices,
		   constr),
	      match List.nth func_vars (List.length func_vars-i-1)
	      with id,_ ->
		List.fold_right (fun (v,_) c -> DApp(c, DVar v))
		  c_vars (DVar id)
	     )::d')
      (0,this_decls) p.mind_consnames in
    List.rev_append this_decls decls



(* Printing declarations *)
let rec print_decls module_name stmts =
  let output_file = open_out (module_name ^ ".dk")
  in
    !pp_obj#output_module output_file
      (match stmts with
	   [] -> (* Dedukti does not like empty files, adding a
		    dummy declaration. *)
	     [Declaration(Id("dummy"),DType)];
	 | _ -> stmts);
    prerr_endline ("closing module " ^ (module_name));
    close_out output_file



(* Translation of a declaration in a structure. *)
let rec sb_decl_trans modpath (label, decl) =
  prerr_endline ("declaring "^ string_of_label label);
  match decl with
      (* Declaration of a constant (theorem, definition, etc.). *)
      SFBconst sbfc ->
	base_env := Environ.add_constraints sbfc.const_constraints !base_env;
	let ttype, type_decls = match sbfc.const_type with
	    NonPolymorphicType t -> type_trans modpath t
	  | PolymorphicArity(context, arity) ->
	      (* TODO: Not sure this is really how it works. *)
	    type_trans modpath (it_mkProd_or_LetIn (Sort (Type arity.poly_level))
				  context)
	in
	let id = Id (string_of_label label) in
	let decls =
	  match sbfc.const_body with
	      None -> (* This is a Coq axiom *)
		List.rev_append type_decls [Declaration(id, ttype)]
	    | Some cb -> let tterm, term_decls =
			   let c = force cb in
			   term_trans modpath c
                         in
			 List.rev_append term_decls
			   (List.rev_append type_decls [Declaration(id, ttype);
							Rule([],DVar id, tterm)])
	in
	base_env := Environ.add_constant
	  (make_con modpath empty_dirpath label)
	  sbfc !base_env;
	decls

    (* Declaration of a (co-)inductive type. *)
    | SFBmind m ->
      if not m.mind_finite
      then prerr_endline
	"mind_translation: coinductive types may not work properly";
      (* Add the mutual inductive type declaration to the environment. *)
      let mind = make_mind modpath empty_dirpath label in
      base_env := Environ.add_mind mind m !base_env;
      base_env := Environ.add_constraints m.mind_constraints !base_env;
	(* The names and typing context of the inductive type. *)
      let _,mind_names =
	Array.fold_left
	  (fun (i, l) _ -> i+1, Ind(mind, i)::l)
	  (0, []) m.mind_packets
      in
	(* Add the inductive type declarations in dedukti. *)
      let decls,_ =
	Array.fold_right
	  (fun p (d,i) ->
	    let constr_types = Array.map (substl mind_names) p.mind_nf_lc in
	    let _,env = Array.fold_left
	      (fun (j, env) consname ->
		j + 1,
		Environ.push_named (consname, None, constr_types.(j)) env)
	      (0, !base_env) p.mind_consnames in
	    packet_translation m.mind_finite modpath env
	      (mind, i)
	      m.mind_params_ctxt constr_types p d, i-1)
	  m.mind_packets ([],Array.length m.mind_packets - 1)
      in
      fst
	(Array.fold_right
	   (fun p (d,i) ->
	     let t, d' = type_trans_aux true modpath !base_env
	       (it_mkProd_or_LetIn
		  (Sort (match p.mind_arity with
		      Monomorphic ar ->  ar.mind_sort
		    | Polymorphic par ->
		      Type par.poly_level))
		  p.mind_arity_ctxt
	       )
	       d in
	     let end_type__constr =
	       let n = List.length p.mind_arity_ctxt in
	       App(Ind(mind,i-1),
		   Array.init n (fun i -> Rel(n-i)))
	     in
	     let t__constr, d' = type_trans_aux true modpath !base_env
	       (it_mkProd_or_LetIn
		  (Prod(Anonymous,
			end_type__constr,
			lift 1 end_type__constr))
		  p.mind_arity_ctxt
	       )
	       d' in
	     Declaration(Id string_of_id p.mind_typename,
			 t)
	     ::
	       (if m.mind_finite then
		   Declaration(Id (string_of_id p.mind_typename ^ "__constr"),
			       t__constr)
		   :: d' else d'), i+1)
	   m.mind_packets (decls,1))
    | SFBmodule mb ->
      base_env := Modops.add_module mb !base_env;
      let decls = mb_trans (MPdot(modpath,label)) mb in
      let mod_name = module_path_to_string modpath
	^ "_" ^ string_of_label label
      in
      print_decls mod_name decls;
      []

    | SFBmodtype mty ->
	(* we do not translate module types, but we put them in the
	   environment *)
      let mp = modpath in
      let kn = MPdot(mp, label) in
      base_env := add_modtype kn mty !base_env;
      []


(* translation of a module body *)
and mb_trans mod_path mb =
  base_env := Environ.add_constraints mb.mod_constraints !base_env;
  match mb.mod_expr with
      Some (SEBstruct sbs) ->
	List.concat
	  (List.map
             (sb_decl_trans mod_path)
	     sbs)

    | Some (SEBfunctor (arg_id, mtb, body)) ->
	(* we do not translate functors, but put them in the environment *)
	(*	base_env := Modops.add_module
		(MPbound arg_id) (Modops.module_body_of_type mtb) !base_env
	*)
      let kn = MPbound arg_id in
      base_env := add_modtype kn mtb !base_env;
      []


(*    | Some(SEBapply _ as app) ->
      let modu = Modops.eval_struct !base_env app in
      mb_trans { mb with mod_expr = Some(modu) }*)

    | Some _ -> raise (NotImplementedYet "With and Ident")

    | None -> failwith "module with empty declarations"
