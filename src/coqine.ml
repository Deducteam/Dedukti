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


let array_forall p a = 
  let r = ref true in
  let i = ref 0 in
  let n = Array.length a in
    while !r && !i < n do
      r := p a.(!i);
      i := !i+1
    done; !r
     

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


let get_dotpi e n t1 t2 =
  let e1 =  push_rel (n,None,t1) e in
    which_dotpi (infer_type e t1, infer_type e1 t2)

let which_e s = match s with
  | Prop Pos  -> "eset"
  | Prop Null -> "eprop"
  | Type _    -> "etype"

exception Not_a_sort

(*********** Sort inference **********)

let rec get_sort env args t = 
  match Reduction.whd_betadeltaiota env t with
    Sort e -> which_e e
    | Prod(_,ty,te) -> (let env = push_rel (Anonymous, None, ty) env in
			  match args with
			      [] -> raise Not_a_sort
			    | a::q -> get_sort env q (subst1 a te))
  | _ -> raise Not_a_sort

let infer_sort env t = 
  let rec aux env args t =
    match collapse_appl (Reduction.whd_betadeltaiota env t) with
	Rel i -> let _,_,ty = lookup_rel i env in
	  get_sort env args ty
      | Var v -> let _,_,ty = lookup_named v env in
	  get_sort env args ty
      | Const c -> (match constant_type env c with
			NonPolymorphicType ty -> get_sort env args ty
		      | _ -> "etype")
      | Sort _ -> "etype"
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
		  Monomorphic ar ->  which_e ar.mind_sort
		| Polymorphic par -> "etype"
	  with Not_found -> failwith ("infer sort: unknown inductive"))
      | Case (ind, ret_ty, matched, branches)  ->
	    (* Get the arguments of the type of the matched term. *)
	  let matched_args =
	    match collapse_appl (Reduction.whd_betadeltaiota env (infer env matched))
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
let get_e e t = (*which_e (infer_type e t)*) infer_sort e t

(* From coq names to string. *)
let path_to_string path =
  let rec aux suffixe = function
      [] -> failwith "empty path"
    | [n] -> n ^ suffixe
    | n::q -> aux ("_" ^ n ^ suffixe) q
  in aux "" path

let name_to_string n = match n with
  | Anonymous -> fresh_var "_dk_anon"
  | Name s -> s

let get_identifier n = match n with
  | Anonymous -> fresh_var "_dk_anon"
  | Name s -> s


(* Get an identifier depending on the current environment e. *)
let rec get_identifier_env e n =
  match n with
    | Anonymous -> get_identifier_env e (Name "_dk_anon")
    | Name s ->
	let rec compute_alpha ch =
	  let rec alpha_counts n = function
	      (Name i,_,_)::q when i = ch -> alpha_counts (n+1) q
	    | (Anonymous, _, _)::q when ch = "_dk_anon" -> alpha_counts (n+1) q
	    | _::q -> alpha_counts n q
	  | [] -> n
	  in
	  let n = alpha_counts 0 e.env_rel_context in
	    if n = 0 then ch
	    else compute_alpha (ch ^ "xxx" ^ string_of_int (n-1))
	in
	  compute_alpha s

let nth_rel_in_env n e =
  let rec aux = function
      1, (n,_,_)::l -> get_identifier_env { e with env_rel_context = l } n
    | n, x::l -> aux (n-1,l)
    | _ -> failwith "nth_rel_in_env: context not big enough"
  in
    aux (n, e.env_rel_context)

(* From coq names to dedukti ids. *)
let name_to_qid n = Id (string_of_id (get_identifier n))


(* base_env is the environment in which inductive declarations are
   progressively added. *)
let base_env = ref empty_env


(* Hash table containing already defined fixpoints *)
let fix_tbl = Hashtbl.create 100


exception Partial_const

(* Translation of t as a term, given an environment e and a set of
   intermediary declarations decls (in reverse order). *)
let rec term_trans_aux e t decls =

  (* Applies every variable in the rel context of the environment e to c. *)
  let rec app_rel_context e c decls = match e.env_rel_context  with
      [] -> [], c, decls
    | (n,_,t)::rel_context ->
	let e = { e with env_rel_context = rel_context } in
	let v = get_identifier_env e n in
	let vs, c, decls1 = app_rel_context e c decls in
	let t_tt, decls2 = type_trans_aux e t decls1 in
	  (Id v, t_tt)::vs, DApp(c,DVar (Id v)), decls
  in

  (* translation of a constructor, with guards
     if the constructor is only partially applied, use eta-expansion
  *)
  let trans_construct mod_path l ind j i args = 
    let m_induc = 
      try lookup_mind ind e
      with Not_found -> failwith ("term translation: unknown inductive "^l) in
    let induc = m_induc.mind_packets.(j) 
    in
    let name = induc.mind_consnames.(i-1) in
    let constr, guard = 
      match mod_path with 
	  MPself _ -> DVar (Id name), 
	    DVar (Id (induc.mind_typename ^ "__constr"))
	| MPfile path -> 
	    let m = path_to_string path in
	      DVar (Qid (m,name)),
	    DVar (Qid (m,induc.mind_typename ^ "__constr"))
	| _ -> raise (NotImplementedYet "modules bound and dot module path")
	    
    in 
    let constr_type = induc.mind_nf_lc.(i-1) in
      try let applied_constr,decls = 
	Array.fold_left 
	  (fun (c,decls) a ->
	     let a_tt, decls' = term_trans_aux e a decls 
	     in
	       DApp(c, a_tt), decls')
	  (constr,decls) args in
      let rec app_args = function
	  Prod(_, _, t2), a::q ->
	    app_args (subst1 a t2, q)
	| App(Rel _, params), [] ->
	    Array.fold_left
	      (fun (c,decls) a ->
		 let a_tt, decls' = term_trans_aux e a decls 
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
	DApp(guard_params, applied_constr), decls
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
	      let e' = push_rel (Name v, None, t1) e in
	      let tt_1, decls' = type_trans_aux e t1 decls in
	      let res, dec = eta e' (v::args) t2 in
		DFun(Id v, tt_1, res), dec
	  | App(Ind _, params) ->
	      let guard, decls	=
		Array.fold_left
		  (fun (c,decls) a ->
		     let a_tt, decls' = term_trans_aux e a decls 
		     in
		       DApp(c, a_tt), decls')
		  (guard,decls) params in
	      let applied_constr = 
                List.fold_right (fun v c -> DApp(c, DVar(Id v))) args constr in
		DApp(guard, applied_constr), decls
	  | Ind _ -> 
	      let applied_constr = 
		List.fold_right (fun v c -> DApp(c, DVar(Id v))) args constr in
		DApp(guard, applied_constr), decls
	  | _ -> failwith "ill-formed type for a constructor"
	in 
	let constr,decls = eta e [] constr_type in
	  Array.fold_left 
	    (fun (c,d) a ->
	       let a_tt, d = term_trans_aux e a d in
		 DApp(c, a_tt), d) (constr,decls) args
  in

    (* The translation is by induction on the term. *)
    match collapse_appl t with
	(* first case is a special case for the constructors *)
	App(Construct(((mod_path,_,l) as ind,j), i), args) ->
	  trans_construct mod_path l ind j i args
	    
      | Construct(((mod_path,_,l) as ind,j), i) ->
	  trans_construct mod_path l ind j i [||]
	    
      | Rel n -> DVar (Id (nth_rel_in_env n e)), decls

      | Var v  -> DVar(Id v), decls

      | Meta _ -> raise ShouldNotAppear

      | Evar _ -> raise ShouldNotAppear

      | Sort s -> (match s with
		     | Prop Null -> DVar (Qid ("Coq1univ","dotprop"))
		     | Prop Pos ->  DVar (Qid ("Coq1univ","dotset"))
		     | Type _ ->    DVar (Qid ("Coq1univ","dottype")))  (*** !!! Attention a Type 0 ***)
	  , decls

      | Cast (t,_,_) -> (* Are casts really needed? *)
	  term_trans_aux e t decls


      | Prod (n,t1,t2)  ->
	  let t_tt1, decls1 = term_trans_aux e t1 decls
	  and e1 = push_rel (n,None,t1) e in
	  let t_tt2, decls2 = term_trans_aux e1 t2 decls1 in
	    DApp (DApp (DVar(Qid("Coq1univ",get_dotpi e n t1 t2)), t_tt1),
		  DFun (Id (get_identifier_env e n),
			DApp(DVar(Qid ("Coq1univ",get_e e t1)), t_tt1),
			t_tt2)), decls2

      | Lambda (n,t1,t2)  ->
	  let t_tt1, decls1 = type_trans_aux e t1 decls
	  and e1 = push_rel (n,None,t1) e in
	  let t_tt2, decls2 = term_trans_aux e1 t2 decls1 in
	    DFun ((Id (get_identifier_env e n)),
		  t_tt1,
		  t_tt2), decls2

      | LetIn (var, eq, ty, body)  ->
	  term_trans_aux e (subst1 eq body) decls 


      | App (t1,a)  -> 
	  Array.fold_left 
	    (fun (u1,decls1) u2 -> 
	       let u_tt2, decls2 = term_trans_aux e u2 decls1 in
		 DApp(u1, u_tt2), decls2)
	    (term_trans_aux e t1 decls) a

      | Const(mod_path,dp,name)  -> (* TODO: treat the module path and the dir path. *)
	  (* depending whether the const is defined here or in
	     another module *)
	  (match mod_path with
	       MPself _ -> DVar (Id name)
	     | MPfile path -> 
		 let m = path_to_string path in
		   DVar (Qid (m,name))
	     | _ -> raise (NotImplementedYet "modules bound and dot module path")
	  ),
	  decls

      | Ind((mod_path,_,l) as ind, num)  -> begin
	  try
	    let name = (lookup_mind ind e).mind_packets.(num).mind_typename in
	      (* depending whether the inductive is defined here or in
		 another module *)
	      (match mod_path with
		   MPself _ -> DVar (Id name)
		 | MPfile path -> 
		     let m = path_to_string path in
		       DVar (Qid (m,name))
		 | _ -> raise (NotImplementedYet "modules bound and dot module path")
	      ),
	    decls
	  with Not_found -> failwith ("term translation: unknown inductive "
				      ^ l) end

      (*      | Construct(((mod_path,_,l) as ind,j), i) -> begin
	      try
	      let induc = (lookup_mind ind e).mind_packets.(j) in
	      let name = indu.mind_consnames.(i-1) in
	      let constr = match mod_path with
	      MPself _ -> DVar (Id name)
	      | MPfile (m :: _) -> (* TODO : use the whole dirpath *)
	      DVar (Qid (m,name))
	      | _ -> raise (NotImplementedYet "modules bound and dot module path")
	      
	      in ,

	      decls
	      with Not_found -> failwith ("term translation: unknown inductive "
	      ^l) end
      *)
      | Case (ind, ret_ty, matched, branches)  ->
	  let mind_body = lookup_mind (fst ind.ci_ind) e in
	  let case_name =
	    mind_body.mind_packets.(snd ind.ci_ind).mind_typename ^ "__case"
	  in
	    (* Get the arguments of the type of the matched term. *)
	  let matched_args =
	    match collapse_appl (Reduction.whd_betadeltaiota e (infer e matched))
	    with App(Ind(i),t) when i = ind.ci_ind -> t
	      | Ind(i) when i = ind.ci_ind -> [||]
	      | _ -> failwith "term_trans: matched term badly typed"
	  in
	  let r = ref (match fst ind.ci_ind with
			   MPself _, _, _ -> DVar (Id case_name)
			 | MPfile path, _ , _  -> 
			     let m = path_to_string path in
			       DVar (Qid (m,case_name))
			 | _ -> raise (NotImplementedYet "modules bound and dot module path")
		      )
	  and d = ref decls in
	    for i = 0 to ind.ci_npar - 1 do 
	      (* We cannot use Array.fold_left since we only need 
		 the parameters. *)
	      let arg_tt, decls' =
		term_trans_aux e matched_args.(i) !d in
		r := DApp(!r, arg_tt);
		d := decls'
	    done;
	    let ret_ty_tt, decls' = term_trans_aux e ret_ty !d in
	      r := DApp(!r, ret_ty_tt);
	      d := decls';
	      Array.iter 
		(fun b -> 
		   let b_tt, decls' = term_trans_aux e b !d in
		     r := DApp(!r, b_tt);
		     d := decls')
		branches;
	      for i = ind.ci_npar to Array.length matched_args - 1 do
		let arg_tt, decls' =  
		  term_trans_aux e matched_args.(i) !d in
		  r := DApp(!r, arg_tt);
		  d := decls'
	      done;
	      let m_tt, decls' = term_trans_aux e matched !d in
		DApp(!r, m_tt), decls'

      | Fix(((struct_arg_nums, num_def),(names, body_types, body_terms))as fix)
	-> begin
	  try Hashtbl.find fix_tbl fix, decls with 
	      Not_found -> 
		(* Get fresh names for the fixpoints. *)
		let names = Array.map
		  (function
		       Name n -> fresh_var (n ^ "_")
		     | Anonymous -> fresh_var "fix_"
		  )
		  names in
		  (* Translation of one inductive fixpoint. *)
		let one_trans struct_arg_num name body_type body_term decls =
		  let fixpoint_context = e.env_rel_context in
		    (* Declare the type of the fixpoint function. *)
		  let decls' =
		    let t, decls' = type_trans_aux
		      { e with env_rel_context = [] }
		      (it_mkProd_or_LetIn
			 body_type
			 fixpoint_context
		      ) decls in
		      Declaration(Id name, t)::decls' in
		    (* Recursively applies all the variables in the context at the
		       point of the fixpoint definition down to the recursive
		       variable, and creates a rule outside of the current context. *)
		  let env_vars, fix, decls' =
		    app_rel_context e (DVar(Id name)) decls' in
		  let rec make_rule e vars fix rhs decls = function
		      0, Prod(n, a, _) ->
			let s = get_identifier_env e n in
			  (* Adds s:Typeofs to the list of things to apply to the
			     fixpoint. *)
			let a_tt, decls' = type_trans_aux e a decls in
			let vars = List.rev_append vars
			  [Id s, a_tt]
			in
			let ind, args = 
			  (* we have to compute a because the inductive type can be 
			     hidden behind a definition. *)
			  match collapse_appl (Reduction.whd_betadeltaiota e a) with
			      App(Ind(i), l) -> i, l
			    | Ind(i) -> i, [||]
			    | _ -> failwith "term translation: structural argument is not an inductive type" 
			in
			let i__constr = 
			  try 
			    let name = (lookup_mind (fst ind) e).mind_packets.(snd ind).mind_typename 
			    in
			      (match fst ind with 
				   MPself _,_,_ -> DVar (Id (name ^ "__constr"))
				 | MPfile path,_,_ -> 
				     let m = path_to_string path in
				       DVar (Qid (m,name ^ "__constr"))
				 | _ -> raise (NotImplementedYet "modules bound and dot module path")
			      )
			  with Not_found -> failwith ("term translation: unknown inductive in structural argument") 
			in
			let guard = Array.fold_left
			  (fun c a ->
			     let a_tt, _ = term_trans_aux e a decls in
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
			let s = get_identifier_env e nom in
			let e' = push_rel (nom, None, a) e in
			let a_tt, decls' = type_trans_aux e a decls in
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
		  let n = List.length e.env_rel_context in
		    (* The variable arguments to pass to fix have de bruijn
		       indices from 0 to n. *)
		  let rel_args = Array.init n (fun i -> Rel (n - i)) in
		    (*rhs_env  adds the names of the mutually defined recursive
		      functions in the context for the rhs*)
		  let _,rhs_env = Array.fold_left
		    (fun (i,e) n ->
		       i+1, 
		       push_named 
			 (n, None, it_mkProd_or_LetIn body_types.(i) fixpoint_context)
			 e)
		    (0,e) names in
		    (* We use the just defined context to replace the indexes in the
		       rhs that refer to recursive calls (the rhs is typed in the
		       context with the recursive functions and their types). *)
		  let sigma = Array.fold_left
		    (fun l n -> App(Var n, rel_args)::l) [] names
		  in
		  let rhs, decls2 =
		    term_trans_aux rhs_env (substl sigma body_term) decls'
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
		let _, t, decls = app_rel_context e (DVar(Id names.(num_def))) decls'
		in
		  if 
		    array_forall (closedn (Array.length body_terms))
		      body_terms
		  then Hashtbl.add fix_tbl fix t;
		  t, decls
	end
      | CoFix   _  -> raise (NotImplementedYet "CoFix")

(*** Translation of t as a type, given an environment e. ***)

and type_trans_aux e t decls = match t with
  | Sort s -> (match s with
		 | Prop Pos  -> DVar(Qid("Coq1univ","Uset"))
		 | Prop Null -> DVar(Qid("Coq1univ","Uprop"))
		 | Type _    -> DVar(Qid("Coq1univ","Utype"))), decls

  | Prod(n,t1,t2) ->  let t_tt1, decls1 = type_trans_aux e t1 decls and e1 = push_rel (n,None,t1) e in
    let t_tt2, decls2 = type_trans_aux e1 t2 decls1 in
      DPi(Id (get_identifier_env e n),t_tt1,t_tt2), decls2

  | t -> let t', decls' = term_trans_aux e t decls in
      DApp(DVar(Qid("Coq1univ",get_e e t)), t'), decls'


(* Translation functions without environment. *)
let term_trans t = term_trans_aux !base_env t []

let type_trans t = type_trans_aux !base_env t []

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
let make_constr env decls params_num params_dec cons_name typ =
  let cons_name_with_params = App(cons_name, 
				  Array.init params_num 
				    (fun i -> Rel(params_dec + params_num - i))) in
  let rec aux e vars decls c = function
      0, Prod(n, t1, t2) -> 
	let v = fresh_var "c_arg_" in
	let e' = push_rel (Name v, None, t1) e in
	let t_tt1, decls' = type_trans_aux e t1 decls in
	  aux e' ((Id v, t_tt1)::vars) decls' (App(c,[|Var v|])) (0, t2)
    | 0, App(_,args) ->
	let ind_num = Array.length args - params_num in
	let ind = Array.make ind_num DKind in
	let d = ref decls in
          for i = 0 to Array.length ind - 1 do
	    let a_i, d' = term_trans_aux e args.(i+params_num) decls in
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
  let res, decls = term_trans_aux env c decls in
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
let packet_translation env ind params constr_types p decls =
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
    let c_tt, decls' = type_trans_aux env c decls in
      Declaration (Id name, c_tt)::decls' in
  let nb_constrs =  Array.length p.mind_consnames in
  let case_name = DVar (Id (p.mind_typename ^ "__case")) in
  let env, param_vars, case_name, this_decls =
    List.fold_right
      (fun (n,_,t) (e, vars, c, decls) ->
	 let v = fresh_var "param_" in
	 let e' = push_rel (Name v, None, t) e in
	 let t_tt, decls' = type_trans_aux e t decls in
	   e',(Id v, t_tt)::vars, DApp(c, DVar (Id v)), decls'
      )
      params (env, [], case_name, [])
  in
  let p_var, case_name, env, this_decls =
    let v = fresh_var "P_" in
    let t = it_mkProd_or_LetIn
      (Prod(Name "i",
            App(Ind(ind),
		let n = List.length p.mind_arity_ctxt in
		  Array.init n (fun i -> Rel (n-i))),
	    Term.Sort (Term.Type (Univ.Atom Univ.Set))))
      indices in
    let e = push_rel (Name v, None, t) env in
    let t_tt, decls' = type_trans_aux env t this_decls in
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
       let e' = push_rel (Name v, None, t) e in
       let t_tt, decls' = type_trans_aux e t decls in
	 i+1,e',(Id v, t_tt)::vars, DApp(c, DVar (Id v)), decls'
    )
    (0,env,[],case_name, this_decls) p.mind_consnames
  in
  let _, this_decls =
    Array.fold_left
      (fun (i, decls) cons_name ->
	 i+1, constr_decl cons_name p.mind_user_lc.(i) decls)
      (0,this_decls) p.mind_consnames
  in
    (* This big piece of code is the type in the Coq world of 
       the __case. 
    *)
  let i__case_coq_type =
    let return_type =
      it_mkProd_or_LetIn
	(Prod(Name "i",
	      App(Ind(ind),
			let n = List.length p.mind_arity_ctxt in
			  Array.init n (fun i -> Rel (n-i))),
		    Term.Sort (Term.Type (Univ.Atom Univ.Set))))
	      indices
	  in
	  let end_type =
	    Prod(Anonymous,
		 App(Ind(ind),
		     let n = List.length p.mind_arity_ctxt in
		       Array.init n (fun i ->
				       if i < n_params
				       then Rel(n - i + nb_constrs + 1)
				       else Rel(n-i))),

		 App(Rel(nb_constrs + 2 + List.length indices),
		     let n = List.length indices + 1
		     in Array.init n (fun i -> Rel(n-i)))
		)	    
	  in
	  let rec lift_indices i = function
	      [] -> []
	    | (a,r,x)::q -> (a, r, liftn (nb_constrs + 1) i x)
		:: lift_indices (i-1) q
	  in
	  let end_type_with_indices =
	    it_mkProd_or_LetIn
	      end_type
	      (lift_indices (List.length indices) indices)
	  in
	  let rec add_functions_from_constrs c = function
	      -1 -> c 
	    | i -> add_functions_from_constrs 
		(Prod(Name "f",
		      make_constr_func_type (ind,i+1) i
			n_params
			constr_types.(i),
		      c))
		  (i-1)
	  in
	    it_mkProd_or_LetIn
	      (Prod(Name "P",
		    return_type,
		    add_functions_from_constrs
		      end_type_with_indices
		      (nb_constrs-1)
		   ))
	      params in
    (* end of i__case_coq_type *)
    

  let params_dec = nb_constrs + 1 in
    (* declaration of the __case type *)
  let i__case_trans, this_decls =
    type_trans_aux env i__case_coq_type this_decls in
  let this_decls =
    Declaration(
      Id (p.mind_typename ^ "__case"),
      i__case_trans)::this_decls
  in
  let _,this_decls =
    Array.fold_left
      (fun (i, d) cons_name ->  
	 let constr, indices, c_vars, d' = 
	   make_constr env d n_params params_dec 
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

(* Translation of a declaration in a structure. *)
let sb_decl_trans label (name, decl) =
  prerr_endline ("declaring "^name);
  match decl with
      (* Declaration of a constant (theorem, definition, etc.). *)
      SFBconst sbfc ->
	base_env := Environ.add_constraints sbfc.const_constraints !base_env;
	let ttype, type_decls = match sbfc.const_type with
	    NonPolymorphicType t -> type_trans t
	  | PolymorphicArity(context, arity) ->
	      (* TODO: Not sure this is really how it works. *)
	      type_trans (it_mkProd_or_LetIn (Sort (Type arity.poly_level)) 
			    context)
	in let decls = 
	    match sbfc.const_body with
		None -> (* This is a Coq axiom *)
		  List.rev_append type_decls [Declaration(Id name, ttype)]
	      | Some cb -> let tterm, term_decls = 
		  match !cb with
		      LSval c -> term_trans c
		    | LSlazy(s,c) -> failwith "not implemented: lazy subst"
		in 
		  List.rev_append term_decls
		    (List.rev_append type_decls [Declaration(Id name, ttype); Rule([],DVar(Id name), tterm)])
	in
	  base_env := Environ.add_constant (Names.MPself label, [], name) sbfc !base_env;
	  decls
	    
    (* Declaration of a (co-)inductive type. *)
    | SFBmind m ->
	if not m.mind_finite
	then prerr_endline
	  "mind_translation: coinductive types may not work properly";
	(* Add the mutual inductive type declaration to the environment. *)
	base_env := Environ.add_mind (Names.MPself label,[],name) m !base_env;
	base_env := Environ.add_constraints m.mind_constraints !base_env;
	(* The names and typing context of the inductive type. *)
	let mind_names, env =
	  let l = ref [] and e = ref !base_env 
	  in
	    for i = 0 to Array.length m.mind_packets - 1 do
	      let p = m.mind_packets.(i) in
		(* For each packet=group of mutal inductive type definitions
		   "p", add the name of the inductive type to l and the
		   declaration of the inductive type with it's kind to the
		   environment. *)
		l := Ind((Names.MPself label, [], name), i)::!l;
		e := Environ.push_rel (Names.Name p.mind_typename, None,
				       (it_mkProd_or_LetIn
					  (Sort (match p.mind_arity with
						     Monomorphic ar ->  ar.mind_sort
						   | Polymorphic par ->
						       Type par.poly_level))
					  p.mind_arity_ctxt))
		  !e
	    done;
	    !l,!e
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
		 (0, env) p.mind_consnames in
		 packet_translation env ((Names.MPself label, [], name), i)
		   m.mind_params_ctxt constr_types p d, i-1)
	    m.mind_packets ([],Array.length m.mind_packets - 1)
	in
	  fst 
	    (Array.fold_right
	       (fun p (d,i) ->
		  let t, d' = type_trans_aux env
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
		      App(Rel(n+i), (* TODO: multiple inductive *)
			  Array.init n (fun i -> Rel(n-i)))
		  in
		  let t__constr, d' = type_trans_aux env
		    (it_mkProd_or_LetIn
		       (Prod(Anonymous, 
			     end_type__constr,
			     lift 1 end_type__constr))
		       p.mind_arity_ctxt
		    )
		    d' in
		    Declaration(Id p.mind_typename,
				t)
		    :: Declaration(Id (p.mind_typename ^ "__constr"),
				   t__constr)
		    :: d', i+1)
	       m.mind_packets (decls,1))
    | _ -> raise (NotImplementedYet "Module, Alias or Module Type")
