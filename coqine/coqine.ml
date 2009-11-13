(*****************************************************************************************************)
(*****************************************   CoqInE    ***********************************************)
(*****************************************************************************************************)

open Environ
open Names
open Term
open Typeops
open EuTerms
open Declarations

exception RuleDoesNotExist
exception Typehasnotype
exception NotASort
exception NotACoqVar
exception AnonymousCoqVar
exception NotImplementedYet
exception ShouldNotAppear
exception EmptyArrayInApp


(**************** Fresh vars **********************)

(* TODO: better fresh vars *)
module VarMap = Map.Make 
  (struct 
     type t = string 
     let compare = compare
   end)

let fresh_map = ref VarMap.empty  

(* get a new name beginning with prefix **)
let fresh_var prefix =
  let i =  
    try VarMap.find prefix !fresh_map
    with Not_found -> 0 in
    fresh_map := VarMap.add prefix (i+1) !fresh_map;
  prefix ^ string_of_int i




(*********** Translation constr (i.e. coq term) to euterm  *************)

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

(* given environment e, get the scene in which t plays *)
let get_e e t = which_e (infer_type e t)

(* from coq names to string *)
let name_to_string n = match n with
  | Anonymous -> fresh_var "dk_anon_"
  | Name s -> s

let get_identifier n = match n with
  | Anonymous -> fresh_var "dk_anon_"
  | Name s -> s

(* get an identifier depending on the current environment e *)
let rec get_identifier_env e n = 
  match n with
    | Anonymous -> get_identifier_env e (Name "dk_anon_")
    | Name s -> 
	let rec compute_alpha ch =
	  let rec alpha_counts n = function
	      (Name i,_,_)::q when i = ch -> alpha_counts (n+1) q
	    | (Anonymous, _, _)::q when ch = "dk_anon_" -> alpha_counts (n+1) q
	    | _::q -> alpha_counts n q
	  | [] -> n
	  in
	  let n = alpha_counts 0 e.env_rel_context in
	    if n = 0 then ch
	    else compute_alpha (ch ^ "\230\172\167" ^ string_of_int (n-1))
	in
	  compute_alpha s

let nth_rel_in_env n e = 
  let rec aux = function
      1, (n,_,_)::l -> get_identifier_env { e with env_rel_context = l } n 
    | n, x::l -> aux (n-1,l)
    | _ -> failwith "nth_rel_in_env: context not big enough"
  in
    aux (n, e.env_rel_context)

(* from coq names to dedukti ids *)
let name_to_qid n = Id (string_of_id (get_identifier n))


(* base_env is the environment in which inductive declarations are 
   progressively added *)
let base_env = ref empty_env


(* in a term, we may need extra declarations (for instance when dealing 
   with a fixpoint. They are stocked in declaration_buffer thanks to 
   add_decl, and they are written to a output channel using flush_decl 
*)
let declaration_buffer = ref []
let add_decl d = declaration_buffer := d::!declaration_buffer
let flush_decl out_chan =
  List.iter (output_line out_chan) (List.rev !declaration_buffer);
  declaration_buffer := []



(*** translation of t as a term, given an environment e ***)
let rec term_trans_aux e t = 

  (* applies every variable in the rel context of the environment e to c *) 
  let rec app_rel_context e c = match e.env_rel_context  with
      [] -> [], c
    | (n,_,t)::rel_context -> 
	let e = { e with env_rel_context = rel_context } in
	let v = get_identifier_env e n in
	let vs, c = app_rel_context e c in
	  (Id v, type_trans_aux e t)::vs, EApp(c,EVar (Id v))
  in

  (* add free variables to complete a constructor 
     add_vars free_vars e c (t, params) :
       free_vars : accumulator for the free variables (as a list of pairs 
         id * type
       e : current environment
       c : constr to be completed 
       t : type of constr
       params : parameters of the inductive type
     example : add_vars [] e c (a : A -> b : B -> d : D -> I x y z, [A])
       returns [b0 : B; d0 : D], c b0 d0, [|x; y; z|]
  *)
  let rec add_vars free_vars e c  = function
    | Prod(_,t, q), [] -> let v = fresh_var "var_" in
      let e' = push_rel (Name v, None, t) e in
	add_vars ((Id v,type_trans_aux e t)::free_vars) e' (EApp(c, EVar (Id v))) (q,[])
    | t,[] -> free_vars, c, 
	begin match collapse_appl t with App(_,a) -> a
	  | _ -> Array.init 0 Obj.magic
	end 
    | Prod(_,t, q),arg::args -> 
	let n = List.length e.env_rel_context in
	let e' = push_named (arg, None, it_mkProd_or_LetIn t e.env_rel_context) 
	  e in
	  add_vars free_vars e'
	    (EApp(c, snd (app_rel_context e (EVar (Id arg))))) 
	    (subst1 (App(Var arg, Array.init n (fun m -> Rel (n-m)))) q, args)
    | _ -> failwith "add_vars: too much parameters"
  in 
    (* transform a list of dedukti variables into an array of Coq variable *)
  let vars_to_args l = 
    let rec vars_to_args array = function
	0,[] -> array
      | n,(Id v,_)::q -> array.(n-1) <- Var v; vars_to_args array (n-1,q)
      | _ -> failwith "should not happen"
    in
    let n = List.length l in 
      vars_to_args (Array.make n (Var "dummy")) (n,l)
  in

    (* the translation is by induction on the term *)
    match t with
      | Rel n -> EVar (Id (nth_rel_in_env n e))

      | Var v  -> EVar(Id v)

      | Meta _ -> raise ShouldNotAppear

      | Evar _ -> raise ShouldNotAppear

      | Sort s -> (match s with
                     | Prop Null -> EVar (Qid ("Coq1univ","dotprop"))
                     | Prop Pos ->  EVar (Qid ("Coq1univ","dotset"))
                     | Type _ ->    EVar (Qid ("Coq1univ","dottype")))  (*** !!! Attention a Type 0 ***)

      | Cast _ -> raise NotImplementedYet


      | Prod (n,t1,t2)  -> let t_tt1 = term_trans_aux e t1 and e1 = push_rel (n,None,t1) e in
	  EApp ((EApp (EVar(Qid("Coq1univ",get_dotpi e n t1 t2)), t_tt1)),
		(EFun (Id (get_identifier_env e n),
                       (EApp(EVar(Qid ("Coq1univ",get_e e t1)),t_tt1)),
                       (term_trans_aux e1 t2))))

      | Lambda (n,t1,t2)  ->  (EFun ((Id (get_identifier_env e n)),
                                     (type_trans_aux e t1),
                                     (term_trans_aux (push_rel (n,None,t1) e) t2)))

      | LetIn (var, eq, ty, body)  ->
	  term_trans_aux e (App(Lambda(var, ty, body), [| eq |]))

      | App (t1,a)  -> Array.fold_left (fun u1 u2 -> EApp(u1,term_trans_aux e u2))
	  (term_trans_aux e t1) a

      | Const(mp,dp,l)  -> (* TODO: treat the module path and the dir path *)
	  EVar(Id l)

      | Ind(ind, num)  -> begin
	  try EVar (Id (lookup_mind ind e).mind_packets.(num).mind_typename)
	  with Not_found -> failwith "term translation: unknown inductive" end

      | Construct((ind,j), i) -> begin
	  try 
	    EVar (Id (lookup_mind ind e).mind_packets.(j).mind_consnames.(i-1))
	  with Not_found -> failwith "term translation: unknown inductive" end

      | Case (ind, ret_ty, matched, branches)  -> 
	  let mind_body = lookup_mind (fst ind.ci_ind) e in
	  let case_name = 
	    mind_body.mind_packets.(snd ind.ci_ind).mind_typename ^ "__case"  
	  in
	    (* get the arguments of the type of the matched term *)
	  let matched_args = 
	    match collapse_appl (Reduction.whd_betadeltaiota e (infer e matched))
	    with App(Ind(i),t) when i = ind.ci_ind -> t
	      | Ind(i) when i = ind.ci_ind -> Array.init 0 Obj.magic
	      | _ -> failwith "term_trans: matched term badly typed"
	  in
	  let r = ref (EVar (Id case_name)) in
	    for i = 0 to ind.ci_npar - 1 do
              r := EApp(!r, term_trans_aux e matched_args.(i))
	    done;
	    r := EApp(!r, term_trans_aux e ret_ty);
	    Array.iter (fun b -> r := EApp(!r, term_trans_aux e b)) branches;
	    for i = ind.ci_npar to Array.length matched_args - 1 do
	      r := EApp(!r, term_trans_aux e matched_args.(i))
	    done;
	    EApp(!r, term_trans_aux e matched)
	      
      | Fix((struct_arg_nums, num_def),(names, body_types, body_terms)) -> 
	  (* may create an unterminating rule *)
	  (* get fresh names for the fixpoints *)
	  let names = Array.map 
	    (function 
		 Name n -> fresh_var (n ^ "_")
	       | Anonymous -> fresh_var "fix_"
	    )
	    names in
	    (* translation of one inductive fixpoints *)
	  let one_trans struct_arg_num name body_type body_term =
	    (* declare the type of the fixpoint function *)
	    add_decl (Declaration(Id name, type_trans_aux 
				    { e with env_rel_context = [] }
				    (it_mkProd_or_LetIn
				       body_type
				       e.env_rel_context
				    )));
	    let env_vars, fix = 
	      app_rel_context e (EVar(Id name)) in
	    let rec make_rule e vars fix rhs = function
		0, Prod(n, a, _) ->
		  let s = get_identifier_env e n in
		  let vars = List.rev_append vars 
		    [Id s, type_trans_aux e a]
		  in 
		    add_decl(
		      Rule(List.rev_append env_vars vars,
			   EApp(fix, EVar (Id s)),
			   EApp(rhs, EVar (Id s))
			  ))
	      | n, Prod(nom, a, t) -> 
		  let s = get_identifier_env e nom in
		  let e' = push_rel (nom, None, a) e in
		    make_rule e' ((Id s, type_trans_aux e a)::vars)
		      (EApp(fix, EVar (Id s)))
		      (EApp(rhs, EVar (Id s)))
		      (n-1, t)
	      | _ -> failwith "fixpoint translation: ill-formed type"
	    in
	    let n = List.length e.env_rel_context in
	    let rel_args = Array.init n (fun i -> Rel (n - i)) in
	    let _,rhs_env = Array.fold_left 
	      (fun (i,e) n ->
		 i+1, push_named (n, None, body_types.(i)) e)
	      (0,e) names
	    in
	    let sigma = Array.fold_left 
	      (fun l n -> App(Var n, rel_args)::l) [] names
	    in
	    let rhs = term_trans_aux rhs_env (substl sigma body_term)
	    in
	      make_rule e [] fix rhs (struct_arg_num, body_type)
	  in
	    Array.iteri 
	      (fun i struct_arg_num ->
		 one_trans struct_arg_num names.(i) 
		   body_types.(i) body_terms.(i)) 
	      struct_arg_nums;
	    snd (app_rel_context e (EVar(Id names.(num_def))))
	      
      | CoFix   _  -> raise NotImplementedYet

(*** translation of t as a type, given an environment e ***)

and type_trans_aux e t = match t with
  | Sort s -> (match s with
		 | Prop Pos  -> EVar(Qid("Coq1univ","Uset"))
		 | Prop Null -> EVar(Qid("Coq1univ","Uprop"))
		 | Type _    -> EVar(Qid("Coq1univ","Utype")))

  | Prod(n,t1,t2) ->  let t_tt1 = type_trans_aux e t1 and e1 = push_rel (n,None,t1) e in
      EPi(Id (get_identifier_env e n),t_tt1,(type_trans_aux e1 t2))

  | t -> EApp(EVar(Qid("Coq1univ",get_e e t)),(term_trans_aux e t))


(* translation functions without environment *)
let term_trans t = term_trans_aux !base_env t

let type_trans t = type_trans_aux !base_env t




(*** translation of a declaration in a structure body ***)

let rec add_ind_and_constr ?(i=0) m p e vars cons_name = function
    [], Prod(n,t1,t2) ->
      let v = fresh_var "c_arg_" in
      let e' = push_rel (Name v, None, t1) e in
        add_ind_and_constr m p e' ((Id v, type_trans_aux e t1)::vars)
          (EApp(cons_name, EVar (Id v))) ([],t2)
  | [], App(_,args) ->
      let ind =
        Array.make (Array.length args - m.mind_nparams) EKind in
        for i = 0 to Array.length ind - 1 do
          ind.(i) <- term_trans_aux e args.(i+m.mind_nparams)
        done;
        cons_name, ind, vars
  | [], _ ->
      let ind =
        Array.init 0 Obj.magic in
        cons_name, ind, vars
  |  (id, _)::q, Prod(n,t1,t2) ->
       let t2 = subst1 (Rel (Array.length p.mind_consnames - i +
                               m.mind_nparams + 1)) t2 in
         add_ind_and_constr ~i:(i+1) m p e vars (EApp(cons_name, EVar id)) (q,t2)
  | _ -> failwith "inductive translation: ill-typed constructor"

	  

(* translation of a declaration in a structure *)
let sb_decl_trans label (name, decl) = match decl with
    (* declaration of a constant (theorem, definition, etc.) *)
    SFBconst sbfc -> 
      base_env := Environ.add_constraints sbfc.const_constraints !base_env;
      let tterm = match sbfc.const_body with
	  Some cb -> begin
	    match !cb with
		LSval c -> term_trans c 
	      | LSlazy(s,c) -> failwith "not implemented: lazy subst"
	  end
	| None -> failwith "no term given"
      and ttype = match sbfc.const_type with 
	  NonPolymorphicType t -> type_trans t
	| _ -> failwith "not implemented: polymorphic types"
      in
	flush_decl stdout;
	output_line stdout (Declaration(Id name, ttype)); 
	output_line stdout (Rule([],EVar(Id name), tterm));
        base_env := Environ.add_constant (Names.MPself label, [], name) sbfc !base_env

  (* declaration of a (co-)inductive type *)
  | SFBmind m ->
      if not m.mind_finite 
      then prerr_endline "mind_translation: coinductive types may not work properly";
      base_env := Environ.add_mind (Names.MPself label,[],name) m !base_env;
      let mind_names, env = 
	let l = ref []
	and e = ref 
	  (Environ.add_constraints m.mind_constraints !base_env) 
	in
	  for i = 0 to Array.length m.mind_packets - 1 do
	    let p = m.mind_packets.(i) in
	      l := Ind((Names.MPself label, [], name), i)::!l;
	      e := Environ.push_rel (Names.Name p.mind_typename, None, 
				     match p.mind_arity with
					 Monomorphic ar -> ar.mind_user_arity
				       | Polymorphic par -> 
					   Sort (Type par.poly_level)) !e
	  done;
	  !l,!e
      in
	Array.iter
	  (fun p -> output_line stdout
	     (Declaration(Id p.mind_typename, 
			  type_trans_aux env 
			    (it_mkProd_or_LetIn
			       (Sort (match p.mind_arity with
					  Monomorphic ar ->  ar.mind_sort
					| Polymorphic par -> 
					    Type par.poly_level))
			       p.mind_arity_ctxt
			    ))))
	  m.mind_packets;
	let packet_translation p i = 
	let _,env = Array.fold_left 
	  (fun (j, env) consname ->
	     j + 1,
	     Environ.push_named (consname, None, substl mind_names p.mind_nf_lc.(j)) env) (0, env) p.mind_consnames in
	let rec make_constr_func_type cons_name num_treated num_args = 
	  function 
	      0, t -> make_constr_func_type cons_name num_treated num_args (-1,lift num_treated t)
	    | -1, Prod(n, t1, t2) ->
		Prod(n, t1,
		     make_constr_func_type cons_name num_treated
		       (num_args+1) (-1, t2))
	    | -1, App(_,args) ->
		  App(App(Rel (num_args + 1 + num_treated),
			  Array.init (Array.length args - m.mind_nparams) 
			    (fun i -> args.(i+ m.mind_nparams))),
		      [| App(Var cons_name, 
			     Array.init (num_args+m.mind_nparams) 
			       (fun i -> if i < m.mind_nparams 
				then Rel(num_args + 1 + num_treated + m.mind_nparams - i)
				else Rel(num_args + m.mind_nparams - i))) |] )
	    | -1, _ ->
		  App(Rel (num_args + 1 + num_treated),
		      [| App(Var cons_name, 
			     Array.init (num_args+m.mind_nparams) 
			       (fun i -> if i < m.mind_nparams 
				then Rel(num_args + 1 + num_treated + m.mind_nparams - i)
				else Rel(num_args + m.mind_nparams - i))) |] )
	    | n, Prod(_, t1, t2) -> 
		make_constr_func_type cons_name num_treated num_args
		  (n-1, subst1 (Rel(n+1)) t2)
	    | _ -> failwith "inductive translation: ill-formed constructor type"
	in
	let indices = 
	  let rec aux accu = function
	      0, _ -> List.rev accu
	    | n, x :: q -> aux (x::accu) (n-1, q)
	    | _ -> failwith "inductive translation: ill-formed arity"
	  in aux [] (List.length p.mind_arity_ctxt - m.mind_nparams,
		     p.mind_arity_ctxt) 
	in
	let case_decl = 
	  Declaration(
	    Id (p.mind_typename ^ "__case"),
	    type_trans_aux env (
	      it_mkProd_or_LetIn
		(Prod(Name "P", 
		      it_mkProd_or_LetIn
			(Prod(Name "i", 
			      App(Ind((Names.MPself label, [], name), i),
				   let n = List.length p.mind_arity_ctxt in
				     Array.init n (fun i -> Rel (n-i))),
			       Term.Sort (Term.Type (Univ.Atom Univ.Set))))

			 indices,
			snd 
			  (Array.fold_right
			     (fun cons_name (i,c) ->
				i-1, Prod(Name "f",
					  make_constr_func_type cons_name i 0 
					    (m.mind_nparams, 
					     substl 
					       mind_names
					       p.mind_nf_lc.(i)),
					  c))
			     p.mind_consnames 
			     (Array.length p.mind_consnames-1,
			      it_mkProd_or_LetIn
				(Prod(Anonymous, 
				      App(Ind((Names.MPself label, [], name), i),
					     let n = List.length p.mind_arity_ctxt in
					       Array.init n (fun i ->
							       if i < m.mind_nparams 
							       then Rel(n - i + Array.length p.mind_consnames + 1) 
							       else Rel(n-i))),
					       
				      App(Rel(Array.length p.mind_consnames + 2 + List.length indices),
					  let n = List.length indices + 1 
					  in Array.init n (fun i -> Rel(n-i)))
				     ))
				(List.map 
				   (fun (a,r,t) ->
				      a, r, 			       
				      lift (Array.length p.mind_consnames + 1) 
					t) indices)
			     ))
		     ))
		m.mind_params_ctxt))
	in
	let constr_decl name c = 
	  Declaration (Id name, type_trans_aux env c) in
	let nb_consts =  Array.length p.mind_consnames in
	let lref = Array.make (2 * nb_consts + 1) case_decl in
	  let case_name = EVar (Id (p.mind_typename ^ "__case")) in
	  let _, env, param_vars, case_name = 
	    List.fold_left 
	      (fun (i, e, vars, c) (n,_,t) ->
		 if i = 0 then i, e, vars, c 
		 else 
		   let v = fresh_var "param_" in
		   let e' = push_rel (Name v, None, t) e in
		     i-1,e',(Id v, type_trans_aux e t)::vars, EApp(c, EVar (Id v))
	      )
	      (m.mind_nparams, env, [], case_name) (List.rev p.mind_arity_ctxt)
	  in
	  let p_var, case_name, env = 
	    let v = fresh_var "P_" in
	    let t = it_mkProd_or_LetIn
	      (Prod(Name "i", 
		    App(Ind((Names.MPself label, [], name), i),
			let n = List.length p.mind_arity_ctxt in
			  Array.init n (fun i -> Rel (n-i))),
		    Term.Sort (Term.Type (Univ.Atom Univ.Set))))
	      indices in
	    let e = push_rel (Name v, None, t) env in
	      (Id v, type_trans_aux env t), 
	    EApp(case_name, EVar(Id v)),
	    e 
	  in
	  let _,env,func_vars, case_name = Array.fold_left 
	    (fun (i,e,vars,c) cons_name  ->
	       let t = make_constr_func_type cons_name i 0 
		 (m.mind_nparams, substl mind_names p.mind_nf_lc.(i)) in
	       let v = fresh_var "f_" in
	       let e' = push_rel (Name v, None, t) e in
		 i+1,e',(Id v, type_trans_aux e t)::vars, EApp(c, EVar (Id v))
	    )
	    (0,env,[],case_name) p.mind_consnames
	  in 
	    Array.iteri 
	      (fun i cons_name -> 
		 let constr, indices, c_vars = add_ind_and_constr 
		   m p env [] 
		   (EVar (Id  cons_name)) 
		   (List.rev param_vars,
		    substl mind_names p.mind_nf_lc.(i)) in
		   lref.(nb_consts + i + 1) <- 
		     Rule(List.rev_append param_vars 
			    (p_var::List.rev_append func_vars (List.rev c_vars)),
			  EApp(Array.fold_left (fun c a -> EApp(c,a))
				 case_name indices,
			       constr),
			  match List.nth func_vars (List.length func_vars-i-1)
			  with id,_ -> 
			    List.fold_right (fun (v,_) c -> EApp(c, EVar v))
			      c_vars (EVar id) 
			 )
	      ) p.mind_consnames;
	    for i = 0 to nb_consts - 1 do
	      lref.(i) <- constr_decl p.mind_consnames.(i) p.mind_user_lc.(i);
	    done; lref
      in
	for i = 0 to Array.length m.mind_packets - 1 do
	  let decls = packet_translation m.mind_packets.(i) i in
	    flush_decl stdout;
	    Array.iter (output_line stdout) decls
	done
  | _ -> raise NotImplementedYet


