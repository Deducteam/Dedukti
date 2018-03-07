module Z3 =
struct

  open Z3
  open Constraints

  type cfg_item = [`Model of bool | `Proof of bool | `Trace of bool | `TraceFile of string]

  type cfg = cfg_item list

  let cfg = [`Model(true);
             `Proof(false);
             `Trace(false)]

  let string_of_cfg_item item =
    match item with
    | `Model(b) -> ("model", string_of_bool b)
    | `Proof(b) -> ("proof", string_of_bool b)
    | `Trace(b) -> ("trace", string_of_bool b)
    | `TraceFile(file) -> ("trace_file_name", file)

  let string_of_cfg cfg = List.map string_of_cfg_item cfg

  let ctx = mk_context (string_of_cfg cfg)

  let solver = Solver.mk_simple_solver ctx

  let variables = Hashtbl.create 100

  let add_obj_var left right =
    let le = Arithmetic.mk_le ctx left right in
    Boolean.mk_ite ctx le right left

  let add_obj_sup i =
    let i = Arithmetic.Integer.mk_numeral_i ctx i in
    Hashtbl.iter (fun _ v ->
        let le = Arithmetic.mk_le ctx v i in Solver.add solver [le]) variables

  let register_variable var =
    let zvar = Arithmetic.Integer.mk_const_s ctx var in
    Hashtbl.add variables var zvar;
    let zero = Arithmetic.Integer.mk_numeral_i ctx 0 in
    let le = Arithmetic.mk_le ctx zero zvar in
    Solver.add solver [le];
    zvar

  let get_variable var =
    if Hashtbl.mem variables var then
      Hashtbl.find variables var
    else
      register_variable var

  let gen_constraint_univ var univ =
    let zvar = get_variable var in
    let level =
      match univ with
      | ReverseCiC.Prop ->
        Arithmetic.Integer.mk_numeral_i ctx 0
      | ReverseCiC.Type i ->
        Arithmetic.Integer.mk_numeral_i ctx (i+1)
    in
    let eq = Boolean.mk_eq ctx zvar level in
    Solver.add solver [eq]

  let gen_constraint_eq var var' =
    let zvar = get_variable var in
    let zvar' = get_variable var' in
    let eq = Boolean.mk_eq ctx zvar zvar' in
    Solver.add solver [eq]

  let gen_constraint_succ var var' =
    let zvar = get_variable var in
    let zvar' = get_variable var' in
    let one = Arithmetic.Integer.mk_numeral_i ctx 1 in
    let plus = Arithmetic.mk_add ctx [zvar;one] in
    let eq = Boolean.mk_eq ctx plus zvar' in
    Solver.add solver [eq]


  let z3_max x y =
    let le = Arithmetic.mk_le ctx x y in
    Boolean.mk_ite ctx le y x

  let gen_constraint_max var var' var'' =
    let zvar = get_variable var in
    let zvar' = get_variable var' in
    let zvar'' = get_variable var'' in
    let max = z3_max zvar zvar' in
    let eq = Boolean.mk_eq ctx max zvar'' in
    Solver.add solver [eq]

  let gen_constraint_lift var var' var'' var''' =
    let zvar = get_variable var in
    let zvar' = get_variable var' in
    let zvar'' = get_variable var'' in
    let zvar''' = get_variable var''' in
    let maxl = Arithmetic.mk_le ctx zvar zvar' in
    let maxr = Arithmetic.mk_le ctx zvar'' zvar''' in
    let eq = Boolean.mk_eq ctx maxl maxr in
    Solver.add solver [eq]

  let gen_constraint_rule var var' var'' =
    let x = get_variable var in
    let y = get_variable var' in
    let z = get_variable var'' in
    let zero = Arithmetic.Integer.mk_numeral_i ctx 0 in
    let zeq0 = Boolean.mk_eq ctx z zero in
    let maxxy = z3_max x y in
    let zeqmax = Boolean.mk_eq ctx z maxxy in
    let yeq0 = Boolean.mk_eq ctx y zero in
    let ite = Boolean.mk_ite ctx yeq0 zeq0 zeqmax in
    Solver.add solver [ite]

  let gen_constraint c =
    let open Naive in
    let sofi = string_of_var in
    match c with
    | Univ(n,u) -> gen_constraint_univ (sofi n) u
    | Eq(n,n') -> gen_constraint_eq (sofi n) (sofi n')
    | Succ(n,n') -> gen_constraint_succ (sofi n) (sofi n')
    | Max(n,n',n'') -> gen_constraint_max (sofi n) (sofi n') (sofi n'')
    | Rule(n,n',n'') -> gen_constraint_rule (sofi n) (sofi n') (sofi n'')

  let import cs =
    Naive.ConstraintsSet.iter gen_constraint cs

  let univ_of_int n =
    if n = 0 then
      ReverseCiC.Prop
    else
      ReverseCiC.Type(n-1)

  let var_solution model var =
    if Hashtbl.mem variables var then
      let zvar = Hashtbl.find variables var in
      match Model.get_const_interp_e model zvar with
      | None -> failwith "no value associated to a variable"
      | Some e ->
        try
          let s = Arithmetic.Integer.numeral_to_string e in
          int_of_string s
        with _ -> failwith "should be a numeral"
    else
      failwith (Format.sprintf "Variable %s not found" var)


  let rec check constraints i =
    let open Symbol in
    let open Expr in
    let open Arithmetic in
    Solver.reset solver;
    Hashtbl.clear variables;
    import constraints;
    add_obj_sup i;
    match Solver.check solver [] with
    | Solver.UNSATISFIABLE ->
      Format.eprintf "No solution found with %d universes@." (i+2);
      check constraints (i+1)
    | Solver.UNKNOWN -> failwith (Format.sprintf "%s" (Solver.get_reason_unknown solver))
    | Solver.SATISFIABLE ->
      match Solver.get_model solver with
      | None -> assert false
      | Some model ->
        let hmodel = Hashtbl.create (UVar.count ()) in
        let find uvar =
          try
            uvar
            |> Naive.var_of_ident
            |> Naive.string_of_var
            |> var_solution model
            |> univ_of_int
            |> ReverseCiC.term_of_univ
          with _ -> ReverseCiC.term_of_univ ReverseCiC.Prop
        in
(*           Format.eprintf "%s@." (Solver.to_string solver);
             Format.eprintf "%s@." (Model.to_string model); *)
        fun (uvar:Basic.ident) : Term.term ->
          if Hashtbl.mem hmodel uvar then
            Hashtbl.find hmodel uvar
          else
            let t = find uvar in
            Hashtbl.add hmodel uvar t;
            t

  let solve constraints = check constraints 0
end
