module Z3 =
struct

  open Z3
  open Constraints

  type cfg_item = [`Model of bool | `Proof of bool | `Trace of bool | `TraceFile of string]

  type cfg = cfg_item list

  let cfg = [`Model(true);
             `Proof(false);
             `Trace(true);
             `TraceFile("z3.trace")]

  let string_of_cfg_item item =
    match item with
    | `Model(b) -> ("model", string_of_bool b)
    | `Proof(b) -> ("proof", string_of_bool b)
    | `Trace(b) -> ("trace", string_of_bool b)
    | `TraceFile(file) -> ("trace_file_name", file)

  let string_of_cfg cfg = List.map string_of_cfg_item cfg

  let ctx = mk_context (string_of_cfg cfg)

  let solver = Optimize.mk_opt ctx

  let variables = Hashtbl.create 100

  let add_obj_var left right =
    let le = Arithmetic.mk_le ctx left right in
    Boolean.mk_ite ctx le right left

  let add_obj_var () =
    let zero = Arithmetic.Integer.mk_numeral_i ctx 0 in
    let obj_fun = Hashtbl.fold (fun _ v r -> add_obj_var r v) variables zero in
    Optimize.minimize solver obj_fun

  let register_variable var =
    let zvar = Arithmetic.Integer.mk_const_s ctx var in
    Hashtbl.add variables var zvar;
    let zero = Arithmetic.Integer.mk_numeral_i ctx 0 in
    let le = Arithmetic.mk_le ctx zero zvar in
    Optimize.add solver [le];
    zvar

  let get_variable var =
    if Hashtbl.mem variables var then
      Hashtbl.find variables var
    else
      register_variable var

  let add_constraint_univ var univ =
    let zvar = get_variable var in
    let level =
      match univ with
      | ReverseCiC.Prop ->
        Arithmetic.Integer.mk_numeral_i ctx 0
      | ReverseCiC.Type i ->
        Arithmetic.Integer.mk_numeral_i ctx (i+1)
    in
    let eq = Boolean.mk_eq ctx zvar level in
    Optimize.add solver [eq]

  let add_constraint_eq var var' =
    let zvar = get_variable var in
    let zvar' = get_variable var' in
    let eq = Boolean.mk_eq ctx zvar zvar' in
    Optimize.add solver [eq]

  let add_constraint_succ var var' =
    let zvar = get_variable var in
    let zvar' = get_variable var' in
    let one = Arithmetic.Integer.mk_numeral_i ctx 1 in
    let plus = Arithmetic.mk_add ctx [zvar;one] in
    let eq = Boolean.mk_eq ctx plus zvar' in
    Optimize.add solver [eq]


  let z3_max x y =
    let le = Arithmetic.mk_le ctx x y in
    Boolean.mk_ite ctx le y x

  let add_constraint_lift var var' var'' var''' =
    let zvar = get_variable var in
    let zvar' = get_variable var' in
    let zvar'' = get_variable var'' in
    let zvar''' = get_variable var''' in
    let maxl = Arithmetic.mk_le ctx zvar zvar' in
    let maxr = Arithmetic.mk_le ctx zvar'' zvar''' in
    let eq = Boolean.mk_eq ctx maxl maxr in
    Optimize.add solver [eq]

  let add_constraint_rule var var' var'' =
    let x = get_variable var in
    let y = get_variable var' in
    let z = get_variable var'' in
    let zero = Arithmetic.Integer.mk_numeral_i ctx 0 in
    let zeq0 = Boolean.mk_eq ctx z zero in
    let maxxy = z3_max x y in
    let zeqmax = Boolean.mk_eq ctx z maxxy in
    let yeq0 = Boolean.mk_eq ctx y zero in
    let ite = Boolean.mk_ite ctx yeq0 zeq0 zeqmax in
    Optimize.add solver [ite]

  let add_constraint c =
    let open BasicConstraints in
    let sofi = string_of_var in
    match c with
    | Univ(n,u) -> add_constraint_univ (sofi n) u
    | Eq(n,n') -> add_constraint_eq (sofi n) (sofi n')
    | Succ(n,n') -> add_constraint_succ (sofi n) (sofi n')
    | Lift((n,n'),(n'',n''')) -> add_constraint_lift (sofi n) (sofi n') (sofi n'') (sofi n''')
    | Rule(n,n',n'') -> add_constraint_rule (sofi n) (sofi n') (sofi n'')

  let import cs =
    BasicConstraints.ConstraintsSet.iter add_constraint cs

  let univ_of_int n =
    if n = 0 then
      ReverseCiC.Prop
    else
      ReverseCiC.Type(n-1)

  let var_solution model var =
    let zvar = Hashtbl.find variables var in
    match Model.get_const_interp_e model zvar with
    | None -> failwith "no value associated to a variable"
    | Some e ->
      try
        let s = Arithmetic.Integer.numeral_to_string e in
        int_of_string s
      with _ -> failwith "should be a numeral"

  let solve constraints =
    let open Symbol in
    let open Expr in
    let open Arithmetic in
    Log.append "Generate a Z3 problem...";
    import constraints;
    Log.append (Optimize.to_string solver);
    Log.append "Try to solve the problem...";
    ignore(add_obj_var ());
    match Optimize.check solver with
    | Solver.UNSATISFIABLE ->
      failwith "unsatisfiable"
          (*
          begin
            match Solver.get_proof solver with
            | None -> assert false
            | Some x -> Format.printf "proof@.%s@." (Expr.to_string x)
          end
*)
    | Solver.UNKNOWN -> failwith "unknown"
    | Solver.SATISFIABLE ->
      Log.append "Problem solved!";
      match Optimize.get_model solver with
      | None -> assert false
      | Some model -> (* Format.printf "%s@." (Model.to_string model); *)
        fun uvar ->
          let var' = uvar
                     |> Mapping.to_index
                     |> BasicConstraints.var_of_index
                     |> BasicConstraints.string_of_var in
          (var_solution model var') |> univ_of_int |> ReverseCiC.term_of_univ
end
