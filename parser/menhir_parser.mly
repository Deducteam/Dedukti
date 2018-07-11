%{
open Basic
open Preterm
open Internals
open Scoping
open Reduction
open Signature
open Entry


    let fresh_var var_list var =
      if List.mem var var_list then
        (* Split the variable name in a string part and a number part *)
        let s = string_of_ident var in
        let number_part = ref "" in
        let i = ref (String.length s - 1) in
        while s.[!i] >= '0' && s.[!i] <= '9' do
          number_part := Printf.sprintf "%c%s" s.[!i] !number_part;
          i := !i - 1
        done;
        let name_part = String.sub s 0 (!i+1) in
        assert (s = name_part ^ !number_part);
        let int_part =
          ref (try
                  int_of_string !number_part
                with Failure _ -> 0)
        in
        while List.mem (mk_ident (Printf.sprintf "%s%d" name_part !int_part)) var_list do
          int_part := !int_part + 1
        done;
        mk_ident (Printf.sprintf "%s%d" name_part !int_part)
      else
        var

    let rec pre_subst map = function
      | PreId (_, x) as t -> (try List.assoc x map with Not_found -> t)
      | PreQId _
      | PreType _ as t -> t
      | PreApp (f, a, l) -> PreApp (pre_subst map f,
                                   pre_subst map a,
                                   List.map (pre_subst map) l)
      | PrePi (l, None, ty, body) ->
         PrePi (l, None, pre_subst map ty, pre_subst map body)
      | PreLam (l, x, ty, body) ->
         let x' = fresh_var (List.map fst map) x in
         let map' = (x, PreId (l, x')) :: map in
         let ty' =
           match ty with
           | None -> None
           | Some ty -> Some (pre_subst map' ty)
         in
         PreLam (l, x', ty', pre_subst map' body)
      | PrePi (l, Some x, ty, body) ->
         let x' = fresh_var (List.map fst map) x in
         let map' = (x, PreId (l, x')) :: map in
         PrePi (l, Some x', pre_subst map' ty, pre_subst map' body)
      | TypeOf(l,a) -> TypeOf(l, pre_subst map a)

    let mk_let var term body = pre_subst [(var, term)] body

    let rec mk_lam (te:preterm) : param list -> preterm = function
        | [] -> te
        | PDecl(l,x,ty)::tl -> PreLam (l,x,Some ty,mk_lam te tl)
        | PDef(l,x,t)::tl -> mk_let x t (mk_lam te tl)

    let rec mk_pi (te:preterm) : param list -> preterm = function
        | [] -> te
        | PDecl(l,x,ty)::tl -> PrePi(l,Some x,ty,mk_pi te tl)
        | PDef(l,x,t)::tl -> mk_let x t (mk_pi te tl)

    let rec preterm_loc = function
        | PreType l | PreId (l,_) | PreQId (l,_) | PreLam  (l,_,_,_) | TypeOf(l,_)
        | PrePi   (l,_,_,_) -> l
        | PreApp (f,_,_) -> preterm_loc f

    let mk_pre_from_list = function
        | [] -> assert false
        | [t] -> t
        | f::a1::args -> PreApp (f,a1,args)

    let rec filter_map f = function
      | [] -> []
      | a :: l ->
         (match f a with
          | None -> filter_map f l
          | Some b -> b :: filter_map f l)

    let mk_record_type md (lt, ty_name) (params : param list) (lc, constr) (fields : pfield list) : entry list =
      (* Convert params and fields to decls, patterns and terms *)
      let preid (l, x) = PreId (l, x) in
      let pdecl (l,x,ty) = PDecl(l,x,ty) in
      let p_as_decl = function PDecl (l,x,_) -> Some (l,x) | PDef _ -> None in
      let p_as_pattern (l, _) = PJoker l in
      let f_as_pattern f (l, x, _) = if x == f then PPattern (l,None,x,[]) else PJoker l in
      let params_as_decls = filter_map p_as_decl params in
      let params_as_patterns = List.map p_as_pattern params_as_decls in
      let fields_as_patterns f = List.map (f_as_pattern f) fields in
      let params_as_terms = List.map preid params_as_decls in

      let params_and_fields = params @ List.map pdecl fields in

      let rec_type = mk_pre_from_list (PreId (lt, ty_name) :: params_as_terms) in
      let rec_name = mk_ident "record" in
      let field_proj_id n = let s = string_of_ident n in mk_ident ("proj_" ^ s) in
      let field_proj_preterm (l, n, t) =
        mk_pre_from_list (PreId(l, field_proj_id n) :: params_as_terms @ [PreId(lt, rec_name)])
      in
      let field_proj_subst (l, n, t) = (n, field_proj_preterm (l, n, t)) in
      let param_and_field_patterns f = params_as_patterns @ fields_as_patterns f in
      (* Declare the record type *)
      Entry.Decl (lt, ty_name, Signature.Static, scope_term md [] (mk_pi (PreType lt) params)) ::
      (* Declare the constructor *)
      Entry.Decl (lc, constr, Signature.Static, scope_term md [] (mk_pi rec_type (params_and_fields))) ::
      List.flatten (List.map
        (fun (lf, field_name, field_type) ->
          let proj_id = field_proj_id field_name in
          (* Declare the projection as definable *)
          [ Entry.Decl (lf, proj_id, Signature.Definable,
            (scope_term md []
               (mk_pi
                  (pre_subst (List.map field_proj_subst fields) field_type)
                  (params @ [PDecl(lf, rec_name, rec_type)]))));
          (* Define the projection *)
          Entry.Rules [
            scope_rule md (
              lf,
              None,
              [(lf,field_name)],
              None,
              proj_id,
              params_as_patterns @
                [PPattern (lf,
                           None,
                           constr,
                           param_and_field_patterns field_name)],
              PreId (lf, field_name))]])
        fields)

let mk_config loc id1 id2_opt =
  try
    let open Env in
    let some i = Some(int_of_string i) in
    let config nb_steps strategy = {default_cfg with nb_steps; strategy} in
    match (id1, id2_opt) with
    | ("SNF" , None       ) -> config None     Snf
    | ("HNF" , None       ) -> config None     Hnf
    | ("WHNF", None       ) -> config None     Whnf
    | ("SNF" , Some i     ) -> config (some i) Snf
    | ("HNF" , Some i     ) -> config (some i) Hnf
    | ("WHNF", Some i     ) -> config (some i) Whnf
    | (i     , Some "SNF" ) -> config (some i) Snf
    | (i     , Some "HNF" ) -> config (some i) Hnf
    | (i     , Some "WHNF") -> config (some i) Whnf
    | (i     , None       ) -> {default_cfg with nb_steps = some i}
    | (_     , _          ) -> raise Exit (* captured bellow *)
  with _ -> raise (Parse_error(loc, "invalid command configuration"))

let mk_lst f md = [f md]

%}

%token EOF
%token DOT
%token COMMA
%token COLON
%token EQUAL
%token ARROW
%token FATARROW
%token LONGARROW
%token DEF
%token LEFTPAR
%token RIGHTPAR
%token LEFTBRA
%token RIGHTBRA
%token LEFTSQU
%token RIGHTSQU
%token RECORD
%token TYPEOF
%token <Basic.loc> EVAL
%token <Basic.loc> INFER
%token <Basic.loc> CHECK
%token <Basic.loc> CHECKNOT
%token <Basic.loc> ASSERT
%token <Basic.loc> ASSERTNOT
%token <Basic.loc> PRINT
%token <Basic.loc> GDT
%token <Basic.loc> UNDERSCORE
%token <Basic.loc*Basic.mident> NAME
%token <Basic.loc*Basic.mident> REQUIRE
%token <Basic.loc> TYPE
%token <Basic.loc> KW_DEF
%token <Basic.loc> KW_THM
%token <Basic.loc*Basic.ident> ID
%token <Basic.loc*Basic.mident*Basic.ident> QID
%token <Basic.loc*string> STRING
%token <Basic.loc*string> NUM
%token <Basic.loc*char> CHAR

%start line
%type <Basic.mident -> Entry.entry list> line
%type <Preterm.prule> rule
%type <Preterm.pdecl> decl
%type <Preterm.param> param
%type <Preterm.pdecl list> context
%type <Basic.loc*Basic.mident option*Basic.ident*Preterm.prepattern list> top_pattern
%type <Preterm.prepattern> pattern
%type <Preterm.prepattern> pattern_wp
%type <Preterm.preterm> sterm
%type <Preterm.preterm> term

%%

/* prelude         : NAME DOT      { let (lc,name) = $1 in */
/*                                         Pp.name := name; */
/*                                         Scoping.name := name; */
/*                                         mk_prelude lc name } */

line            : ID COLON letterm DOT
                { mk_lst (fun md -> Decl (fst $1, snd $1, Static, scope_term md [] $3)) }
                | ID param+ COLON letterm DOT
                { mk_lst (fun md -> Decl (fst $1, snd $1, Definable, scope_term md [] (mk_pi $4 $2))) }
                | KW_DEF ID COLON arrterm DOT
                { mk_lst (fun md -> Decl (fst $2, snd $2, Definable, scope_term md [] $4)) }
                | KW_DEF ID COLON arrterm DEF letterm DOT
                { mk_lst (fun md -> Def (fst $2, snd $2, false, Some (scope_term md [] $4), scope_term md [] $6)) }
                | KW_DEF ID DEF letterm DOT
                { mk_lst (fun md -> Def (fst $2, snd $2, false, None, scope_term md [] $4)) }
                | KW_DEF ID param+ COLON arrterm DEF letterm DOT
                { mk_lst (fun md -> Def (fst $2, snd $2, false, Some (scope_term md [] (mk_pi $5 $3)), scope_term md [] (mk_lam $7 $3))) }
                | KW_DEF ID param+ DEF letterm DOT
                { mk_lst (fun md -> Def (fst $2, snd $2, false, None, scope_term md [] (mk_lam $5 $3))) }
                | KW_THM ID COLON arrterm DEF letterm DOT
                { mk_lst (fun md -> Def (fst $2, snd $2, true, Some (scope_term md [] $4), scope_term md [] $6)) }
                | KW_THM ID DEF letterm DOT
                { mk_lst (fun md -> Def (fst $2, snd $2, true, None, scope_term md [] $4)) }
                | KW_THM ID param+ COLON arrterm DEF letterm DOT
                { mk_lst (fun md -> Def (fst $2, snd $2, true, Some (scope_term md [] (mk_pi $5 $3)), scope_term md [] (mk_lam $7 $3))) }
                | KW_THM ID param+ DEF letterm DOT
                { mk_lst (fun md -> Def (fst $2, snd $2, true, None, scope_term md [] (mk_lam $5 $3))) }
                | rule+ DOT
                { mk_lst (fun md -> Rules (List.map (scope_rule md) $1)) }
                | RECORD ID DEF ID LEFTBRA def_context RIGHTBRA DOT
                { fun md -> mk_record_type md $2 [] $4 $6 }
                | RECORD ID param+ DEF ID LEFTBRA def_context RIGHTBRA DOT
                { fun md -> mk_record_type md $2 $3 $5 $7 }
                | command DOT { mk_lst $1 }
                | EOF {raise End_of_file}

command         : EVAL te=letterm
                  {fun md -> Eval($1, default_cfg, scope_term md [] te)}
                | EVAL cfg=eval_config te=letterm
                  {fun md -> Eval($1, cfg, scope_term md [] te)}
                | INFER te=letterm
                  {fun md -> Infer($1, default_cfg, scope_term md [] te)}
                | INFER cfg=eval_config te=letterm
                  {fun md -> Infer($1, cfg, scope_term md [] te)}
                | CHECK te=term COLON ty=letterm
                  {fun md ->
                   Check($1, false, false,
                         HasType(scope_term md [] te, scope_term md [] ty))}
                | CHECKNOT te=term COLON ty=letterm
                  {fun md ->
                   Check($1, false, true,
                         HasType(scope_term md [] te, scope_term md [] ty))}
                | ASSERT te=term COLON ty=letterm
                  {fun md ->
                   Check($1, true, false,
                         HasType(scope_term md [] te, scope_term md [] ty))}
                | ASSERTNOT te=term COLON ty=letterm
                  {fun md ->
                   Check($1, true, true,
                         HasType(scope_term md [] te, scope_term md [] ty))}
                | CHECK t1=term EQUAL t2=letterm
                  {fun md ->
                   Check($1, false, false,
                         Convert(scope_term md [] t1, scope_term md [] t2))}
                | CHECKNOT t1=term EQUAL t2=letterm
                  {fun md ->
                   Check($1, false, true,
                         Convert(scope_term md [] t1, scope_term md [] t2))}
                | ASSERT t1=term EQUAL t2=letterm
                  {fun md ->
                   Check($1, true, false,
                         Convert(scope_term md [] t1, scope_term md [] t2))}
                | ASSERTNOT t1=term EQUAL t2=letterm
                  {fun md ->
                   Check($1, true, true,
                         Convert(scope_term md [] t1, scope_term md [] t2))}
                | PRINT STRING {fun _ -> Print($1, snd $2)}
                | GDT ID {fun _ -> DTree($1, None, snd $2)}
                | GDT QID {fun _ -> let (_,m,v) = $2 in DTree($1, Some m, v)}
                | n=NAME {fun _ -> Name(fst n, snd n)}
                | r=REQUIRE {fun _ -> Require(fst r,snd r)}

eval_config:
  | LEFTSQU id=ID RIGHTSQU
      {mk_config (Lexer.loc_of_pos $startpos) (string_of_ident (snd id)) None}
  | LEFTSQU id1=ID COMMA id2=ID RIGHTSQU
      {mk_config (Lexer.loc_of_pos $startpos) (string_of_ident (snd id1))
        (Some(string_of_ident (snd id2)))}

param           : LEFTPAR ID COLON arrterm RIGHTPAR        { PDecl (fst $2, snd $2,$4) }
                | LEFTPAR ID DEF arrterm RIGHTPAR          { PDef  (fst $2, snd $2,$4) }

rule:
  | LEFTSQU context RIGHTSQU top_pattern LONGARROW letterm
      { let (l,md_opt,id,args) = $4 in
        ( l , None, $2 , md_opt, id , args , $6) }
  | LEFTBRA ID RIGHTBRA LEFTSQU context RIGHTSQU top_pattern LONGARROW letterm
      { let (l,md_opt,id,args) = $7 in
        ( l , Some (None,snd $2), $5 , md_opt, id , args , $9)}
  | LEFTBRA QID RIGHTBRA LEFTSQU context RIGHTSQU top_pattern LONGARROW letterm
      { let (l,md_opt,id,args) = $7 in
        let (_,m,v) = $2 in
        ( l , Some (Some m,v), $5 , md_opt, id , args , $9)}

decl            : ID COLON term         { $1 }
                | ID                    { $1 }

def_decl        : ID COLON arrterm         { (fst $1,snd $1,$3) }

context         : /* empty */          { [] }
                | separated_nonempty_list(COMMA, decl) { $1 }

def_context     : /* empty */          { [] }
                | separated_nonempty_list(COMMA, def_decl) { $1 }

top_pattern     : ID pattern_wp*        { (fst $1,None, snd $1,$2) }
                | QID pattern_wp*       { let (l,md,id) = $1 in (l,Some md,id,$2) }


pattern_wp      : ID
                        { PPattern (fst $1,None, snd $1,[]) }
                | QID
                        { let (l,md,id) = $1 in PPattern (l,Some md,id,[]) }
                | UNDERSCORE
                        { PJoker $1 }
                | LEFTBRA term RIGHTBRA
                        { PCondition $2 }
                | LEFTPAR pattern RIGHTPAR
                        { $2 }

pattern         : ID  pattern_wp+
                        { PPattern (fst $1,None, snd $1, $2) }
                | QID pattern_wp+
                        { let (l,md,id) = $1 in PPattern (l,Some md,id,$2) }
                | ID FATARROW pattern
                        { PLambda (fst $1,snd $1,$3) }
                | pattern_wp
                        { $1 }

sterm           : QID
                { let (l,md,id) = $1 in PreQId(l, mk_name md id) }
                | ID
                { PreId (fst $1, snd $1) }
                | LEFTPAR letterm RIGHTPAR
                { $2 }
                | TYPE
                { PreType $1 }
                | NUM
                { Builtins.mk_num $1 }
                | CHAR
                { Builtins.mk_char $1 }
                | STRING
                { Builtins.mk_string $1 }

term            : sterm+
                { mk_pre_from_list $1 }

letterm         : term
                { $1 }
                | ID DEF term FATARROW letterm
                { mk_let (snd $1) $3 $5 }
                | ID COLON term ARROW letterm
                { PrePi (fst $1,Some (snd $1),$3,$5) }
                | term ARROW letterm
                { PrePi (preterm_loc $1,None,$1,$3) }
                | ID COLON UNDERSCORE FATARROW letterm
                { PreLam (fst $1,snd $1,None,$5) }
                | ID COLON term FATARROW letterm
                { PreLam (fst $1,snd $1,Some($3),$5) }
                | ID FATARROW letterm
                { PreLam (fst $1,snd $1,None,$3) }
                | TYPEOF letterm
                { TypeOf (preterm_loc $2, $2) }

arrterm         : term
                { $1 }
                | ID COLON term ARROW arrterm
                { PrePi (fst $1,Some (snd $1),$3,$5) }
                | term ARROW arrterm
                { PrePi (preterm_loc $1,None,$1,$3) }

%%
