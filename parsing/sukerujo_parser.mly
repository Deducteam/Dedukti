%{
open Kernel.Basic
open Sukerujo_preterm
open Scoping
open Kernel.Reduction
open Kernel.Signature
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
        | PreType l | PreId (l,_) | PreQId (l,_) | PreLam  (l,_,_,_)
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
      let p_as_pattern (l, _) = PJoker(l, []) in
      let f_as_pattern f (l, x, _) = if x == f then PPattern (l,None,x,[]) else PJoker(l, []) in
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
      Entry.Decl (lt, ty_name, Public, Static, scope_term md [] (mk_pi (PreType lt) params)) ::
      (* Declare the constructor *)
      Entry.Decl (lc, constr, Public, Static, scope_term md [] (mk_pi rec_type (params_and_fields))) ::
      List.flatten (List.map
        (fun (lf, field_name, field_type) ->
          let proj_id = field_proj_id field_name in
          (* Declare the projection as definable *)
          [ Entry.Decl (lf, proj_id, Public, Definable Free,
            (scope_term md []
               (mk_pi
                  (pre_subst (List.map field_proj_subst fields) field_type)
                  (params @ [PDecl(lf, rec_name, rec_type)]))));
          (* Define the projection *)
          Entry.Rules (lc,[
            scope_rule md (
              lf,
              None,
              [(lf,field_name), None],
              None,
              proj_id,
              params_as_patterns @
                [PPattern (lf,
                           None,
                           constr,
                           param_and_field_patterns field_name)],
              PreId (lf, field_name))])])
        fields)

let mk_config loc lid =
  let strat    = ref None in
  let target   = ref None in
  let nb_steps = ref None in
  let proc = function
    | "SNF"  when !target   = None -> target   := Some Snf
    | "WHNF" when !target   = None -> target   := Some Whnf
    | "CBN"  when !strat    = None -> strat    := Some ByName
    | "CBV"  when !strat    = None -> strat    := Some ByValue
    | "CBSV" when !strat    = None -> strat    := Some ByStrongValue
    | i      when !nb_steps = None -> nb_steps := Some (int_of_string i)
    | _ -> raise Exit in
  try
    List.iter proc lid;
    { default_cfg with
      nb_steps = (!nb_steps);
      target   = (match !target with None -> default_cfg.target | Some t -> t);
      strat    = (match !strat  with None -> default_cfg.strat  | Some s -> s) }
  with _ -> raise @@ Scoping_error(loc, "invalid command configuration")

let loc_of_rs = function
  | [] -> assert false
  | (l,_,_,_,_,_,_) :: _ -> l

let mk_lst x = [x]

let loc_of_rule = function
    | []                  -> assert false
    | (lc,_,_,_,_,_,_)::_ -> lc
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
%token <loc> EVAL
%token <loc> INFER
%token <loc> CHECK
%token <loc> CHECKNOT
%token <loc> ASSERT
%token <loc> ASSERTNOT
%token <loc> PRINT
%token <loc> GDT
%token <loc> UNDERSCORE
%token <loc*mident> NAME
%token <loc*mident> REQUIRE
%token <loc> TYPE
%token <loc> KW_DEF
%token <loc> KW_DEFAC
%token <loc> KW_DEFACU
%token <loc> KW_THM
%token <loc> KW_INJ
%token <loc> KW_PRV
%token <loc*ident> ID
%token <loc*mident*ident> QID
%token <loc*string> STRING
%token <loc*string> NUM
%token <loc*char> CHAR

%start line
%type <mident -> Entry.entry list> line
%type <prule> rule
%type <pdecl> decl
%type <param> param
%type <pdecl list> context
%type <loc*mident option*ident*Preterm.prepattern list> top_pattern
%type <prepattern> pattern
%type <prepattern> pattern_wp
%type <preterm> sterm
%type <preterm> term

%%

/* prelude         : NAME DOT      { let (lc,name) = $1 in */
/*                                         Pp.name := name; */
/*                                         Scoping.name := name; */
/*                                         mk_prelude lc name } */

line:
  | id=ID ps=param* COLON ty=term DOT
    {fun md -> mk_lst @@ Decl(fst id, snd id, Public, Static, scope_term md [] (mk_pi ty ps))}
  | KW_PRV id=ID ps=param* COLON ty=term DOT
    {fun md -> mk_lst @@ Decl(fst id, snd id, Private, Static, scope_term md [] (mk_pi ty ps))}
  | KW_DEF id=ID COLON ty=term DOT
    {fun md -> mk_lst @@ Decl(fst id, snd id, Public, Definable Free, scope_term md [] ty)}
  | KW_PRV KW_DEF id=ID COLON ty=term DOT
    {fun md -> mk_lst @@ Decl(fst id, snd id, Private, Definable Free, scope_term md [] ty)}
  | KW_INJ id=ID COLON ty=term DOT
    {fun md -> mk_lst @@ Decl(fst id, snd id, Public, Injective, scope_term md [] ty)}
  | KW_PRV KW_INJ id=ID COLON ty=term DOT
    {fun md -> mk_lst @@ Decl(fst id, snd id, Private, Injective, scope_term md [] ty)}
  | KW_DEFAC id=ID LEFTSQU ty=term RIGHTSQU DOT
    {fun md -> mk_lst @@ Decl(fst id, snd id, Public, Definable AC, scope_term md [] ty)}
  | KW_PRV KW_DEFAC id=ID LEFTSQU ty=term RIGHTSQU DOT
    {fun md -> mk_lst @@ Decl(fst id, snd id, Private, Definable AC, scope_term md [] ty)}
  | KW_DEFACU id=ID LEFTSQU ty=term COMMA neu=term RIGHTSQU DOT
    {fun md -> mk_lst @@ Decl(fst id, snd id, Public, Definable(ACU(scope_term md [] neu)),
	            scope_term md [] ty)}
  | KW_PRV KW_DEFACU id=ID LEFTSQU ty=term COMMA neu=term RIGHTSQU DOT
    {fun md -> mk_lst @@ Decl(fst id, snd id, Private, Definable(ACU(scope_term md [] neu)),
	            scope_term md [] ty)}
  | KW_DEF id=ID COLON ty=term DEF te=term DOT
    {fun md -> mk_lst @@ Def(fst id, snd id, Public, false, Some(scope_term md [] ty), scope_term md [] te)}
  | KW_DEF id=ID DEF te=term DOT
    {fun md -> mk_lst @@ Def(fst id, snd id, Public, false, None, scope_term md [] te)}
  | KW_DEF id=ID ps=param+ COLON ty=term DEF te=term DOT
    {fun md -> mk_lst @@ Def(fst id, snd id, Public, false, Some(scope_term md [] (mk_pi ty ps)),
                     scope_term md [] (mk_lam te ps))}
  | KW_DEF id=ID ps=param+ DEF te=term DOT
    {fun md -> mk_lst @@ Def(fst id, snd id, Public, false, None, scope_term md [] (mk_lam te ps))}
  | KW_PRV KW_DEF id=ID COLON ty=term DEF te=term DOT
      {fun md -> mk_lst @@ Def(fst id, snd id, Private, false, Some(scope_term md [] ty), scope_term md [] te)}
  | KW_PRV KW_DEF id=ID DEF te=term DOT
      {fun md -> mk_lst @@ Def(fst id, snd id, Private, false, None, scope_term md [] te)}
  | KW_PRV KW_DEF id=ID ps=param+ COLON ty=term DEF te=term DOT
      {fun md -> mk_lst @@ Def(fst id, snd id, Private, false, Some(scope_term md [] (mk_pi ty ps)),
                     scope_term md [] (mk_lam te ps))}
  | KW_PRV KW_DEF id=ID ps=param+ DEF te=term DOT
      {fun md -> mk_lst @@ Def(fst id, snd id, Private, false, None, scope_term md [] (mk_lam te ps))}
  | KW_THM id=ID COLON ty=term DEF te=term DOT
      {fun md -> mk_lst @@ Def(fst id, snd id, Public, true, Some(scope_term md [] ty), scope_term md [] te)}
  | KW_THM id=ID ps=param+ COLON ty=term DEF te=term DOT
      {fun md -> mk_lst @@ Def(fst id, snd id, Public, true, Some(scope_term md [] (mk_pi ty ps)),
                     scope_term md [] (mk_lam te ps))}
  | rs=rule+ DOT
      {fun md -> mk_lst @@ Rules(loc_of_rs rs,(List.map (scope_rule md) rs))}

  | EVAL te=term DOT
      {fun md -> mk_lst @@ Eval($1, default_cfg, scope_term md [] te)}
  | EVAL cfg=eval_config te=term DOT
      {fun md -> mk_lst @@ Eval($1, cfg, scope_term md [] te)}
  | INFER te=term DOT
      {fun md -> mk_lst @@ Infer($1, default_cfg, scope_term md [] te)}
  | INFER cfg=eval_config te=term DOT
      {fun md -> mk_lst @@ Infer($1, cfg, scope_term md [] te)}

  | CHECK te=aterm COLON ty=term DOT
      {fun md -> mk_lst @@ Check($1, false, false, HasType(scope_term md [] te, scope_term md [] ty))}
  | CHECKNOT te=aterm COLON ty=term DOT
      {fun md -> mk_lst @@ Check($1, false, true , HasType(scope_term md [] te, scope_term md [] ty))}
  | ASSERT te=aterm COLON ty=term DOT
      {fun md -> mk_lst @@ Check($1, true , false, HasType(scope_term md [] te, scope_term md [] ty))}
  | ASSERTNOT te=aterm COLON ty=term DOT
      {fun md -> mk_lst @@  Check($1, true , true , HasType(scope_term md [] te, scope_term md [] ty))}

  | CHECK t1=aterm EQUAL t2=term DOT
      {fun md -> mk_lst @@ Check($1, false, false, Convert(scope_term md [] t1, scope_term md [] t2))}
  | CHECKNOT t1=aterm EQUAL t2=term DOT
      {fun md -> mk_lst @@ Check($1, false, true , Convert(scope_term md [] t1, scope_term md [] t2))}
  | ASSERT t1=aterm EQUAL t2=term DOT
      {fun md -> mk_lst @@ Check($1, true , false, Convert(scope_term md [] t1, scope_term md [] t2))}
  | ASSERTNOT t1=aterm EQUAL t2=term DOT
      {fun md -> mk_lst @@ Check($1, true , true , Convert(scope_term md [] t1, scope_term md [] t2))}

  | RECORD ID DEF ID LEFTBRA def_context RIGHTBRA DOT
    { fun md -> mk_record_type md $2 [] $4 $6 }
  | RECORD ID param+ DEF ID LEFTBRA def_context RIGHTBRA DOT
    { fun md -> mk_record_type md $2 $3 $5 $7 }

  | PRINT STRING DOT {fun _ -> mk_lst @@ Print($1, snd $2)}
  | GDT   ID     DOT {fun _ -> mk_lst @@ DTree($1, None, snd $2)}
  | GDT   QID    DOT {fun _ -> mk_lst @@ let (_,m,v) = $2 in DTree($1, Some m, v)}
  | n=NAME       DOT {fun _ -> mk_lst @@ Name(fst n, snd n)}
  | r=REQUIRE    DOT {fun _ -> mk_lst @@ Require(fst r,snd r)}
  | EOF              {raise End_of_file}

eval_config:
  | LEFTSQU l=separated_nonempty_list(COMMA, ID) RIGHTSQU
    {mk_config (Lexer.loc_of_pos $startpos) (List.map (fun x -> string_of_ident (snd x)) l) }

param:
  | LEFTPAR id=ID COLON te=term RIGHTPAR     { PDecl (fst id, snd id, te)}
  | LEFTPAR ID DEF term RIGHTPAR             { PDef  (fst $2, snd $2,$4) }
rule:
  | LEFTSQU context RIGHTSQU top_pattern LONGARROW term
      { let (l,md_opt,id,args) = $4 in
        ( l , None, $2 , md_opt, id , args , $6) }
  | LEFTBRA ID RIGHTBRA LEFTSQU context RIGHTSQU top_pattern LONGARROW term
      { let (l,md_opt,id,args) = $7 in
        ( l , Some (None,snd $2), $5 , md_opt, id , args , $9)}
  | LEFTBRA QID RIGHTBRA LEFTSQU context RIGHTSQU top_pattern LONGARROW term
      { let (l,md_opt,id,args) = $7 in
        let (_,m,v) = $2 in
        ( l , Some (Some m,v), $5 , md_opt, id , args , $9)}

decl:
  | ID COLON term { ($1, Some $3) }
  | ID            { ($1, None   ) }

def_decl        : ID COLON term         { (fst $1,snd $1,$3) }

context         : /* empty */          { [] }
                | separated_nonempty_list(COMMA, decl) { $1 }

def_context     : /* empty */          { [] }
                | separated_nonempty_list(COMMA, def_decl) { $1 }

top_pattern:
  | ID  pattern_wp* { (fst $1,None,snd $1,$2) }
  | QID pattern_wp* { let (l,md,id)=$1 in (l,Some md,id,$2) }

%inline pid:
  | UNDERSCORE { ($1, mk_ident "_") }
  | ID { $1 }

pattern_wp:
  | ID                       { PPattern (fst $1,None,snd $1,[]) }
  | QID                      { let (l,md,id)=$1 in PPattern (l,Some md,id,[]) }
  | UNDERSCORE               { PJoker ($1,[]) }
  | LEFTBRA term RIGHTBRA    { PCondition $2 }
  | LEFTPAR pattern RIGHTPAR { $2 }

pattern:
  | pid FATARROW pattern     { PLambda (fst $1,snd $1,$3) }
  | ID  pattern_wp+          { PPattern (fst $1,None,snd $1,$2) }
  | QID pattern_wp+          { let (l,md,id)=$1 in PPattern (l,Some md,id,$2) }
  | UNDERSCORE pattern_wp+   { PJoker ($1,$2) }
  | pattern_wp               { $1 }

sterm:
  | QID                      { let (l,md,id)=$1 in PreQId(l,mk_name md id) }
  | ID                       { PreId (fst $1,snd $1) }
  | LEFTPAR term RIGHTPAR    { $2 }
  | TYPE                     { PreType $1 }
  | NUM                      { Sukerujo_builtins.mk_num $1 }
  | CHAR                     { Sukerujo_builtins.mk_char $1 }
  | STRING                   { Sukerujo_builtins.mk_string $1 }

aterm:
  | te=sterm ts=sterm*
    {match ts with [] -> te | a::args -> PreApp(te,a,args)}

term:
  | t=aterm
      { t }
  | pid COLON aterm ARROW term
      { PrePi (fst $1,Some (snd $1), $3, $5) }
  | LEFTPAR ID COLON aterm RIGHTPAR ARROW term
      { PrePi (fst $2,Some (snd $2), $4 ,$7) }
  | aterm ARROW term
      { PrePi (Lexer.loc_of_pos $startpos,None,$1,$3) }
  | pid FATARROW term
      {PreLam (fst $1, snd $1, None, $3)}
  | pid COLON aterm FATARROW term
      {PreLam (fst $1, snd $1, Some $3, $5)}
  | LEFTPAR pid DEF aterm RIGHTPAR FATARROW term
      { PreApp (PreLam (fst $2, snd $2, None, $7), $4, []) }
  | LEFTPAR pid COLON aterm DEF aterm RIGHTPAR FATARROW term
    { PreApp (PreLam (fst $2, snd $2, Some $4, $9), $6, []) }

%%
