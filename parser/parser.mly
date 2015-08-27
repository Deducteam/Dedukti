%parameter <M :
  sig
    val mk_prelude     : Basics.loc -> Basics.ident -> unit
    val mk_declaration : Basics.loc -> Basics.ident -> Term.term -> unit
    val mk_definition  : Basics.loc -> Basics.ident -> Term.term option -> Term.term -> unit
    val mk_definable   : Basics.loc -> Basics.ident -> Term.term -> unit
    val mk_opaque      : Basics.loc -> Basics.ident -> Term.term option -> Term.term -> unit
    val mk_rules       : Rule.rule list -> unit
    val mk_command     : Basics.loc -> Cmd.command -> unit
    val mk_ending      : unit -> unit
  end>
%{
    open Basics
    open Preterm
    open Scoping
    open Rule
    open Cmd
    open M

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
                with Failure "int_of_string" -> 0)
        in
        while List.mem (hstring (Printf.sprintf "%s%d" name_part !int_part)) var_list do
          int_part := !int_part + 1
        done;
        hstring (Printf.sprintf "%s%d" name_part !int_part)
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
        | PreType l | PreId (l,_) | PreQId (l,_,_) | PreLam  (l,_,_,_)
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

    let mk_record_type (lt, ty_name) (params : param list) (lc, constr) (fields : pfield list) =
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
      let rec_name = hstring "record" in
      let field_proj_id n = let s = string_of_ident n in hstring ("proj_" ^ s) in
      let field_proj_preterm (l, n, t) =
        mk_pre_from_list (PreId(l, field_proj_id n) :: params_as_terms @ [PreId(lt, rec_name)])
      in
      let field_proj_subst (l, n, t) = (n, field_proj_preterm (l, n, t)) in
      let param_and_field_patterns f = params_as_patterns @ fields_as_patterns f in
      (* Declare the record type *)
      mk_declaration lt ty_name (scope_term [] (mk_pi (PreType lt) params));
      (* Declare the constructor *)
      mk_declaration lc constr (scope_term [] (mk_pi rec_type (params_and_fields)));
      List.iter
        (fun (lf, field_name, field_type) ->
          let proj_id = field_proj_id field_name in
          (* Declare the projection as definable *)
          mk_definable lf proj_id
            (scope_term []
               (mk_pi
                  (pre_subst (List.map field_proj_subst fields) field_type)
                  (params @ [PDecl(lf, rec_name, rec_type)])));
          (* Define the projection *)
          mk_rules [
            scope_rule (
              lf,
              [(lf,field_name)],
              None,
              proj_id,
              params_as_patterns @
                [PPattern (lf,
                           None,
                           constr,
                           param_and_field_patterns field_name)],
              PreId (lf, field_name))])
        fields

%}

%token EOF
%token DOT
%token COMMA
%token COLON
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
%token <Basics.loc> WHNF
%token <Basics.loc> HNF
%token <Basics.loc> SNF
%token <Basics.loc> STEP
%token <Basics.loc> INFER
%token <Basics.loc> CONV
%token <Basics.loc> CHECK
%token <Basics.loc> PRINT
%token <Basics.loc> GDT
%token <Basics.loc*string> OTHER
%token <Basics.loc> UNDERSCORE
%token <Basics.loc*Basics.ident>NAME
%token <Basics.loc> TYPE
%token <Basics.loc> KW_DEF
%token <Basics.loc*Basics.ident> ID
%token <Basics.loc*Basics.ident*Basics.ident> QID
%token <Basics.loc*string> STRING
%token <Basics.loc*string> NUM
%token <Basics.loc*char> CHAR

%start prelude
%start line
%type <unit> prelude
%type <unit> line
%type <Preterm.prule> rule
%type <Preterm.pdecl> decl
%type <Preterm.param> param
%type <Preterm.pdecl list> context
%type <Basics.loc*Basics.ident option*Basics.ident*Preterm.prepattern list> top_pattern
%type <Preterm.prepattern> pattern
%type <Preterm.prepattern> pattern_wp
%type <Preterm.preterm> sterm
%type <Preterm.preterm> term

%right ARROW FATARROW

%%

prelude         : NAME DOT      { let (lc,name) = $1 in
                                        Pp.name := name;
                                        Scoping.name := name;
                                        mk_prelude lc name }

line            : ID COLON term DOT
                { mk_declaration (fst $1) (snd $1) (scope_term [] $3) }
                | ID param+ COLON term DOT
                { mk_declaration (fst $1) (snd $1) (scope_term [] (mk_pi $4 $2)) }
                | KW_DEF ID COLON term DOT
                { mk_definable (fst $2) (snd $2) (scope_term [] $4) }
                | KW_DEF ID COLON term DEF term DOT
                { mk_definition (fst $2) (snd $2) (Some (scope_term [] $4)) (scope_term [] $6) }
                | KW_DEF ID DEF term DOT
                { mk_definition (fst $2) (snd $2)  None (scope_term [] $4) }
                | KW_DEF ID param+ COLON term DEF term DOT
                { mk_definition (fst $2) (snd $2) (Some (scope_term [] (mk_pi $5 $3)))
                        (scope_term [] (mk_lam $7 $3)) }
                | KW_DEF ID param+ DEF term DOT
                { mk_definition (fst $2) (snd $2) None (scope_term [] (mk_lam $5 $3)) }
                | LEFTBRA ID RIGHTBRA COLON term DEF term DOT
                { mk_opaque (fst $2) (snd $2) (Some (scope_term [] $5)) (scope_term [] $7) }
                | LEFTBRA ID RIGHTBRA DEF term DOT
                { mk_opaque (fst $2) (snd $2)  None (scope_term [] $5) }
                | LEFTBRA ID param+ RIGHTBRA COLON term DEF term DOT
                { mk_opaque (fst $2) (snd $2) (Some (scope_term [] (mk_pi $6 $3)))
                        (scope_term [] (mk_lam $8 $3)) }
                | LEFTBRA ID param+ RIGHTBRA DEF term DOT
                { mk_opaque (fst $2) (snd $2)  None (scope_term [] (mk_lam $6 $3)) }
                | rule+ DOT
                { mk_rules (List.map scope_rule $1) }
                | RECORD ID DEF ID LEFTBRA def_context RIGHTBRA DOT
                { mk_record_type $2 [] $4 $6 }
                | RECORD ID param+ DEF ID LEFTBRA def_context RIGHTBRA DOT
                { mk_record_type $2 $3 $5 $7 }
                | command DOT { $1 }
                | EOF
                { mk_ending () ; raise Tokens.EndOfFile }


command         : WHNF  letterm    { mk_command $1 (Whnf (scope_term [] $2)) }
                | HNF   letterm    { mk_command $1 (Hnf (scope_term [] $2)) }
                | SNF   letterm    { mk_command $1 (Snf (scope_term [] $2)) }
                | STEP  letterm    { mk_command $1 (OneStep (scope_term [] $2)) }
                | INFER letterm    { mk_command $1 (Infer (scope_term [] $2)) }
                | CONV  letterm  COMMA letterm { mk_command $1 (Conv (scope_term [] $2,scope_term [] $4)) }
                | CHECK letterm  COMMA letterm { mk_command $1 (Check (scope_term [] $2,scope_term [] $4)) }
                | PRINT STRING  { mk_command $1 (Print (snd $2)) }
                | GDT   ID      { mk_command $1 (Gdt (None,snd $2)) }
                | GDT   QID     { let (_,m,v) = $2 in mk_command $1 (Gdt (Some m,v)) }
                | OTHER term_lst { mk_command (fst $1) (Other (snd $1,List.map (scope_term []) $2)) }


term_lst        : letterm                                  { [$1] }
                | letterm COMMA term_lst                   { $1::$3 }

param           : LEFTPAR ID COLON term RIGHTPAR        { PDecl (fst $2,snd $2,$4) }
                | LEFTPAR ID DEF term RIGHTPAR          { PDef  (fst $2,snd $2,$4) }

rule            : LEFTSQU context RIGHTSQU top_pattern LONGARROW term
                { let (l,md_opt,id,args) = $4 in ( l , $2 , md_opt, id , args , $6) }

decl            : ID COLON term         { debug "Ignoring type declaration in rule context."; $1 }
                | ID                    { $1 }

def_decl        : ID COLON term         { (fst $1,snd $1,$3) }

context         : /* empty */          { [] }
                | separated_nonempty_list(COMMA, decl) { $1 }

def_context     : /* empty */          { [] }
                | separated_nonempty_list(COMMA, def_decl) { $1 }

top_pattern     : ID pattern_wp*        { (fst $1,None,snd $1,$2) }
                | QID pattern_wp*       { let (l,md,id)=$1 in (l,Some md,id,$2) }


pattern_wp      : ID
                        { PPattern (fst $1,None,snd $1,[]) }
                | QID
                        { let (l,md,id)=$1 in PPattern (l,Some md,id,[]) }
                | UNDERSCORE
                        { PJoker $1 }
                | LEFTBRA term RIGHTBRA
                        { PCondition $2 }
                | LEFTPAR pattern RIGHTPAR
                        { $2 }

pattern         : ID  pattern_wp+
                        { PPattern (fst $1,None,snd $1,$2) }
                | QID pattern_wp+
                        { let (l,md,id)=$1 in PPattern (l,Some md,id,$2) }
                | ID FATARROW pattern
                        { PLambda (fst $1,snd $1,$3) }
                | pattern_wp
                        { $1 }

sterm           : QID
                { let (l,md,id)=$1 in PreQId(l,md,id) }
                | ID
                { PreId (fst $1,snd $1) }
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
                | ID COLON sterm+ ARROW term
                { PrePi (fst $1,Some (snd $1),mk_pre_from_list $3,$5) }
                | term ARROW term
                { PrePi (preterm_loc $1,None,$1,$3) }
                | ID COLON UNDERSCORE FATARROW term
                { PreLam (fst $1,snd $1,None,$5) }
                | ID COLON sterm+ FATARROW term
                { PreLam (fst $1,snd $1,Some(mk_pre_from_list $3),$5) }

letterm         : term
                { $1 }
                | ID DEF term FATARROW letterm
                { mk_let (snd $1) $3 $5 }

%%
