%parameter <M :
  sig
    val mk_prelude     : Basics.loc -> Basics.ident -> unit
    val mk_declaration : Basics.loc -> Basics.ident -> Term.term -> unit
    val mk_definition  : Basics.loc -> Basics.ident -> Term.term option -> Term.term -> unit
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

    let rec mk_lam (te:preterm) : pdecl list -> preterm = function
        | [] -> te
        | PDecl(l,x,ty)::tl -> PreLam (l,x,Some ty,mk_lam te tl)
        | PDef(l,x,t)::tl -> mk_let x t (mk_lam te tl)

    let rec mk_pi (te:preterm) : pdecl list -> preterm = function
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

    let mk_record_type (lt, ty_name) params (lc, constr) fields =
      let pdecl (l,x,ty) = PDecl(l,x,ty) in
      let params_and_fields = params @ fields in
      let pattern_of_decl = function
        | PDecl (l, x, t) -> Some (PPattern (l,None,x,[]))
        | PDef _ -> None
      in
      let app_params =
        filter_map
          (function
            | PDecl (l,x,t) -> Some (PreId (l,x))
            | PDef _ -> None)
          params
      in
      let param_patterns = filter_map pattern_of_decl params in
      let rec_type = mk_pre_from_list (PreId (lt, ty_name) :: app_params) in
      let rec_name = hstring "record" in
      let field_proj_id n = let s = string_of_ident n in hstring ("proj_" ^ s) in
      let field_proj_preterm (l, n, t) =
        mk_pre_from_list (PreId(l, field_proj_id n) :: app_params @ [PreId(lt, rec_name)])
      in
      let field_proj_subst = function
        | PDecl (l, n, t) -> (n, field_proj_preterm (l, n, t))
        | PDef (l, n, t) -> (n, t)
      in
      let param_and_field_patterns = filter_map pattern_of_decl (params_and_fields) in
      mk_declaration lt ty_name (scope_term [] (mk_pi (PreType lt) params));
      mk_declaration lc constr (scope_term [] (mk_pi rec_type (params_and_fields)));
      List.iter
        (function
          | PDecl (lf, field_name, field_type) ->
             let proj_id = field_proj_id field_name in
             mk_declaration lf proj_id
                            (scope_term []
                                        (mk_pi
                                           (pre_subst (List.map field_proj_subst fields) field_type)
                                           (params @ [PDecl(lf, rec_name, rec_type)])));
             mk_rules [
                 scope_rule (
                     lf,
                     params_and_fields,
                     proj_id,
                     param_patterns @
                       [PPattern (lf,
                                  None,
                                  constr,
                                  param_and_field_patterns)],
                     PreId (lf, field_name))]
          | PDef (lf, field_name, field_val) ->
             failwith "Unimplemented: Defined field in record type"
        )
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
%type <Preterm.pdecl> param
%type <Preterm.pdecl list> context
%type <Basics.loc*Basics.ident*Preterm.prepattern list> top_pattern
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
                | ID COLON term DEF term DOT
                { mk_definition (fst $1) (snd $1) (Some (scope_term [] $3)) (scope_term [] $5) }
                | ID DEF term DOT
                { mk_definition (fst $1) (snd $1)  None (scope_term [] $3) }
                | ID param+ COLON term DEF term DOT
                { mk_definition (fst $1) (snd $1) (Some (scope_term [] (mk_pi $4 $2)))
                        (scope_term [] (mk_lam $6 $2)) }
                | ID param+ DEF term DOT
                { mk_definition (fst $1) (snd $1) None (scope_term [] (mk_lam $4 $2)) }
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
                | RECORD ID DEF ID LEFTBRA context RIGHTBRA DOT
                { mk_record_type $2 [] $4 $6 }
                | RECORD ID param+ DEF ID LEFTBRA context RIGHTBRA DOT
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

param           : LEFTPAR decl RIGHTPAR                 { $2 }

rule            : LEFTSQU context RIGHTSQU top_pattern LONGARROW term
                { let (l,id,args) = $4 in ( l , $2 , id , args , $6) }

decl            : ID COLON term         { PDecl (fst $1,snd $1,$3) }
                | ID DEF term           { PDef (fst $1,snd $1,$3) }

context         : /* empty */          { [] }
                | separated_nonempty_list(COMMA, decl) { $1 }

top_pattern     : ID pattern_wp*        { (fst $1,snd $1,$2) }

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
                | ID COLON sterm+ FATARROW term
                { PreLam (fst $1,snd $1,Some(mk_pre_from_list $3),$5) }

letterm         : term
                { $1 }
                | ID DEF term FATARROW letterm
                { mk_let (snd $1) $3 $5 }

%%
