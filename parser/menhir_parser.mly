%{
    open Basic
    open Preterm
    open Scoping
    open Rule
    open Internals
    open Reduction

    exception InvalidConfig

    let rec mk_lam (te:preterm) : (loc*ident*preterm) list -> preterm = function
        | [] -> te
        | (l,x,ty)::tl -> PreLam (l,x,Some ty,mk_lam te tl)

    let rec mk_pi (te:preterm) : (loc*ident*preterm) list -> preterm = function
        | [] -> te
        | (l,x,ty)::tl -> PrePi(l,Some x,ty,mk_pi te tl)

    let rec preterm_loc = function
        | PreType l | PreId (l,_) | PreQId (l,_) | PreLam  (l,_,_,_)
        | PrePi   (l,_,_,_) -> l
        | PreApp (f,_,_) -> preterm_loc f

    let mk_pre_from_list = function
        | [] -> assert false
        | [t] -> t
        | f::a1::args -> PreApp (f,a1,args)

    let mk_declaration lc id st t = Decl(lc,id,st,t)

    let mk_definition lc id pty te = Def(lc,id,false,pty,te)

    let mk_opaque lc id pty te = Def(lc,id,true,pty,te)

    let mk_rules rs = Rules(rs)

    let mk_eval lc config te = Eval(lc,config,te)

    let mk_check lc is_assert fail te = Check(lc,is_assert,fail,te)

    let mk_infer lc config te = Eval(lc,config,te)

    let mk_print lc s = Print(lc,s)

    let mk_dtree lc md id = DTree(lc,md,id)

    let mk_config id1 id2_opt =
      try
	let open Env in
        match (id1, id2_opt) with
        | "SNF" , None   ->
          {default_cfg with nb_steps = None; strategy=Reduction.Snf  }
        | "HNF" , None   ->
          {default_cfg with nb_steps = None; strategy=Reduction.Hnf  }
        | "WHNF", None   ->
          {default_cfg with nb_steps = None; strategy=Reduction.Whnf }
        | "SNF" , Some i ->
          {default_cfg with nb_steps = Some (int_of_string i); strategy=Reduction.Snf  }
        | "HNF" , Some i ->
          {default_cfg with nb_steps = Some (int_of_string i); strategy=Reduction.Hnf  }
        | "WHNF", Some i ->
          {default_cfg with nb_steps = Some (int_of_string i); strategy=Reduction.Whnf }
        | i, Some "SNF"  ->
          {default_cfg with nb_steps = Some (int_of_string i); strategy=Reduction.Snf  }
        | i, Some "HNF"  ->
          {default_cfg with nb_steps = Some (int_of_string i); strategy=Reduction.Hnf  }
        | i, Some "WHNF" ->
          {default_cfg with nb_steps = Some (int_of_string i); strategy=Reduction.Whnf }
        | i, None        ->
          {default_cfg with nb_steps = Some (int_of_string i); strategy=Reduction.Snf  }
	    | _ -> raise InvalidConfig
      with _ -> raise InvalidConfig

      let snf_cfg = mk_config "SNF" None
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
%token <Basic.loc> EVAL
%token <Basic.loc> INFER
%token <Basic.loc> CONV
%token <Basic.loc> CHECK
%token <Basic.loc> PRINT
%token <Basic.loc> GDT
%token <Basic.loc> UNDERSCORE
%token <Basic.loc*Basic.mident>NAME
%token <Basic.loc> TYPE
%token <Basic.loc> KW_DEF
%token <Basic.loc> KW_THM
%token <Basic.loc*Basic.ident> ID
%token <Basic.loc*Basic.mident*Basic.ident> QID
%token <string> STRING

%start prelude
%start line
%type <Basic.mident option> prelude
%type <Internals.entry> line
%type <Preterm.prule> rule
%type <Preterm.pdecl> decl
%type <Basic.loc*Basic.ident*Preterm.preterm> param
%type <Preterm.pdecl list> context
%type <Basic.loc*Basic.mident option*Basic.ident*Preterm.prepattern list> top_pattern
%type <Preterm.prepattern> pattern
%type <Preterm.prepattern> pattern_wp
%type <Preterm.preterm> sterm
%type <Preterm.preterm> term

%right ARROW FATARROW

%%

prelude         : NAME DOT     { let (lc,name) = $1 in Some name }

line            : ID COLON term DOT
                { mk_declaration (fst $1) (snd $1) Signature.Static (scope_term [] $3) }
                | ID param+ COLON term DOT
                { mk_declaration (fst $1) (snd $1) Signature.Static (scope_term [] (mk_pi $4 $2)) }
                | KW_DEF ID COLON term DOT
                { mk_declaration (fst $2) (snd $2) Signature.Definable (scope_term [] $4) }
                | KW_DEF ID COLON term DEF term DOT
                { mk_definition (fst $2) (snd $2) (Some (scope_term [] $4)) (scope_term [] $6) }
                | KW_DEF ID DEF term DOT
                { mk_definition (fst $2) (snd $2)  None (scope_term [] $4) }
                | KW_DEF ID param+ COLON term DEF term DOT
                { mk_definition (fst $2) (snd $2) (Some (scope_term [] (mk_pi $5 $3)))
                        (scope_term [] (mk_lam $7 $3)) }
                | KW_DEF ID param+ DEF term DOT
                { mk_definition (fst $2) (snd $2) None (scope_term [] (mk_lam $5 $3)) }
                | KW_THM ID COLON term DEF term DOT
                { mk_opaque (fst $2) (snd $2) (Some (scope_term [] $4)) (scope_term [] $6) }
                | KW_THM ID param+ COLON term DEF term DOT
                { mk_opaque (fst $2) (snd $2) (Some (scope_term [] (mk_pi $5 $3)))
                        (scope_term [] (mk_lam $7 $3)) }
                | rule+ DOT
                { mk_rules (List.map scope_rule $1) }
                | command DOT { $1 }
                | EOF
                { raise End_of_file }


command         : EVAL term
                { mk_eval $1 snf_cfg (scope_term [] $2) }
                | EVAL eval_config term
                { mk_eval $1 $2 (scope_term [] $3) }
                | INFER term
                { mk_infer $1 Reduction.default_cfg (scope_term [] $2) }
                | INFER eval_config term
                { mk_infer $1 $2 (scope_term [] $3) }
                | CONV  term  COMMA term
		{ mk_check $1 false false (Convert(scope_term [] $2,scope_term [] $4)) }
                | CHECK term  COMMA term
		{ mk_check $1 false false (HasType(scope_term [] $2,scope_term [] $4)) }

                | PRINT STRING  { mk_print $1 $2 }
                | GDT   ID      { mk_dtree $1 None (snd $2) }
                | GDT   QID     { let (_,m,v) = $2 in mk_dtree $1 (Some m) v }

eval_config     : LEFTSQU ID RIGHTSQU
                { mk_config (string_of_ident (snd $2)) None            }
                | LEFTSQU ID COMMA ID RIGHTSQU
                { mk_config (string_of_ident (snd $2)) (Some (string_of_ident (snd $4))) }

param           : LEFTPAR ID COLON term RIGHTPAR        { (fst $2,snd $2,$4) }

rule            : LEFTSQU context RIGHTSQU top_pattern LONGARROW term
                { let (l,md_opt,id,args) = $4 in ( l , None, $2 , md_opt, id , args , $6) }
                | LEFTBRA ID RIGHTBRA LEFTSQU context RIGHTSQU top_pattern LONGARROW term
                { let (l,md_opt,id,args) = $7 in ( l , Some (None,snd $2), $5 , md_opt, id , args , $9)}
                | LEFTBRA QID RIGHTBRA LEFTSQU context RIGHTSQU top_pattern LONGARROW term
                { let (l,md_opt,id,args) = $7 in let (_,m,v) = $2 in ( l , Some (Some m,v), $5 , md_opt, id , args , $9)}


decl            : ID COLON term         { debug 1 "Ignoring type declaration in rule context."; $1 }
                | ID                    { $1 }

context         : /* empty */          { [] }
                | separated_nonempty_list(COMMA, decl) { $1 }

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
                { let (l,md,id)=$1 in PreQId(l,mk_name md id) }
                | ID
                { PreId (fst $1,snd $1) }
                | LEFTPAR term RIGHTPAR
                { $2 }
                | TYPE
                { PreType $1 }

term            : sterm+
                { mk_pre_from_list $1 }
                | ID COLON sterm+ ARROW term
                { PrePi (fst $1,Some (snd $1),mk_pre_from_list $3,$5) }
                | LEFTPAR ID COLON sterm+ RIGHTPAR ARROW term
                { PrePi (fst $2,Some (snd $2),mk_pre_from_list $4,$7) }
                | term ARROW term
                { PrePi (preterm_loc $1,None,$1,$3) }
                | ID FATARROW term
                { PreLam (fst $1,snd $1,None,$3) }
                | ID COLON sterm+ FATARROW term
                { PreLam (fst $1,snd $1,Some(mk_pre_from_list $3),$5) }
%%
