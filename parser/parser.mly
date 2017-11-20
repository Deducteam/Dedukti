%parameter <M :
  sig
    val mk_prelude     : Basic.loc -> Basic.mident -> unit
    val mk_declaration : Basic.loc -> Basic.ident -> Signature.staticity -> Term.term -> unit
    val mk_definition  : Basic.loc -> Basic.ident -> Term.term option -> Term.term -> unit
    val mk_opaque      : Basic.loc -> Basic.ident -> Term.term option -> Term.term -> unit
    val mk_rules       : Rule.untyped_rule list -> unit
    val mk_command     : Basic.loc -> Cmd.command -> unit
    val mk_ending      : unit -> unit
  end>
%{
    open Basic
    open Preterm
    open Scoping
    open Rule
    open Cmd
    open M

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
%token <Basic.loc> WHNF
%token <Basic.loc> HNF
%token <Basic.loc> SNF
%token <Basic.loc> STEP
%token <Basic.loc> NSTEPS
%token <Basic.loc> INFER
%token <Basic.loc> INFERSNF
%token <Basic.loc> CONV
%token <Basic.loc> CHECK
%token <Basic.loc> PRINT
%token <Basic.loc*Basic.mident> REQUIRE
%token <Basic.loc> GDT
%token <Basic.loc*string> OTHER
%token <Basic.loc> UNDERSCORE
%token <Basic.loc*Basic.mident>NAME
%token <Basic.loc> TYPE
%token <Basic.loc> KW_DEF
%token <Basic.loc> KW_DEFAC
%token <Basic.loc> KW_DEFACU
%token <Basic.loc> KW_THM
%token <Basic.loc*Basic.ident> ID
%token <Basic.loc*Basic.mident*Basic.ident> QID
%token <string> STRING
%token <int>    INT

%start prelude
%start line
%type <unit> prelude
%type <unit> line
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

prelude         : NAME DOT      { let (lc,name) = $1 in
                                        Scoping.name := name;
                                        mk_prelude lc name }

line            : ID COLON term DOT
                { mk_declaration (fst $1) (snd $1) Signature.Static (scope_term [] $3) }
                | ID param+ COLON term DOT
                { mk_declaration (fst $1) (snd $1) Signature.Static (scope_term [] (mk_pi $4 $2)) }
                | KW_DEF ID COLON term DOT
                { mk_declaration (fst $2) (snd $2)
				  				 (Signature.Definable Term.Free) (scope_term [] $4) }
                | KW_DEFAC ID LEFTSQU term RIGHTSQU DOT
                { mk_declaration (fst $2) (snd $2)
				  				 (Signature.Definable Term.AC) (scope_term [] $4) }
                | KW_DEFACU ID LEFTSQU term COMMA term RIGHTSQU DOT
                { mk_declaration (fst $2) (snd $2)
                                 (Signature.Definable (Term.ACU (scope_term [] $6)))
                                 (scope_term [] $4) }
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
                { mk_ending () ; raise Tokens.EndOfFile }


command         : WHNF     term { mk_command $1 (Whnf     (scope_term [] $2)) }
                | HNF      term { mk_command $1 (Hnf      (scope_term [] $2)) }
                | SNF      term { mk_command $1 (Snf      (scope_term [] $2)) }
                | STEP     term { mk_command $1 (OneStep  (scope_term [] $2)) }
                | NSTEPS INT term { mk_command $1 (NSteps ($2,(scope_term [] $3))) }
                | INFER    term { mk_command $1 (Infer    (scope_term [] $2)) }
                | INFERSNF term { mk_command $1 (InferSnf (scope_term [] $2)) }
                | CONV  term  COMMA term
				{ mk_command $1 (Conv  (scope_term [] $2,scope_term [] $4)) }
                | CHECK term  COMMA term
				{ mk_command $1 (Check (scope_term [] $2,scope_term [] $4)) }
                | PRINT STRING  { mk_command $1 (Print $2) }
                | GDT   ID      { mk_command $1 (Gdt (None,snd $2)) }
                | GDT   QID     { let (_,m,v) = $2 in mk_command $1 (Gdt (Some m,v)) }
                | REQUIRE { let (l,m) = $1 in mk_command l (Require(m) ) }
                | OTHER term_lst { mk_command (fst $1) (Other (snd $1,List.map (scope_term []) $2)) }


term_lst        : term                                  { [$1] }
                | term COMMA term_lst                   { $1::$3 }

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
                | term ARROW term
                { PrePi (preterm_loc $1,None,$1,$3) }
                | ID FATARROW term
                { PreLam (fst $1,snd $1,None,$3) }
                | ID COLON sterm+ FATARROW term
                { PreLam (fst $1,snd $1,Some(mk_pre_from_list $3),$5) }
%%
