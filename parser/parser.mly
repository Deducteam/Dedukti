%parameter <M :
  sig
    val mk_prelude     : Basics.loc -> Basics.ident -> unit
    val mk_declaration : Basics.loc -> Basics.ident -> Term.preterm -> unit
    val mk_definition  : Basics.loc -> Basics.ident -> Term.preterm option -> Term.preterm -> unit
    val mk_opaque      : Basics.loc -> Basics.ident -> Term.preterm option -> Term.preterm -> unit
    val mk_rules       : Rule.prule list -> unit
    val mk_command     : Basics.loc -> Cmd.command -> unit
    val mk_ending      : unit -> unit
  end>
%{
    open Basics
    open Term
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
        | PreType l | PreId (l,_) | PreQId (l,_,_) | PreLam  (l,_,_,_)
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
%token <string> STRING

%start prelude
%start line
%type <unit> prelude
%type <unit> line
%type <Rule.prule> rule
%type <Rule.pdecl> decl
%type <Rule.pdecl> param
%type <Rule.pdecl list> context
%type <Basics.loc*Basics.ident*Rule.prepattern list> top_pattern
%type <Rule.prepattern> pattern
%type <Rule.prepattern> pattern_wp
%type <Term.preterm> sterm
%type <Term.preterm> term

%right ARROW FATARROW

%%

prelude         : NAME DOT      { mk_prelude (fst $1) (snd $1) }

line            : ID COLON term DOT
                { mk_declaration (fst $1) (snd $1) $3 }
                | ID COLON term DEF term DOT
                { mk_definition (fst $1) (snd $1) (Some $3) $5 }
                | ID DEF term DOT
                { mk_definition (fst $1) (snd $1)  None     $3 }
                | ID param+ COLON term DEF term DOT
                { mk_definition (fst $1) (snd $1) (Some (mk_pi $4 $2)) (mk_lam $6 $2) }
                | ID param+ DEF term DOT
                { mk_definition (fst $1) (snd $1) None (mk_lam $4 $2) }
                | LEFTBRA ID RIGHTBRA COLON term DEF term DOT
                { mk_opaque (fst $2) (snd $2) (Some $5) $7 }
                | LEFTBRA ID RIGHTBRA DEF term DOT
                { mk_opaque (fst $2) (snd $2)  None     $5 }
                | LEFTBRA ID param+ RIGHTBRA COLON term DEF term DOT
                { mk_opaque (fst $2) (snd $2) (Some (mk_pi $6 $3)) (mk_lam $8 $3) }
                | LEFTBRA ID param+ RIGHTBRA DEF term DOT
                { mk_opaque (fst $2) (snd $2)  None (mk_lam $6 $3) }
                | rule+ DOT
                { mk_rules $1 }
                | command DOT { $1 }
                | EOF
                { mk_ending () ; raise Tokens.EndOfFile }


command         : WHNF  term    { mk_command $1 (Whnf $2) }
                | HNF   term    { mk_command $1 (Hnf $2) }
                | SNF   term    { mk_command $1 (Snf $2) }
                | STEP  term    { mk_command $1 (OneStep $2) }
                | INFER term    { mk_command $1 (Infer $2) }
                | CONV  term  COMMA term { mk_command $1 (Conv ($2,$4)) }
                | CHECK term  COMMA term { mk_command $1 (Check ($2,$4)) }
                | PRINT STRING  { mk_command $1 (Print $2) }
                | GDT   ID      { mk_command $1 (Gdt (None,snd $2)) }
                | GDT   QID     { let (_,m,v) = $2 in mk_command $1 (Gdt (Some m,v)) }
                | OTHER term_lst { mk_command (fst $1) (Other (snd $1,$2)) }


term_lst        : term                                  { [$1] }
                | term COMMA term_lst                   { $1::$3 }

param           : LEFTPAR decl RIGHTPAR                 { $2 }

rule            : LEFTSQU context RIGHTSQU top_pattern LONGARROW term
                { let (l,id,args) = $4 in ( l , $2 , id , args , $6) }

decl           : ID COLON term         { (fst $1,snd $1,$3) }

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
