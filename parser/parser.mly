%parameter <M :
  sig
    type 'a m
    type entry
    val return         : 'a -> 'a m
    val bind           : 'a m -> ('a -> 'b m) -> 'b m
    val mk_prelude     : Basics.loc -> Basics.ident -> entry m
    val mk_declaration : Basics.loc -> Basics.ident -> Term.term -> entry m
    val mk_definition  : Basics.loc -> Basics.ident -> Term.term option -> Term.term -> entry m
    val mk_definable   : Basics.loc -> Basics.ident -> Term.term -> entry m
    val mk_opaque      : Basics.loc -> Basics.ident -> Term.term option -> Term.term -> entry m
    val mk_rules       : Rule.rule list -> entry m
    val mk_command     : Basics.loc -> Cmd.command -> entry m
    val mk_Type        : Basics.loc -> Term.term m
    val mk_DB          : Basics.loc -> Basics.ident -> int -> Term.term m
    val mk_Const       : Basics.loc -> Basics.ident -> Basics.ident -> Term.term m
    val mk_Lam         : Basics.loc -> Basics.ident -> Term.term option -> Term.term -> Term.term m
    val mk_App         : Term.term  -> Term.term -> Term.term list -> Term.term m
    val mk_Pi          : Basics.loc -> Basics.ident -> Term.term -> Term.term -> Term.term m
    val mk_Arrow       : Basics.loc -> Term.term   -> Term.term -> Term.term m
    val mk_ending      : entry m -> unit
  end>
%{
    open Basics
    open Preterm
    open Rule
    open Cmd
    open M

    module S = Scoping.Make(M)

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

    let rec mmap (f:'a -> 'b m) l =
      match l with
      | [] -> return []
      | a::at -> bind (f a) (fun b -> bind (mmap f at) (fun bt -> return (b::bt)))


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
%token <Basics.loc> KW_DEF
%token <Basics.loc*Basics.ident> ID
%token <Basics.loc*Basics.ident*Basics.ident> QID
%token <string> STRING

%start prelude
%start line
(* %start lines
%type <unit> lines *)
%type <M.entry M.m> prelude
%type <M.entry M.m> line
%type <Preterm.prule> rule
%type <Preterm.pdecl> decl
%type <Basics.loc*Basics.ident*Preterm.preterm> param
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

(*
lines           :  line+ EOF
		  { mk_ending (mmap (fun x -> x) $1) }
                | EOF
		  {raise Tokens.EndOfFile }
*)
line            : ID COLON term DOT
                {bind (S.scope_term [] $3) (fun ty -> mk_declaration (fst $1) (snd $1) ty) }
                | ID param+ COLON term DOT
                { bind (S.scope_term [] (mk_pi $4 $2)) (fun ty -> mk_declaration (fst $1) (snd $1) ty) }
                | KW_DEF ID COLON term DOT
                { bind (S.scope_term [] $4) (fun ty -> mk_definable (fst $2) (snd $2) ty) }
                | KW_DEF ID COLON term DEF term DOT
                { bind (S.scope_term [] $4) (fun ty ->
		  bind (S.scope_term [] $6) (fun te ->
		    mk_definition (fst $2) (snd $2) (Some ty) te)) }
                | KW_DEF ID DEF term DOT
                { bind (S.scope_term [] $4) (fun te -> mk_definition (fst $2) (snd $2)  None te) }
                | KW_DEF ID param+ COLON term DEF term DOT
                { bind (S.scope_term [] (mk_pi $5 $3)) (fun ty ->
		  bind (S.scope_term [] (mk_pi $7 $3)) (fun te ->
		    mk_definition (fst $2) (snd $2) (Some ty) te)) }
                | KW_DEF ID param+ DEF term DOT
                { bind (S.scope_term [] (mk_lam $5 $3)) (fun te ->
		  mk_definition (fst $2) (snd $2) None te) }
                | LEFTBRA ID RIGHTBRA COLON term DEF term DOT
                { bind (S.scope_term [] $5) (fun ty -> bind (S.scope_term [] $7) (fun te ->
		  mk_opaque (fst $2) (snd $2) (Some ty) te)) }
                | LEFTBRA ID RIGHTBRA DEF term DOT
                { bind (S.scope_term [] $5) (fun te -> mk_opaque (fst $2) (snd $2)  None te) }
                | LEFTBRA ID param+ RIGHTBRA COLON term DEF term DOT
                { bind (S.scope_term [] (mk_pi $6 $3)) (fun ty ->
		  bind (S.scope_term [] (mk_lam $8 $3)) (fun te ->
		  mk_opaque (fst $2) (snd $2) (Some ty) te)) }
                | LEFTBRA ID param+ RIGHTBRA DEF term DOT
                { bind (S.scope_term [] (mk_lam $6 $3)) (fun te ->
		  mk_opaque (fst $2) (snd $2)  None te) }
                | rule+ DOT
                { bind (mmap S.scope_rule $1) mk_rules }
                | command DOT { $1 }
                | EOF
                { (* mk_ending () ; *) raise Tokens.EndOfFile }



command         : WHNF  term    { bind (S.scope_term [] $2) (fun te -> mk_command $1 (Whnf te)) }
                | HNF   term    { bind (S.scope_term [] $2) (fun te -> mk_command $1 (Hnf te)) }
                | SNF   term    { bind (S.scope_term [] $2) (fun te -> mk_command $1 (Snf te)) }
                | STEP  term    { bind (S.scope_term [] $2) (fun te -> mk_command $1 (OneStep te)) }
                | INFER term    { bind (S.scope_term [] $2) (fun te -> mk_command $1 (Infer te)) }
                | CONV  term  COMMA term {
		  bind (S.scope_term [] $2) (fun ta ->
		    bind (S.scope_term [] $4) (fun tb -> mk_command $1 (Conv (ta,tb)))) }
                | CHECK term  COMMA term {
		  bind (S.scope_term [] $2) (fun ta ->
		    bind (S.scope_term [] $4) (fun tb -> mk_command $1 (Check (ta,tb)))) }
                | PRINT STRING  { mk_command $1 (Print $2) }
                | GDT   ID      { mk_command $1 (Gdt (None,snd $2)) }
                | GDT   QID     { let (_,m,v) = $2 in mk_command $1 (Gdt (Some m,v)) }
                | OTHER term_lst {
		  bind (mmap (S.scope_term []) $2) (fun terms ->
		  mk_command (fst $1) (Other (snd $1,terms)))}


term_lst        : term                                  { [$1] }
                | term COMMA term_lst                   { $1::$3 }

param           : LEFTPAR ID COLON term RIGHTPAR        { (fst $2,snd $2,$4) }

rule            : LEFTSQU context RIGHTSQU top_pattern LONGARROW term
                { let (l,md_opt,id,args) = $4 in ( l , $2 , md_opt, id , args , $6) }

decl            : ID COLON term         { debug "Ignoring type declaration in rule context."; $1 }
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
