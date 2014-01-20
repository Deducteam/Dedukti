%parameter <M :
        sig
                val mk_prelude          : Types.loc -> Types.ident -> unit
                val mk_require          : Types.loc -> Types.ident -> unit
                val mk_declaration      : Types.loc -> Types.ident -> Types.preterm -> unit
                val mk_definition       : Types.loc -> Types.ident -> Types.preterm option -> Types.preterm -> unit
                val mk_opaque           : Types.loc -> Types.ident -> Types.preterm option -> Types.preterm -> unit
                val mk_term             : Types.preterm -> unit
                val mk_rules            : Types.prule list -> unit
                val mk_assert           : Types.loc -> Types.preterm -> Types.preterm -> unit
                val mk_ending           : unit -> unit
        end>
%{
        open Types
        open M
        let mk_dot t = 
          let lc = get_loc t in
            Global.warning lc "Obsolete { _ } construct, ignoring..." ; 
            mk_unknown lc 
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
%token EQUIV
%token <Types.loc> ASSERT
%token <Types.loc> UNDERSCORE
%token <Types.loc*Types.ident>NAME
%token <Types.loc*Types.ident>IMPORT
%token <Types.loc> TYPE
%token <Types.loc*Types.ident> ID
%token <Types.loc*Types.ident*Types.ident> QID

%start prelude
%start line
%type <unit> prelude
%type <unit> line
%type <Types.pdecl list> param_lst
%type <Types.prule list> rule_lst
%type <Types.prule> rule
%type <Types.pdecl> decl
%type <Types.pcontext> context
%type <Types.prepattern list> pat_lst
%type <Types.ptop> top_pattern
%type <Types.prepattern> pattern
%type <Types.preterm> sterm
%type <Types.preterm list> app
%type <Types.preterm> term

%right ARROW FATARROW

%%

prelude         : NAME /* DOT TODO */                           { let (lc,name)=$1 in 
                                                                        Global.set_name name;
                                                                        Env.init name;
                                                                        mk_prelude lc name }

line            : IMPORT /* DOT TODO */                         { mk_require (fst $1) (snd $1) }
                | ID COLON term DOT                             { mk_declaration (fst $1) (snd $1) $3 }
                | ID COLON term DEF term DOT                    { mk_definition (fst $1) (snd $1) (Some $3) $5 }
                | ID DEF term DOT                               { mk_definition (fst $1) (snd $1)  None     $3 }
                | ID param_lst COLON term DEF term DOT          { assert false (*TODO*) }
                | ID param_lst DEF term DOT                     { assert false (*TODO*) }
                | LEFTBRA ID RIGHTBRA COLON term DEF term DOT   { mk_opaque (fst $2) (snd $2) (Some $5) $7 }
                | LEFTBRA ID RIGHTBRA DEF term DOT              { mk_opaque (fst $2) (snd $2)  None     $5 }
                | LEFTBRA ID RIGHTBRA param_lst COLON term DEF term DOT { assert false (*TODO*) }
                | LEFTBRA ID RIGHTBRA param_lst DEF term DOT            { assert false (*TODO*) }
                | rule_lst DOT                                  { mk_rules $1 }
                | DEF term DOT                                  { mk_term $2 }
                | ASSERT term EQUIV term DOT                    { mk_assert $1 $2 $4 (*FIXME*) }
                | EOF                                           { mk_ending () ; raise EndOfFile }

param_lst       : LEFTPAR decl RIGHTPAR                         { [$2] }
                | LEFTPAR decl RIGHTPAR param_lst               { $2::$4 }

rule_lst        : rule                                          { [$1] }
                | rule rule_lst                                 { $1::$2 }

rule            : LEFTSQU context RIGHTSQU top_pattern LONGARROW term { ( $2 , $4 , $6) }

decl            : ID COLON term                                 { (fst $1,snd $1,$3) }

context         : /* empty */                                   { [] }
                | decl COMMA context                            { $1::$3 }
                | decl                                          { [$1] }

top_pattern     : ID pat_lst                                    { ( fst $1 , snd $1 , $2 ) }

pat_lst         : /* empty */                                   { [] }
                | pattern pat_lst                               { $1::$2 }

                pattern         : ID                            { PPattern (fst $1,None,snd $1,[]) }
                | QID                                           { let (l,md,id)=$1 in PPattern (l,Some md,id,[]) }
                | UNDERSCORE                                    { mk_unknown $1 }
                | LEFTPAR ID  pat_lst RIGHTPAR                  { PPattern (fst $2,None,snd $2,$3) }
                | LEFTPAR QID pat_lst RIGHTPAR                  { let (l,md,id)=$2 in PPattern (l,Some md,id,$3) }
                | LEFTBRA term RIGHTBRA                         { mk_dot $2 }

sterm           : QID                                           { let (l,md,id)=$1 in mk_pre_qid l md id }
                | ID                                            { mk_pre_id (fst $1) (snd $1) }
                | LEFTPAR term RIGHTPAR                         { $2 }
                | TYPE                                          { mk_pre_type $1 }

app             : sterm                                         { [$1] }
                | sterm app                                     { $1::$2 }

term            : app                                           { mk_pre_app $1 }
                | ID COLON app ARROW term                       { mk_pre_pi (fst $1) (snd $1) (mk_pre_app $3) $5 }
                | term ARROW term                               { mk_pre_arrow $1 $3 }
                | ID FATARROW term                              { failwith "Not implemented (untyped lambda)." }
                | ID COLON app FATARROW term                    { mk_pre_lam (fst $1) (snd $1) (mk_pre_app $3) $5}

%%

