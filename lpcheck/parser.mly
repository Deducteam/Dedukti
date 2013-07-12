%{
        open Types

let mk_prelude (l,v : loc*string) : unit =
  Global.msg ( Debug.string_of_loc l ^ "[Name] " ^ v ^ ".\n") ;
  Global.name := v ;
  Env.init v

let mk_require (l,v : loc*string) : unit = 
  Global.msg ( Debug.string_of_loc l ^ "[Import] " ^ v ^ ".\n") ;
  Env.import v

let mk_declaration ((l,id),pty : (loc*string)*pterm) : unit = 
  let ty = Term.of_pterm pty in
    Global.msg ( Debug.string_of_loc l ^ "[Declaration] " ^ id ^ ".\n" ) ;
    Checker.check_type ty ;
    Env.add_decl id ty

let mk_definition ((l,id),pty,pte : (loc*string)*pterm*pterm) : unit = 
  let ty = Term.of_pterm pty in
  let te = Term.of_pterm pte in
    Global.msg ( Debug.string_of_loc l ^ "[Definition] " ^ id ^ ".\n") ;
    Checker.check_type ty ;
    Checker.check_term te ty ;
    Env.add_def id te ty 

let mk_opaque ((l,id),pty,pte : (loc*string)*pterm*pterm) : unit = 
  let ty = Term.of_pterm pty in
  let te = Term.of_pterm pte in
    Global.msg ( Debug.string_of_loc l ^ "[Opaque] " ^ id ^ ".\n") ;
    Checker.check_type ty ;
    Checker.check_term te ty ;
    Env.add_decl id ty 

let mk_typecheck (l,pty,pte : loc*pterm*pterm) :unit = 
  let ty = Term.of_pterm pty in
  let te = Term.of_pterm pte in
    Global.msg ( Debug.string_of_loc l ^ "[TypeCheck] _ \n") ;
    Checker.check_type ty ;
    Checker.check_term te ty

let mk_rules (lst:rule list) : unit = assert false (*TODO*) 

let mk_ending _ : unit = () 

%}

%token EOF
%token DOT
%token COMMA
%token COLON
%token ARROW
%token FATARROW
%token LONGARROW
%token DEF
%token <Types.loc> UNDERSCORE
%token NAME
%token IMPORT
%token LEFTPAR
%token RIGHTPAR
%token LEFTBRA
%token RIGHTBRA
%token LEFTSQU
%token RIGHTSQU
%token TYPE
%token <Types.loc*string> ID
%token <Types.loc*string*string> QID

%start top
%type <unit> top
%type <unit> prelude
%type <unit> line_lst
%type <unit> line
%type <Types.rule list> rule_lst
%type <Types.rule> rule
%type <(Types.loc*string)*Types.pterm> decl
%type <((Types.loc*string)*Types.pterm) list> context
%type <Types.top_pattern> top_pattern
%type <Types.pterm list> dot_lst
%type <Types.pattern list> pat_lst
%type <Types.pattern> pattern
%type <Types.pterm> sterm
%type <Types.pterm> app
%type <Types.pterm> term

%right ARROW FATARROW

%%
top:            prelude line_lst EOF                            { mk_ending () }

prelude:        NAME ID                                         { mk_prelude $2 }

line_lst:       /* empty */                                     { () }
                | line line_lst                                 { () }

line:             ID COLON term DOT                             { mk_declaration ($1,$3) }
                | ID COLON term DEF term DOT                    { mk_definition ($1,$3,$5) }
                | LEFTBRA ID RIGHTBRA COLON term DEF term DOT   { mk_opaque ($2,$5,$7) }
                | UNDERSCORE COLON term DEF term DOT            { mk_typecheck ($1,$3,$5) }
                | rule_lst DOT                                  { mk_rules $1 } 
                | IMPORT ID                                     { mk_require $2 }

rule_lst:         rule                                          { [$1] }
                | rule rule_lst                                 { $1::$2 }

rule:            LEFTSQU context RIGHTSQU top_pattern LONGARROW term    { ($2,$4,$6) } 

decl:           ID COLON term                                   { ($1,$3) }

context:        /* empty */                                     { [] }
                | decl COMMA context                            { $1::$3 }
                | decl                                          { [$1] }

top_pattern:      ID dot_lst pat_lst                            { ( (fst $1,snd $1) , Array.of_list $2 , Array.of_list $3) }
         /*       | QID dot_lst pat_lst                         { ( $1 , Array.of_list $2 , Array.of_list $3 ) } */

dot_lst:         /* empty */                                    { [] }
                | LEFTBRA term RIGHTBRA dot_lst                 { $2::$4 }

pat_lst:         /* empty */                                    { [] }
                | pattern pat_lst                               { $1::$2 }

                pattern:          ID                            { Pat ((fst $1,!Global.name,snd $1),[||],[||]) }
                | QID                                           { Pat ($1,[||],[||]) }
                | LEFTPAR ID  dot_lst pat_lst RIGHTPAR          { Pat ((fst $2,!Global.name,snd $2),Array.of_list $3,Array.of_list $4) }           
                | LEFTPAR QID dot_lst pat_lst RIGHTPAR          { Pat ($2,Array.of_list $3,Array.of_list $4) }           

sterm           : QID                                           { let (a,b,c)=$1 in Types.PQid (a,b,c) }
                | ID                                            { PId (fst $1,snd $1) }
                | LEFTPAR term RIGHTPAR                         { $2 }
                | TYPE                                          { PType }

app:            sterm                                           { $1 }
                | app sterm                                     { PApp ($1,$2) }

term:           app                                             { $1 }
                | ID COLON app ARROW term                       { PPi  (Some $1, $3, $5) }
                | term ARROW term                               { PPi  (None   , $1, $3) }
                | ID FATARROW term                              { PLam ($1, None , $3) }
                | ID COLON app FATARROW term                    { PLam ($1, Some $3 , $5 ) }

%%

