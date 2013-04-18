%{

open Types

let mk_prelude (name:var) : unit = 
        Global.set_name name ;
        match !Global.action with
          | PrintDedukti        -> assert false (*TODO*)
          | PrintMMT            -> Mmt.mk_prelude name
          | LuaGeneration       -> LuaGeneration.mk_prelude name 

let mk_ending  _ : unit = 
        match !Global.action with
          | PrintDedukti        -> assert false (*TODO*)
          | PrintMMT            -> Mmt.mk_ending ()
          | LuaGeneration       -> ( LuaGeneration.mk_ending () ; Scope.clear () )


let mk_declaration ( v,ty : var*pterm ) : unit = 
   match !Global.action with
          | PrintDedukti        -> assert false (*TODO*)
          | PrintMMT            -> Mmt.mk_declaration (v,ty)
          | LuaGeneration       -> LuaGeneration.mk_declaration (v,ty)


let mk_definition (def:definition) : unit =
   match !Global.action with
          | PrintDedukti        -> assert false (*TODO*)
          | PrintMMT            -> Mmt.mk_definition def
          | LuaGeneration       -> LuaGeneration.mk_definition def 

let mk_rules (rs:prule list) : unit = 
   match !Global.action with
          | PrintDedukti        -> assert false (*TODO*)
          | PrintMMT            -> Mmt.mk_rules rs
          | LuaGeneration       -> LuaGeneration.mk_rules rs 

let mk_require ( l,dep : loc*string ) : unit = 
   match !Global.action with
          | PrintDedukti        -> assert false (*TODO*)
          | PrintMMT            -> Mmt.mk_require dep
          | LuaGeneration       -> LuaGeneration.mk_require l dep

%}

%token EOF
%token AT
%token DOT
%token COMMA
%token COLON
%token ARROW
%token FATARROW
%token LONGARROW
%token DEF
%token <Types.loc> UNDERSCORE
%token HASH
%token LEFTPAR
%token RIGHTPAR
%token LEFTBRA
%token RIGHTBRA
%token LEFTSQU
%token RIGHTSQU
%token TYPE
%token <Types.var> ID
%token <Types.id> QID

%start top
%type <unit> top
%type <unit> prelude
%type <unit> line_lst
%type <unit> line
%type <Types.prule list> rule_lst
%type <Types.prule> rule
%type <Types.var*Types.pterm> decl
%type <(Types.var*Types.pterm) list> context
%type <Types.top_ppat> top_pattern
%type <Types.pterm list> dot_lst
%type <Types.ppat list> pat_lst
%type <Types.ppat> pattern
%type <Types.pterm> sterm
%type <Types.pterm> app
%type <Types.pterm> term

%right ARROW FATARROW

%%
top:            prelude line_lst EOF                            { mk_ending () ; raise End_of_file }

prelude:        AT ID                                           { mk_prelude $2 }

line_lst:       /* empty */                                     { () }
                | line line_lst                                 { () }

line:             ID COLON term DOT                             { mk_declaration ($1,$3) }
                | ID COLON term DEF term DOT                    { mk_definition (Def ($1,$3,$5)) }
                | LEFTBRA ID RIGHTBRA COLON term DEF term DOT   { mk_definition (Opaque ($2,$5,$7)) }
                | UNDERSCORE COLON term DEF term DOT            { mk_definition (Anon ($1,$3,$5)) }
                | rule_lst DOT                                  { mk_rules $1 } 
                | HASH ID                                       { mk_require $2 }

rule_lst:         rule                                          { [$1] }
                | rule rule_lst                                 { $1::$2 }

rule:            LEFTSQU context RIGHTSQU top_pattern LONGARROW term    { ($2,$4,$6) } 

decl:           ID COLON term                                   { ($1,$3) }

context:        /* empty */                                     { [] }
                | decl COMMA context                            { $1::$3 }
                | decl                                          { [$1] }

top_pattern:      ID dot_lst pat_lst                            { ( (fst $1,snd $1) , Array.of_list $2 , Array.of_list $3) }
         /*       | QID dot_lst pat_lst                           { ( $1 , Array.of_list $2 , Array.of_list $3 ) } */

dot_lst:         /* empty */                                    { [] }
                | LEFTBRA term RIGHTBRA dot_lst                 { $2::$4 }

pat_lst:         /* empty */                                    { [] }
                | pattern pat_lst                               { $1::$2 }

pattern:          ID                                            { Pat_Id $1 }
                | QID                                           { Pat_Pa ($1,[||],[||]) }
                | LEFTPAR ID  dot_lst pat_lst RIGHTPAR          { Pat_Pa ((fst $2,!Global.name,snd $2),Array.of_list $3,Array.of_list $4) }           
                | LEFTPAR QID dot_lst pat_lst RIGHTPAR          { Pat_Pa ($2,Array.of_list $3,Array.of_list $4) }           

sterm           : QID                                           { P_Qid $1 }
                | ID                                            { P_Id $1 }
                | LEFTPAR term RIGHTPAR                         { $2 }
                | TYPE                                          { P_Type }

app:            sterm                                           { $1 }
                | app sterm                                     { P_App ($1,$2) }

term:           app                                             { $1 }
                | ID COLON app ARROW term                       { P_Pi  (Some $1, $3, $5) }
                | term ARROW term                               { P_Pi  (None   , $1, $3) }
                | ID FATARROW term                              { P_Lam ($1, None , $3) }
                | ID COLON app FATARROW term                    { P_Lam ($1, Some $3 , $5 ) }

%%

