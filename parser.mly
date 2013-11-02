%parameter <M : 
        sig 
                val mk_prelude          : Types.loc -> Types.ident -> unit
                val mk_require          : Types.loc -> Types.ident -> unit
                val mk_declaration      : Types.loc -> Types.ident -> Types.term -> unit
                val mk_definition       : Types.loc -> Types.ident -> Types.term option -> Types.term -> unit
                val mk_opaque           : Types.loc -> Types.ident -> Types.term option -> Types.term -> unit
                val mk_term             : Types.term -> unit
                val mk_rules            : Types.rule list -> unit
                val mk_ending           : unit -> unit
        end>  
%{
        open Types
        open M
        let of_pterm = Pterm.of_pterm [] 
        (*TODO remettre dot ?*)
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
%token <Types.loc> TYPE
%token <Types.loc*Types.ident> ID
%token <Types.loc*Types.ident*Types.ident> QID

%start prelude
%start line
%type <unit> prelude
%type <unit> line
%type <Types.rule list> rule_lst
%type <Types.rule> rule
%type <Types.loc*Types.ident*Types.pterm> decl
%type <Types.context> context
%type <Types.ptop> top_pattern
%type <Types.ppattern list> pat_lst
%type <Types.ppattern> pattern
%type <Types.pterm> sterm
%type <Types.pterm list> app
%type <Types.pterm> term

%right ARROW FATARROW

%%

prelude         : NAME ID /* DOT TODO */                        { mk_prelude (fst $2) (snd $2) }

line            : IMPORT ID /* DOT TODO */                      { mk_require (fst $2) (snd $2) }
                | ID COLON term DOT                             { mk_declaration (fst $1) (snd $1) (of_pterm $3) }
                | ID COLON term DEF term DOT                    { mk_definition (fst $1) (snd $1) (Some (of_pterm $3)) (of_pterm $5) }
                | ID DEF term DOT                               { mk_definition (fst $1) (snd $1) None (of_pterm $3) }
                | LEFTBRA ID RIGHTBRA COLON term DEF term DOT   { mk_opaque (fst $2) (snd $2) (Some (of_pterm $5)) (of_pterm $7) }
                | LEFTBRA ID RIGHTBRA DEF term DOT              { mk_opaque (fst $2) (snd $2) None (of_pterm $5) }
                | rule_lst DOT                                  { mk_rules $1 } 
                | DEF term DOT                                  { mk_term (of_pterm $2) } 
                | EOF                                           { mk_ending () ; raise EndOfFile }

rule_lst        : rule                                          { [$1] }
                | rule rule_lst                                 { $1::$2 }

rule            : LEFTSQU context RIGHTSQU top_pattern LONGARROW term   { ($2,Pterm.of_ptop $2 $4,Pterm.of_pterm $2 $6) } 

decl            : ID COLON term                                 { (fst $1,snd $1,$3) }

context         : /* empty */                                   { [] }
                | decl COMMA context                            { let (l,id,pt)=$1 in (l,id,Pterm.of_pterm $3 pt)::$3 }
                | decl                                          { let (l,id,pt)=$1 in [(l,id,of_pterm pt)] }

top_pattern     : ID pat_lst                                    { ( (fst $1,snd $1) , Array.of_list $2 ) }

pat_lst         : /* empty */                                   { [] }
                | pattern pat_lst                               { $1::$2 }

pattern         : ID                                            { PPat ((fst $1,!Global.name,snd $1),[||]) }
                | QID                                           { PPat ($1,[||]) }
                | UNDERSCORE                                    { PDash $1 }
                | LEFTPAR ID  pat_lst RIGHTPAR                  { PPat ((fst $2,!Global.name,snd $2),Array.of_list $3) }           
                | LEFTPAR QID pat_lst RIGHTPAR                  { PPat ($2,Array.of_list $3) }           

sterm           : QID                                           { let (a,b,c)=$1 in Types.PQid (a,b,c) }
                | ID                                            { PId (fst $1,snd $1) }
                | LEFTPAR term RIGHTPAR                         { $2 }
                | TYPE                                          { PType $1 }

app             : sterm                                         { [$1] }
                | sterm app                                     { $1::$2 }

term            : app                                           { mk_papp $1 }
                | ID COLON app ARROW term                       { PPi  (Some $1, mk_papp $3, $5) }
                | term ARROW term                               { PPi  (None   , $1, $3) }
                | ID FATARROW term                              { PLam ($1, None , $3) }
                | ID COLON app FATARROW term                    { PLam ($1, Some (mk_papp $3) , $5 ) }

%%

