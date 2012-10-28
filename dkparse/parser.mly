%{
open Types
open Global
%}

%token DOT
%token COMMA
%token COLON
%token ARROW
%token FATARROW
%token LONGARROW
%token LEFTPAR
%token RIGHTPAR
%token LEFTBRA
%token RIGHTBRA
%token LEFTSQU
%token RIGHTSQU
%token TYPE
%token <Types.id*Types.loc> ID
%token <Types.id> QID

%start top
%type <unit> top
%type <Types.loc*Types.rules> rules
%type <Types.id*Types.loc*Types.rule> rule
%type <Types.id*Types.loc*Types.term array*Types.pattern array> pat

%right ARROW FATARROW

%%
top:            /* empty */                                     { () }
                | top decl DOT                                  { gscope_add (fst $2) ; LuaGeneration.gen_decl  $2 }
                | top rules DOT                                 { chk_rules_id $2     ; LuaGeneration.gen_rules (snd $2) }
                ;

rules:          rule                                            { let (id,loc,ru) = $1 in (loc,(id,[ru])) }
                | rule rules                             
                        { let (id1,l1,ru) = $1 and (l2,(id2,lst)) = $2 in 
                          if id1<>id2 then raise (Error (ConstructorMismatch (id1,l1,id2,l2))) 
                          else (l1,(id1,ru::lst)) }
                ;

decl:           ID COLON term                                   { ($1 ,$3) } 
                ;

rule:            LEFTSQU bdgs RIGHTSQU pat LONGARROW term                     
                        { lscope_remove_lst $2 ; 
                          let (id,loc,dots,pats) = $4 in 
                          (id,loc,($2,dots,pats,$6))  }
                ;

bdg:            ID COLON term                                   { lscope_add (fst $1) ; (fst $1,$3) }
                ;

bdgs:           /* empty */                                     { [] }
                | bdg COMMA bdgs                                { $1::$3 }
                | bdg                                           { [$1] }
                ;

pat:            ID dotps spats                                  { (fst $1,snd $1,Array.of_list $2,Array.of_list $3) }
                ;

dotps:          /* empty */                                     { [] }
                | dotps LEFTBRA term RIGHTBRA                   { $3::$1 }
                ;

spats:          /* empty */                                     { [] }
                | spat spats                                    { $1::$2 }
                ;

spat:           ID                                              { mk_pat_var $1 }
                | QID                                           { Pat (filter_qid $1,[||],[||]) }
                | LEFTPAR ID  dotps spats RIGHTPAR              { Pat (fst $2,Array.of_list $3,Array.of_list $4) }           
                | LEFTPAR QID dotps spats RIGHTPAR              { Pat (filter_qid $2,Array.of_list $3,Array.of_list $4) }           
                ;

sterm           : QID                                           { mk_evar $1 }
                | ID                                            { mk_var  $1 }
                | LEFTPAR term RIGHTPAR                         { $2 }
                | TYPE                                          { Type }
                ;

app:            sterm                                           { $1 }
                | app sterm                                     { App ($1,$2) }
                ;

lam_decl:       ID                                              { lscope_add (fst $1) ; (fst $1,None) }
                | ID COLON app                                  { lscope_add (fst $1) ; (fst $1,Some $3) }
                ;

pi_decl:        ID COLON app                                    { lscope_add (fst $1) ; (fst $1,$3) }
                ;

term:           app                                             { $1 }
                | pi_decl ARROW term                            { lscope_remove (fst $1) ; Pi  (Some (fst $1), snd $1, $3) }
                | term ARROW term                               { Pi  (None   , $1, $3) }
                | lam_decl FATARROW term                        { lscope_remove (fst $1) ; Lam (fst $1, snd $1   , $3) }
                ;
%%

