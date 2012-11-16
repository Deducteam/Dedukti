%{
open Types
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
                | top decl DOT                                  
                        { 
                        let gname = !Global.name^"."^(fst (fst $2)) in    (*FIXME*)
                          Global.gscope_add (fst $2) ; 
                          if !Global.check then
                            begin
                              LuaCodeGeneration2.generate_decl_check gname (snd $2) ;
                              LuaCodeGeneration2.generate_decl_code  gname ;
                              LuaCodeGeneration2.generate_decl_term  gname (snd $2)
                            end
                          else
                            begin
                              LuaCodeGeneration2.generate_decl_code gname ;
                              LuaCodeGeneration2.generate_decl_term  gname (snd $2)
                            end
                        }
                | top rules DOT                                 
                        { let (_,(id,rules)) = $2       in
                          let rs = Array.of_list rules  in
                          Global.chk_rules_id $2  ; 
                          if !Global.check then
                            begin
                              Array.iteri (LuaCodeGeneration2.generate_rule_check id) rs ;
                              LuaCodeGeneration2.generate_rules_code id rs ;
                            end
                          else
                            LuaCodeGeneration2.generate_rules_code id rs
                        } ;

rules:          rule                                            { let (id,loc,ru) = $1 in (loc,(id,[ru])) }
                | rule rules                             
                        { let (id1,l1,ru) = $1 and (l2,(id2,lst)) = $2 in 
                          if id1<>id2 then raise (Error (ConstructorMismatch (id1,l1,id2,l2))) 
                          else (l1,(id1,ru::lst)) }
                ;

decl:           ID COLON term                                   { ($1 ,$3) } 
                ;

rule:            LEFTSQU bdgs RIGHTSQU pat LONGARROW term                     
                        { Global.lscope_remove_lst $2 ; 
                          let (id,loc,dots,pats) = $4 in 
                          (id,loc,($2,dots,pats,$6))  }
                ;

bdg:            ID COLON term                                   { Global.lscope_add (fst $1) ; (fst $1,$3) }
                ;

bdgs:           /* empty */                                     { [] }
                | bdg COMMA bdgs                                { $1::$3 }
                | bdg                                           { [$1] }
                ;

pat:            ID dotps spats                                  { (fst $1,snd $1,Array.of_list $2,Array.of_list $3) }
                ;

dotps:          /* empty */                                     { [] }
                | LEFTBRA term RIGHTBRA dotps                  { $2::$4 }
                ;

spats:          /* empty */                                     { [] }
                | spat spats                                    { $1::$2 }
                ;

spat:           ID                                              { Global.mk_pat_var $1 }
                | QID                                           { Pat (Global.filter_qid $1,[||],[||]) }
                | LEFTPAR ID  dotps spats RIGHTPAR              { Pat (fst $2,Array.of_list $3,Array.of_list $4) }           
                | LEFTPAR QID dotps spats RIGHTPAR              { Pat (Global.filter_qid $2,Array.of_list $3,Array.of_list $4) }           
                ;

sterm           : QID                                           { Global.mk_evar $1 }
                | ID                                            { Global.mk_var  $1 }
                | LEFTPAR term RIGHTPAR                         { $2 }
                | TYPE                                          { Type }
                ;

app:            sterm                                           { $1 }
                | app sterm                                     { App ($1,$2) }
                ;

lam_decl:       ID                                              { Global.lscope_add (fst $1) ; (fst $1,None) }
                | ID COLON app                                  { Global.lscope_add (fst $1) ; (fst $1,Some $3) }
                ;

pi_decl:        ID COLON app                                    { Global.lscope_add (fst $1) ; (fst $1,$3) }
                ;

term:           app                                             { $1 }
                | pi_decl ARROW term                            { Global.lscope_remove (fst $1) ; Pi  (Some (fst $1), snd $1, $3) }
                | term ARROW term                               { Pi  (None   , $1, $3) }
                | lam_decl FATARROW term                        { Global.lscope_remove (fst $1) ; Lam (fst $1, snd $1   , $3) }
                ;
%%

