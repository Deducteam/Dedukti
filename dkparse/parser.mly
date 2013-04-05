%{

open Types

let mk_declaration id loc ty =
        if !Global.ignore_redeclarations then
                if Global.gscope_add_decl (id,loc) then (
                        Global.debug (Debug.string_of_loc loc ^ "\tGenerating declaration " ^ id ^ "\t\t")  ;
                        CodeGeneration.mk_declaration id loc ty ;
                        Global.debug_ok () )
                else 
                        Global.debug (Debug.string_of_loc loc ^ "\tGenerating declaration " ^ (!Global.name) ^ "." ^ id ^ "\t\t[IGNORED]\n")
        else (
                Global.gscope_add (id,loc) ;
                Global.debug (Debug.string_of_loc loc ^ "\tGenerating declaration " ^ id ^ "\t\t")  ;
                CodeGeneration.mk_declaration id loc ty ;
                Global.debug_ok () ) 

let mk_definition (id,loc) te ty =
        Global.debug (Debug.string_of_loc loc ^ "\tGenerating definition " ^ id ^ "\t\t") ;
        CodeGeneration.mk_definition id loc te ty ;
        Global.debug_ok () 

let mk_opaque (id,loc) te ty = 
        Global.debug (Debug.string_of_loc loc ^ "\tGenerating opaque definition " ^ id ^ "\t\t")  ;
        CodeGeneration.mk_opaque id loc te ty ;
        Global.debug_ok () 

let mk_typecheck loc te ty = 
        Global.debug (Debug.string_of_loc loc ^ "\tGenerating typechecking ... \t\t") ;
        CodeGeneration.mk_typecheck loc te ty ;
        Global.debug_ok () 

let mk_rules (a:loc*rules) = 
        let (loc,(id,rules)) = a         in
        Global.debug (Debug.string_of_loc loc ^ "\tGenerating rule checks for "^ id^" \t\t") ; 
        let rs = Array.of_list rules    in
        Global.chk_rules_id a  ; 
        CodeGeneration.mk_rules id rs ;
        Global.debug_ok () 

let mk_require (dep,loc) =
        Global.libs := dep::(!Global.libs) ;
        Global.debug (Debug.string_of_loc loc ^ "\tGenerating dependency "^dep^" \t\t") ;
        CodeGeneration.mk_require dep ; 
        Global.debug_ok () 

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
%token HASH
%token LEFTPAR
%token RIGHTPAR
%token LEFTBRA
%token RIGHTBRA
%token LEFTSQU
%token RIGHTSQU
%token TYPE
%token <string*Types.loc> ID
%token <string*string*Types.loc> QID

%start top
%type <unit> top
%type <unit> line
%type <Types.loc*Types.rules> rules
%type <string*Types.loc*Types.rule> rule
%type <string*Types.loc*Types.term array*Types.pattern array> pat

%right ARROW FATARROW

%%
top:              EOF                                                   { raise End_of_file }
                | line top                                              { () }

line:             ID COLON term DOT                                     { mk_declaration (fst $1) (snd $1) $3 }
                | ID COLON term DEF term DOT                            { Global.gscope_add $1 ; mk_definition $1 $5 $3 }
                | LEFTBRA ID RIGHTBRA COLON term DEF term DOT           { Global.gscope_add $2 ; mk_opaque $2 $7 $5 }
                | UNDERSCORE COLON term DEF term DOT                    { mk_typecheck $1 $5 $3 }
                | rules DOT                                             { mk_rules $1 } 
                | HASH ID                                               { mk_require $2 } ;

rules:          rule                                                    { let (id,loc,ru) = $1 in (loc,(id,[ru])) }
                | rule rules                             
                        { let (id1,l1,ru) = $1 and (l2,(id2,lst)) = $2 in 
                          if id1<>id2 then raise (ParserError (ConstructorMismatch (id1,l1,id2,l2))) 
                          else (l1,(id1,ru::lst)) }
                ;

rule:            LEFTSQU bdgs RIGHTSQU pat LONGARROW term                     
                        { Global.lscope_remove_lst $2 ; 
                          let (id,loc,dots,pats) = $4 in (id,loc,(loc,$2,dots,pats,$6))  }
                ;

bdg:            ID COLON term                                   { Global.lscope_add (fst $1) ; ($1,$3) }
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
                | LEFTPAR ID  dotps spats RIGHTPAR              { Pat ((!Global.name,fst $2),Array.of_list $3,Array.of_list $4) }           
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

