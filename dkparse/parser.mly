%{

open Types

let mk_declaration id ty =
        let gname = !Global.name^"."^id in
        Global.debug ("{Declaration} " ^ gname ^ "\t\t")  ;
        match !Global.state with
        | None          ->
            begin
              if !Global.do_not_check then () else LuaCodeGeneration2.generate_decl_check gname ty ;
              LuaCodeGeneration2.generate_decl_code gname ;
              LuaCodeGeneration2.generate_decl_term gname ty ;
              Global.debug_ok ()
            end
        | Some ls       ->
            begin
              LuaTypeChecker.check_type_type ls ty ;
              LuaTypeChecker.add_in_context ls gname ty ;
              Global.debug_ok ()
            end

let mk_declaration0 id ty =
        if !Global.ignore_redefinition then
                if Global.gscope_add_decl id then mk_declaration (fst id) ty
                else Global.debug ("{Declaration} " ^ (!Global.name) ^ "." ^ (fst id) ^ "\t\t[IGNORED]\n")
        else (
                Global.gscope_add id ;
                mk_declaration (fst id) ty
        )

let mk_definition id te ty =
        let gname = !Global.name^"."^id in
        Global.debug ("{Definition} " ^ gname ^ "\t\t") ;
        match !Global.state with
        | None         ->
            begin
              if !Global.do_not_check then () else LuaCodeGeneration2.generate_def_check gname te ty ;
              LuaCodeGeneration2.generate_def_code gname te ;
              LuaCodeGeneration2.generate_def_term gname te ;
              Global.debug_ok ()
            end
        | Some ls       ->
            begin
              LuaTypeChecker.check_type_type ls ty ;
              LuaTypeChecker.check_type ls te ty ;
              LuaTypeChecker.add_definition ls gname te ;
              Global.debug_ok ()
            end

let mk_opaque id te ty = 
        let gname = !Global.name^"."^id in
        Global.debug ("{Opaque} " ^ gname ^ "\t\t")  ;
        match !Global.state with
        | None          ->
            begin
              if !Global.do_not_check then () else LuaCodeGeneration2.generate_def_check gname te ty ;
              LuaCodeGeneration2.generate_decl_code gname ;
              LuaCodeGeneration2.generate_decl_term gname ty ;
              Global.debug_ok ()
            end
        | Some ls       -> 
            begin
              LuaTypeChecker.check_type_type ls ty ;
              LuaTypeChecker.check_type ls te ty ;
              LuaTypeChecker.add_in_context ls gname ty ;
              Global.debug_ok ()
            end

let mk_typecheck te ty = 
        Global.debug ("{TypeCheck} ... \t\t") ;
        match !Global.state with
        | None          -> ( 
                if !Global.do_not_check then () else LuaCodeGeneration2.generate_def_check "_" te ty ; 
                Global.debug_ok () )
        | Some ls       -> ( LuaTypeChecker.check_type ls te ty ; Global.debug_ok () )

let mk_rules a = 
  Global.debug ("{RuleCheck} ... \t\t") ;
  let (_,(id,rules)) = a         in
  let rs = Array.of_list rules    in
  Global.chk_rules_id a  ; 
  match !Global.state with
  | None        ->
      begin
        if !Global.do_not_check then () else Array.iteri (LuaCodeGeneration2.generate_rule_check id) rs ;
        LuaCodeGeneration2.generate_rules_code id rs ;
        Global.debug_ok ()
      end
  | Some ls     -> 
      begin
        Array.iter (LuaTypeChecker.check_rule ls id) rs ;
        LuaTypeChecker.add_rules ls id rs ;
        Global.debug_ok ()
      end

let mk_require dep =
        Global.debug ("{Module} "^dep^" \t\t") ;
        match !Global.state with
        | None        -> assert false (*TODO*)
        | Some ls     -> ( LuaTypeChecker.require ls dep ; Global.debug_ok () )

%}

%token DOT
%token COMMA
%token COLON
%token ARROW
%token FATARROW
%token LONGARROW
%token DEF
%token UNDERSCORE
%token HASH
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
top:            /* empty */                                             { () }
                | top ID COLON term DOT                                 { mk_declaration0 $2 $4 }
                | top ID COLON term DEF term DOT                        { Global.gscope_add $2 ; mk_definition (fst $2) $6 $4 }
                | top LEFTBRA ID RIGHTBRA COLON term DEF term DOT       { Global.gscope_add $3 ; mk_opaque (fst $3) $8 $6 }
                | top UNDERSCORE COLON term DEF term DOT                { mk_typecheck $6 $4 }
                | top rules DOT                                         { mk_rules $2 } 
                | top HASH ID                                           { mk_require (fst $3) };

rules:          rule                                                    { let (id,loc,ru) = $1 in (loc,(id,[ru])) }
                | rule rules                             
                        { let (id1,l1,ru) = $1 and (l2,(id2,lst)) = $2 in 
                          if id1<>id2 then raise (ParsingError (ConstructorMismatch (id1,l1,id2,l2))) 
                          else (l1,(id1,ru::lst)) }
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

