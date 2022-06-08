%{
open Kernel
open Basic
open Preterm
open Scoping
open Reduction
open Signature
open Entry

let rec mk_lam : preterm -> (loc * ident * preterm) list -> preterm = fun te ps ->
  match ps with
  | []           -> te
  | (l,x,ty)::ps -> PreLam(l, x, Some ty, mk_lam te ps)

let rec mk_pi  : preterm -> (loc * ident * preterm) list -> preterm = fun ty ps ->
  match ps with
  | []           -> ty
  | (l,x,aa)::ps -> PrePi(l, Some x, aa, mk_pi ty ps)

let mk_config loc lid =
  let strat    = ref None in
  let target   = ref None in
  let nb_steps = ref None in
  let proc = function
    | "SNF"  when !target   = None -> target   := Some Snf
    | "WHNF" when !target   = None -> target   := Some Whnf
    | "CBN"  when !strat    = None -> strat    := Some ByName
    | "CBV"  when !strat    = None -> strat    := Some ByValue
    | "CBSV" when !strat    = None -> strat    := Some ByStrongValue
    | i      when !nb_steps = None -> nb_steps := Some (int_of_string i)
    | _ -> raise Exit in
  try
    List.iter proc lid;
    { default_cfg with
      nb_steps = (!nb_steps);
      target   = (match !target with None -> default_cfg.target | Some t -> t);
      strat    = (match !strat  with None -> default_cfg.strat  | Some s -> s) }
  with _ -> raise @@ Scoping_error(loc, "invalid command configuration")

let loc_of_rs = function
  | [] -> assert false
  | (l,_,_,_,_,_,_) :: _ -> l
%}

%token EOF
%token DOT
%token COMMA
%token COLON
%token EQUAL
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
%token VDASH
%token QUESTION
%token <Kernel.Basic.loc> PRAGMA_EVAL
%token <Kernel.Basic.loc> PRAGMA_INFER
%token <Kernel.Basic.loc> PRAGMA_CHECK
%token <Kernel.Basic.loc> PRAGMA_CHECKNOT
%token <Kernel.Basic.loc> PRAGMA_ASSERT
%token <Kernel.Basic.loc> PRAGMA_ASSERTNOT
%token <Kernel.Basic.loc> PRAGMA_PRINT
%token <Kernel.Basic.loc> PRAGMA_GDT
%token <Kernel.Basic.loc> PRAGMA_END
%token <Kernel.Basic.loc> ASSERT
%token <Kernel.Basic.loc> UNDERSCORE
%token <Kernel.Basic.loc*Basic.mident> NAME
%token <Kernel.Basic.loc*Basic.mident> REQUIRE
%token <Kernel.Basic.loc> TYPE
%token <Kernel.Basic.loc> KW_DEF
%token <Kernel.Basic.loc> KW_DEFAC
%token <Kernel.Basic.loc> KW_DEFACU
%token <Kernel.Basic.loc> KW_THM
%token <Kernel.Basic.loc> KW_INJ
%token <Kernel.Basic.loc> KW_PRV
%token <Kernel.Basic.loc*Kernel.Basic.ident> ID
%token <Kernel.Basic.loc*Kernel.Basic.mident*Basic.ident> QID
%token <string> STRING

%start line
%type <Kernel.Basic.mident -> Entry.entry> line
%type <Preterm.prule> rule
%type <Preterm.pdecl> decl
%type <Kernel.Basic.loc*Kernel.Basic.ident*Preterm.preterm> param
%type <Preterm.pdecl list> context
%type <Kernel.Basic.loc*Kernel.Basic.mident option*Kernel.Basic.ident*Preterm.prepattern list> top_pattern
%type <Preterm.prepattern> pattern
%type <Preterm.prepattern> pattern_wp
%type <Preterm.preterm> sterm
%type <Preterm.preterm> term

%%

line:
  | id=ID ps=param* COLON ty=term DOT
    {fun md -> Decl(fst id, snd id, Public, Static, scope_term md [] (mk_pi ty ps))}
  | KW_PRV id=ID ps=param* COLON ty=term DOT
    {fun md -> Decl(fst id, snd id, Private, Static, scope_term md [] (mk_pi ty ps))}
  | KW_DEF id=ID COLON ty=term DOT
    {fun md -> Decl(fst id, snd id, Public, Definable Term.Free, scope_term md [] ty)}
  | KW_PRV KW_DEF id=ID COLON ty=term DOT
    {fun md -> Decl(fst id, snd id, Private, Definable Term.Free, scope_term md [] ty)}
  | KW_INJ id=ID COLON ty=term DOT
    {fun md -> Decl(fst id, snd id, Public, Injective, scope_term md [] ty)}
  | KW_PRV KW_INJ id=ID COLON ty=term DOT
    {fun md -> Decl(fst id, snd id, Private, Injective, scope_term md [] ty)}
  | KW_DEFAC id=ID LEFTSQU ty=term RIGHTSQU DOT
    {fun md -> Decl(fst id, snd id, Public, Definable Term.AC, scope_term md [] ty)}
  | KW_PRV KW_DEFAC id=ID LEFTSQU ty=term RIGHTSQU DOT
    {fun md -> Decl(fst id, snd id, Private, Definable Term.AC, scope_term md [] ty)}
  | KW_DEFACU id=ID LEFTSQU ty=term COMMA neu=term RIGHTSQU DOT
    {fun md -> Decl(fst id, snd id, Public, Definable(Term.ACU(scope_term md [] neu)),
	            scope_term md [] ty)}
  | KW_PRV KW_DEFACU id=ID LEFTSQU ty=term COMMA neu=term RIGHTSQU DOT
    {fun md -> Decl(fst id, snd id, Private, Definable(Term.ACU(scope_term md [] neu)),
	            scope_term md [] ty)}
  | KW_DEF id=ID COLON ty=term DEF te=term DOT
    {fun md -> Def(fst id, snd id, Public, false, Some(scope_term md [] ty), scope_term md [] te)}
  | KW_DEF id=ID DEF te=term DOT
    {fun md -> Def(fst id, snd id, Public, false, None, scope_term md [] te)}
  | KW_DEF id=ID ps=param+ COLON ty=term DEF te=term DOT
    {fun md -> Def(fst id, snd id, Public, false, Some(scope_term md [] (mk_pi ty ps)),
                     scope_term md [] (mk_lam te ps))}
  | KW_DEF id=ID ps=param+ DEF te=term DOT
    {fun md -> Def(fst id, snd id, Public, false, None, scope_term md [] (mk_lam te ps))}
  | KW_PRV KW_DEF id=ID COLON ty=term DEF te=term DOT
      {fun md -> Def(fst id, snd id, Private, false, Some(scope_term md [] ty), scope_term md [] te)}
  | KW_PRV KW_DEF id=ID DEF te=term DOT
      {fun md -> Def(fst id, snd id, Private, false, None, scope_term md [] te)}
  | KW_PRV KW_DEF id=ID ps=param+ COLON ty=term DEF te=term DOT
      {fun md -> Def(fst id, snd id, Private, false, Some(scope_term md [] (mk_pi ty ps)),
                     scope_term md [] (mk_lam te ps))}
  | KW_PRV KW_DEF id=ID ps=param+ DEF te=term DOT
      {fun md -> Def(fst id, snd id, Private, false, None, scope_term md [] (mk_lam te ps))}
  | KW_THM id=ID COLON ty=term DEF te=term DOT
      {fun md -> Def(fst id, snd id, Public, true, Some(scope_term md [] ty), scope_term md [] te)}
  | KW_THM id=ID ps=param+ COLON ty=term DEF te=term DOT
      {fun md -> Def(fst id, snd id, Public, true, Some(scope_term md [] (mk_pi ty ps)),
                     scope_term md [] (mk_lam te ps))}
  | rs=rule+ DOT
      {fun md -> Rules(loc_of_rs rs,(List.map (scope_rule md) rs))}

  | ASSERT QUESTION te=term DOT
      {fun md -> Check($1, true, false, Typeable(scope_term md [] te))}
  | ASSERT VDASH t1=aterm EQUAL t2=term DOT
      {fun md -> Check($1, true , false, Convert(scope_term md [] t1, scope_term md [] t2))}
  | ASSERT VDASH te=aterm COLON ty=term DOT
      {fun md -> Check($1, true, false, HasType(scope_term md [] te, scope_term md [] ty))}

  | PRAGMA_EVAL te=term PRAGMA_END
      {fun md -> Eval($1, default_cfg, scope_term md [] te)}
  | PRAGMA_EVAL cfg=eval_config te=term PRAGMA_END
      {fun md -> Eval($1, cfg, scope_term md [] te)}
  | PRAGMA_INFER te=term PRAGMA_END
      {fun md -> Infer($1, default_cfg, scope_term md [] te)}
  | PRAGMA_INFER cfg=eval_config te=term PRAGMA_END
      {fun md -> Infer($1, cfg, scope_term md [] te)}

  | PRAGMA_CHECK te=aterm COLON ty=term PRAGMA_END
      {fun md -> Check($1, false, false, HasType(scope_term md [] te, scope_term md [] ty))}
  | PRAGMA_CHECKNOT te=aterm COLON ty=term PRAGMA_END
      {fun md -> Check($1, false, true , HasType(scope_term md [] te, scope_term md [] ty))}
  | PRAGMA_ASSERT te=aterm COLON ty=term PRAGMA_END
      {fun md -> Check($1, true , false, HasType(scope_term md [] te, scope_term md [] ty))}
  | PRAGMA_ASSERTNOT te=aterm COLON ty=term PRAGMA_END
      {fun md -> Check($1, true , true , HasType(scope_term md [] te, scope_term md [] ty))}

  | PRAGMA_CHECK t1=aterm EQUAL t2=term PRAGMA_END
      {fun md -> Check($1, false, false, Convert(scope_term md [] t1, scope_term md [] t2))}
  | PRAGMA_CHECKNOT t1=aterm EQUAL t2=term PRAGMA_END
      {fun md -> Check($1, false, true , Convert(scope_term md [] t1, scope_term md [] t2))}
  | PRAGMA_ASSERT t1=aterm EQUAL t2=term PRAGMA_END
      {fun md -> Check($1, true , false, Convert(scope_term md [] t1, scope_term md [] t2))}
  | PRAGMA_ASSERTNOT t1=aterm EQUAL t2=term PRAGMA_END
      {fun md -> Check($1, true , true , Convert(scope_term md [] t1, scope_term md [] t2))}

  | PRAGMA_PRINT STRING PRAGMA_END
    {fun _ -> Print($1, $2)}
  | PRAGMA_GDT ID PRAGMA_END
    {fun _ -> DTree($1, None, snd $2)}
  | PRAGMA_GDT QID PRAGMA_END
    {fun _ -> let (_,m,v) = $2 in DTree($1, Some m, v)}

  | n=NAME       DOT {fun _ -> Name(fst n, snd n)}
  | r=REQUIRE    DOT {fun _ -> Require(fst r,snd r)}
  | EOF              {raise End_of_file}

eval_config:
  | LEFTSQU l=separated_nonempty_list(COMMA, ID) RIGHTSQU
  {mk_config (Lexer.loc_of_pos $startpos) (List.map (fun x -> string_of_ident (snd x)) l) }

param:
  | LEFTPAR id=ID COLON te=term RIGHTPAR
      {(fst id, snd id, te)}

rule:
  | LEFTSQU context RIGHTSQU top_pattern LONGARROW term
      { let (l,md_opt,id,args) = $4 in
        ( l , None, $2 , md_opt, id , args , $6) }
  | LEFTBRA ID RIGHTBRA LEFTSQU context RIGHTSQU top_pattern LONGARROW term
      { let (l,md_opt,id,args) = $7 in
        ( l , Some (None,snd $2), $5 , md_opt, id , args , $9)}
  | LEFTBRA QID RIGHTBRA LEFTSQU context RIGHTSQU top_pattern LONGARROW term
      { let (l,md_opt,id,args) = $7 in
        let (_,m,v) = $2 in
        ( l , Some (Some m,v), $5 , md_opt, id , args , $9)}

decl:
  | ID COLON term { ($1, Some $3) }
  | ID            { ($1, None   ) }

context: separated_list(COMMA, decl) { $1 }

top_pattern:
  | ID  pattern_wp* { (fst $1,None,snd $1,$2) }
  | QID pattern_wp* { let (l,md,id)=$1 in (l,Some md,id,$2) }

%inline pid:
  | UNDERSCORE { ($1, mk_ident "_") }
  | ID { $1 }

pattern_wp:
  | ID                       { PPattern (fst $1,None,snd $1,[]) }
  | QID                      { let (l,md,id)=$1 in PPattern (l,Some md,id,[]) }
  | UNDERSCORE               { PJoker ($1,[]) }
  | LEFTBRA term RIGHTBRA    { PCondition $2 }
  | LEFTPAR pattern RIGHTPAR { $2 }

pattern:
  | pid FATARROW pattern     { PLambda (fst $1,snd $1,$3) }
  | pattern_wp+              { PApp($1) }

sterm:
  | QID                      { let (l,md,id)=$1 in PreQId(l,mk_name md id) }
  | ID                       { PreId (fst $1,snd $1) }
  | LEFTPAR term RIGHTPAR    { $2 }
  | TYPE                     { PreType $1 }

aterm:
  | te=sterm ts=sterm*
      {match ts with [] -> te | a::args -> PreApp(te,a,args)}

term:
  | t=aterm
      { t }
  | pid COLON aterm ARROW term
      { PrePi (fst $1,Some (snd $1), $3, $5) }
  | LEFTPAR ID COLON aterm RIGHTPAR ARROW term
      { PrePi (fst $2,Some (snd $2), $4 ,$7) }
  | aterm ARROW term
      { PrePi (Lexer.loc_of_pos $startpos,None,$1,$3) }
  | pid FATARROW term
      {PreLam (fst $1, snd $1, None, $3)}
  | pid COLON aterm FATARROW term
      {PreLam (fst $1, snd $1, Some $3, $5)}
  | LEFTPAR pid COLON aterm DEF aterm RIGHTPAR FATARROW term
      { PreApp (PreLam (fst $2, snd $2, Some $4, $9), $6, []) }
%%
